/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNrrdImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <string>
#include "itkNrrdImageIO.h"
#include "itkMacro.h"
#include "itkMetaDataObject.h"
#include "itkIOCommon.h"

#if defined(__BORLANDC__)
# include <math.h>
# include <float.h> // for _control87()
#endif // defined(__BORLANDC__)

namespace itk {
 
void NrrdImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
 
bool NrrdImageIO::CanReadFile( const char* filename ) 
{
#if defined(__BORLANDC__)
  // Disable floating point exceptions in Borland
  _control87(MCW_EM, MCW_EM);
#endif // defined(__BORLANDC__)
   
  // Check the extension first to avoid opening files that do not
  // look like nrrds.  The file must have an appropriate extension to be
  // recognized.
  std::string fname = filename;
  if(  fname == "" )
    {
    itkDebugMacro(<<"No filename specified.");
    return false;
    }

  bool extensionFound = false;
  std::string::size_type nrrdPos = fname.rfind(".nrrd");
  if ((nrrdPos != std::string::npos)
      && (nrrdPos == fname.length() - 5))
    {
    extensionFound = true;
    }

  std::string::size_type nhdrPos = fname.rfind(".nhdr");
  if ((nhdrPos != std::string::npos)
      && (nhdrPos == fname.length() - 5))
    {
    extensionFound = true;
    }

  if( !extensionFound )
    {
    itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  // We have the correct extension, so now check for the Nrrd magic.
  std::ifstream inputStream;

  inputStream.open( filename, std::ios::in | std::ios::binary );

  if( inputStream.fail() )
    {
    return false;
    }

  char key[8000];

  inputStream >> key;

  if( inputStream.eof() )
    {
    inputStream.close();
    return false;
    }

  // Ignores the version number of the NRRD file.  Version is checked when the
  // file is read. For now we only want to report whether this is or is not a
  // NRRD file.
  if( strncmp(key,"NRRD", 4)==0 ) 
    {
    inputStream.close();
    return true;
    }

  inputStream.close();
  return false;
}

void NrrdImageIO::ReadImageInformation()
 {
   // This method determines the following and sets the appropriate value in
   // the parent IO class:
   //
   // binary/ascii file type 
   // endianness
   // pixel type
   // pixel component type
   // number of pixel components
   // number of image dimensions
   // image spacing
   // image origin
   // meta data dictionary information

   int i;
   char *key = NULL;
   char *val = NULL;
   char *err;
   NrrdIoState *nio;
   Nrrd *nrrd;

   nio = nrrdIoStateNew();
   nrrdIoStateSet(nio, nrrdIoStateSkipData, 1);

   nrrd = nrrdNew();
   if (nrrdLoad(nrrd, this->GetFileName(), nio) != 0)
     {
     err = biffGetDone("nrrd");
     itkExceptionMacro("Error reading " << this->GetFileName() << ": " << err);
     free(err); // err points to malloc'd data!!
     //     err = NULL;
     }

   if ( nio->endian == airEndianLittle )
     {
     this->SetByteOrderToLittleEndian();
     }
   else if (nio->endian == airEndianBig )
     {
     this->SetByteOrderToBigEndian();
     }
   else
     {
     this->SetByteOrder( ImageIOBase::OrderNotApplicable );
     }

   if ( nio->encoding == nrrdEncodingAscii )
     {
     this->SetFileTypeToASCII();
     }
   else
     {
     this->SetFileTypeToBinary();
     }
   
   // NrrdIO only supports scalar data.  Future implementations may support
   // read/write of vector data.
   this->SetNumberOfComponents(1);

   // Set the number of image dimensions
   this->SetNumberOfDimensions(nrrd->dim);

   // Set type information
   //   this->SetPixelType( this->NrrdToITKComponentType(nrrd->type) );
   // For now we only support scalar reads/writes
   this->SetPixelType( SCALAR );
   this->SetComponentType( this->NrrdToITKComponentType(nrrd->type) );

   // Set axis information
   double spacing;
   int sdim;
   double axis[NRRD_SPACE_DIM_MAX];
   for (i=0; i < static_cast<int>(this->GetNumberOfDimensions()); i++)
     {
     this->SetDimensions(i, nrrd->axis[i].size);
     nrrdSpacingCalculate(nrrd, i, &spacing, axis);
     if (AIR_EXISTS(spacing)) // is the spacing NaN?
       {
       this->SetSpacing(i, spacing);
       }
     else
       {
       this->SetSpacing(i, 1.0);
       }
     if ( AIR_EXISTS(nrrd->axis[i].min) ) // is the min NaN?
       {
       this->SetOrigin(i, nrrd->axis[i].min);
       }
     else // If min has not been set, assume a default.
       {  // An ITK image _must_ have a valid origin.
       this->SetOrigin(i, 0.0);
       }
     }

   // Push extra key/value pair data into an itkDataDictionary
   MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
   std::string classname(this->GetNameOfClass());
   EncapsulateMetaData<std::string>(thisDic, ITK_InputFilterName, classname);
   // EncapsulateMetaData<std::string>(thisDic, ITK_OnDiskStorageTypeName,  std::string());
   // itk::EncapsulateMetaData<std::string>(thisDic,ITK_ImageFileBaseName,std::string(this->m_hdr.hk.db_name,18));

   for (i=0; i < nrrdKeyValueSize(nrrd); i++)
     {
     nrrdKeyValueIndex(nrrd, &key, &val, i);
     EncapsulateMetaData<std::string>(thisDic, std::string(key), std::string(val));
     free(key);  // key and val point to malloc'd data!!
     free(val);
     key = val = NULL;
     }
   
   nrrdNix(nrrd);
   nrrdIoStateNix(nio);
} 


void NrrdImageIO::Read(void* buffer)
{
#if defined(__BORLANDC__)
  // Disable floating point exceptions in Borland
  _control87(MCW_EM, MCW_EM);
#endif // defined(__BORLANDC__)
  
  Nrrd *nrrd;

  // Allocate a nrrd and a nrrd io state.
  nrrd = nrrdNew();

  // The data buffer has already been allocated.  Hand this off to the nrrd.
  nrrd->data = buffer;

  // Now to prevent the Nrrd IO from trying to delete the buffer (illegally
  // with free) when it reads the file, set the size, type, and dimensionality.
  nrrd->dim = static_cast<int>(this->GetNumberOfDimensions());
  for (unsigned int i = 0; i < this->GetNumberOfDimensions(); i++)
    {
    nrrd->axis[i].size = static_cast<int>(this->GetDimensions(i));
    }

  // Set data type information
  nrrd->type = this->ITKToNrrdPixelType( this->m_ComponentType );
  
  // Load using the nrrdLoad call.
  if ( nrrdLoad(nrrd, this->GetFileName(), NULL) != 0 )
    {
    char *err =  biffGetDone("nrrd");
    itkExceptionMacro("Could not read " << this->GetFileName() << std::endl << "The error returned was " << err << std::endl );
    free(err); // err points to malloc'd data!
    }

  // Free the nrrd struct but do not delete nrrd.data
  nrrdNix(nrrd);
} 


ImageIOBase::IOComponentType
NrrdImageIO::
NrrdToITKComponentType( const int nrrdPixelType ) const
{
#if defined(__BORLANDC__)
   // Disable floating point exceptions in Borland
   _control87(MCW_EM, MCW_EM);
#endif // defined(__BORLANDC__)
  switch( nrrdPixelType )
    {
    default:
    case nrrdTypeDefault:
      return UNKNOWNCOMPONENTTYPE;
      break;
    case nrrdTypeChar:
      return CHAR;
      break;
    case nrrdTypeUChar:
      return UCHAR;
      break;
    case nrrdTypeShort:
      return SHORT;
      break;
    case nrrdTypeUShort:
      return USHORT;
      break;
      //    case nrrdTypeLLong:
      //      return LONG ;
      //      break;
      //    case nrrdTypeULong:
      //      return ULONG;
      //      break;
    case nrrdTypeInt:
      return INT;
      break;
    case nrrdTypeUInt:
      return UINT;
      break;
    case nrrdTypeFloat:
      return FLOAT;
      break;
    case nrrdTypeDouble:
      return DOUBLE;
      break;
    case nrrdTypeBlock:
      return UNKNOWNCOMPONENTTYPE;
      break;
    }
}

int
NrrdImageIO::
ITKToNrrdPixelType( const ImageIOBase::IOComponentType itkComponentType ) const
{
#if defined(__BORLANDC__)
  // Disable floating point exceptions in Borland
  _control87(MCW_EM, MCW_EM);
#endif // defined(__BORLANDC__)

  switch( itkComponentType )
    {
    default:
    case UNKNOWNCOMPONENTTYPE:
      return nrrdTypeUnknown;
      break;
    case CHAR:
      return nrrdTypeChar;
      break;
    case UCHAR:
      return nrrdTypeUChar;
      break;
    case SHORT:
      return nrrdTypeShort;
      break;
    case USHORT:
      return nrrdTypeUShort;
      break;
      //    case LONG:
      //      return nrrdTypeLLong;
      //      break;
      //    case ULONG:
      //      return nrrdTypeULong;
      //      break;
    case INT:
      return nrrdTypeInt;
      break;
    case UINT:
      return nrrdTypeUInt;
      break;
    case FLOAT:
      return nrrdTypeFloat;
      break;
    case DOUBLE:
      return nrrdTypeDouble;
      break;
    }  
}

bool NrrdImageIO::CanWriteFile( const char * name )
 {
#if defined(__BORLANDC__)
   // Disable floating point exceptions in Borland
   _control87(MCW_EM, MCW_EM);
#endif // defined(__BORLANDC__)
  std::string filename = name;
  if(  filename == "" )
    {
    return false;
    }

  std::string::size_type nrrdPos = filename.rfind(".nrrd");
  if ((nrrdPos != std::string::npos)
      && (nrrdPos == filename.length() - 5))
    {
    return true;
    }

  std::string::size_type nhdrPos = filename.rfind(".nhdr");
  if ((nhdrPos != std::string::npos)
      && (nhdrPos == filename.length() - 5))
    {
    return true;
    }

  return false;
}

  
void 
NrrdImageIO
::WriteImageInformation(void)
{
  // Nothing needs doing here.
}


void
NrrdImageIO
::Write( const void* buffer) 
{
#if defined(__BORLANDC__)
   // Disable floating point exceptions in Borland
   _control87(MCW_EM, MCW_EM);
#endif // defined(__BORLANDC__)
   
  Nrrd *nrrd = nrrdNew();
  NrrdIoState *nio = nrrdIoStateNew();
  
  // Set the various struct fields.  The const cast on buffer is unavoidable.
  nrrd->data = const_cast<void *>(buffer);
  nrrd->dim  = static_cast<int>(this->GetNumberOfDimensions());
  //nrrd->content = something from the data dictionary
  //nrrd->blockSize = can we support block data anyway?
  //nrrd oldMin, oldMax = something from the data dictionary
  //nrrd ptr = irrelevant

  // Are we writing ASCII or Binary data?
  Superclass::FileType  fileType = this->GetFileType();
  switch ( fileType )
    {
    default:
    case TypeNotApplicable:
    case Binary:
      nio->encoding = nrrdEncodingRaw;
      break;
    case ASCII:
      nio->encoding = nrrdEncodingAscii;
      break;
    }

  // Should we use compression?  Note that enabling compression always produces
  // binary output.
  if (this->GetUseCompression() == true)
    {
    // enable default nrrd compression flags
    nio->encoding = nrrdEncodingGzip;
    }

  // Which byte order should we use?
  Superclass::ByteOrder byteOrder = this->GetByteOrder();
  switch (byteOrder)
    {
    default:
    case OrderNotApplicable:
      nio->endian = airEndianUnknown;
      break;
    case BigEndian:
      nio->endian = airEndianBig;
      break;
    case LittleEndian:
      nio->endian = airEndianLittle;
      break;
    }
  
  // Is this vector data?  If so then raise an exception.
  //
  // TO DO: Support vector data writing as N+1 dimensional data.  This will
  // involve copying the buffer, extracting vector components into raster
  // scalar format.
  if (this->GetNumberOfComponents() > 1)
    {
    itkExceptionMacro("NrrdIO does not currently support writing multiple-component data.");
    }

  // Set the pixel type;
  nrrd->type = this->ITKToNrrdPixelType( m_ComponentType );

  // Set axis information
  for (int i = 0; i <nrrd->dim; i++)
    {
    nrrd->axis[i].size    = this->GetDimensions(i);
    nrrd->axis[i].spacing = this->GetSpacing(i);
    nrrd->axis[i].min     = static_cast<double>(this->GetOrigin(i));
    nrrd->axis[i].max     = nrrd->axis[i].min + (nrrd->axis[i].size * nrrd->axis[i].spacing);
    //nrrd->label =  something from the data dictionary
    //nrrd->unit  =  something from the data dictionary
    }

  // Populate key/value pairs with anything hanging around in the data
  // dictionary.
  MetaDataDictionary &thisDic = this->GetMetaDataDictionary();
  std::vector<std::string> keys = thisDic.GetKeys();
  for( std::vector<std::string>::const_iterator it = keys.begin();
      it != keys.end(); it++ )
    {
    std::string temp;
    if (ExposeMetaData<std::string>(thisDic, *it, temp))
      {
      nrrdKeyValueAdd(nrrd, (*it).c_str(), temp.c_str());
      }
    }

  // Write the nrrd to file.
  if (nrrdSave(this->GetFileName(), nrrd, NULL) != 0)
    {
    char *err = biffGetDone("nrrd");
    itkExceptionMacro("Error saving file " << this->GetFileName() << std::endl << "The error returned was " << err << std::endl);
    free(err);  // err points to malloc'd data!!
    return;
    }
  
  // Release the nrrd struct memory.  Do not free nrrd.data.
  nrrdNix(nrrd);
  nrrdIoStateNix(nio);

}
 
} // end namespace itk
