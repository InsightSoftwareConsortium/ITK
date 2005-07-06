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


namespace itk {

#define KEY_PREFIX "NRRD_"
 
bool NrrdImageIO::SupportsDimension(unsigned long dim)
{
  // HEY: this has to use < NRRD_DIM_MAX in case of non-SCALAR type
  return dim <= NRRD_DIM_MAX;
}

void NrrdImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
 
ImageIOBase::IOComponentType
NrrdImageIO::
NrrdToITKComponentType( const int nrrdComponentType ) const
{
  switch( nrrdComponentType )
    {
    default:
    case nrrdTypeUnknown:
    case nrrdTypeBlock:
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
      // these types are 64-bit; "long" is only 64-bit on 64-bit machines
      //    case nrrdTypeLLong:
      //      return LONG ;
      //      break;
      //    case nrrdTypeULLong:
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
    }
}

int
NrrdImageIO::
ITKToNrrdComponentType( const ImageIOBase::IOComponentType itkComponentType ) const
{
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
      // these types are 64-bit; "long" is only 64-bit on 64-bit machines
      //    case LONG:
      //      return nrrdTypeLLong;
      //      break;
      //    case ULONG:
      //      return nrrdTypeULLong;
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

bool NrrdImageIO::CanReadFile( const char* filename ) 
{
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

  // We have the correct extension, so now check for the Nrrd magic "NRRD",
  // while ignoring the format version (the next four characters)
  std::ifstream inputStream;

  inputStream.open( filename, std::ios::in | std::ios::binary );

  if( inputStream.fail() )
    {
    return false;
    }

  char magic[5] = {'\0','\0','\0','\0','\0'};
  inputStream.read(magic,4*sizeof(char));

  if( inputStream.eof() )
    {
    inputStream.close();
    return false;
    }

  if( strcmp(magic,"NRRD")==0 ) 
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

   int axii;  // axis index index
   int axi;   // axis index
   Nrrd *nrrd = nrrdNew();
   NrrdIoState *nio = nrrdIoStateNew();

   // this is the mechanism by which we tell nrrdLoad to read
   // just the header, and none of the data
   nrrdIoStateSet(nio, nrrdIoStateSkipData, 1);
   if (nrrdLoad(nrrd, this->GetFileName(), nio) != 0)
     {
     char *err = biffGetDone(NRRD);  // would be nice to free(err)
     itkExceptionMacro("ReadImageInformation: Error reading " 
                       << this->GetFileName() << ": " << err);
     }

   if (nrrdTypeBlock == nrrd->type)
     {
     itkExceptionMacro("ReadImageInformation: Cannot currently "
                       "handle nrrdTypeBlock");
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
   this->SetNumberOfComponents(1); // HEY fix this
   this->SetPixelType( SCALAR );  // HEY fix this

   // Set the number of image dimensions and bail if needed
   int indAxisNum, indAxisIdx[NRRD_DIM_MAX];
   indAxisNum = nrrdDomainAxesGet(nrrd, indAxisIdx);
   if (!( indAxisNum == nrrd->dim || indAxisNum == nrrd->dim-1 ))
     {
     itkExceptionMacro("ReadImageInformation: nrrd has more than one "
                       "dependent axis; not currently handled");
     }
   if (nrrd->spaceDim && nrrd->spaceDim != indAxisNum)
     {
     itkExceptionMacro("ReadImageInformation: nrrd's # independent axes "
                       "doesn't match dimension of space in which "
                       "orientation is defined; not currently handled");
     }
   // else nrrd->spaceDim == indAxisNum, if the nrrd has orientation
   this->SetNumberOfDimensions(indAxisNum);

   // Set type information
   //   this->SetPixelType( this->NrrdToITKComponentType(nrrd->type) );
   // For now we only support scalar reads/writes
   // HEY fix this
   this->SetComponentType( this->NrrdToITKComponentType(nrrd->type) );

   // Set axis information
   double spacing;
   double spaceVec[NRRD_SPACE_DIM_MAX];
   vnl_vector<double> spaceVecVnl(NRRD_SPACE_DIM_MAX);
   int spacingStatus, saxi;
   for (axii=0; axii < indAxisNum; axii++)
     {
     axi = indAxisIdx[axii];
     this->SetDimensions(axi, nrrd->axis[axi].size);
     spacingStatus = nrrdSpacingCalculate(nrrd, axi, &spacing, spaceVec);
     switch(spacingStatus) 
       {
       case nrrdSpacingStatusNone:
         this->SetSpacing(axi, 1.0);
         break;
       case nrrdSpacingStatusScalarNoSpace:
         this->SetSpacing(axi, spacing);
         break;
       case nrrdSpacingStatusDirection:
         this->SetSpacing(axi, spacing);
         for (saxi=0; saxi < nrrd->spaceDim; saxi++)
           {
           spaceVecVnl[saxi] = spaceVec[saxi];
           }
         this->SetDirection(axi, spaceVecVnl);
         break;
       default:
       case nrrdSpacingStatusUnknown:
         itkExceptionMacro("ReadImageInformation: Error interpreting "
                           "nrrd spacing (nrrdSpacingStatusUnknown)");
         break;
       case nrrdSpacingStatusScalarWithSpace:
         itkExceptionMacro("ReadImageInformation: Error interpreting "
                           "nrrd spacing (nrrdSpacingStatusScalarWithSpace)");
         break;
       }
     }
   
   // Figure out origin
   if (nrrd->spaceDim)
     {
     for (saxi=0; saxi < nrrd->spaceDim; saxi++)
       {
       this->SetOrigin(saxi, (AIR_EXISTS(nrrd->spaceOrigin[saxi])
                              ? nrrd->spaceOrigin[saxi]
                              : 0.0));
       }
     }
   else 
     {
     double spaceOrigin[NRRD_DIM_MAX];
     int originStatus = nrrdOriginCalculate(nrrd, indAxisIdx, indAxisNum,
                                            nrrdCenterCell, spaceOrigin);
     for (saxi=0; saxi < indAxisNum; saxi++) 
       {
       switch (originStatus)
         {
         case nrrdOriginStatusNoMin:
         case nrrdOriginStatusNoMaxOrSpacing:
           this->SetOrigin(saxi, 0.0);
           break;
         case nrrdOriginStatusOkay:
           this->SetOrigin(saxi, spaceOrigin[saxi]);
           break;
         default:
         case nrrdOriginStatusUnknown:
         case nrrdOriginStatusDirection:
           itkExceptionMacro("ReadImageInformation: Error interpreting "
                             "nrrd origin status");
           break;
         }
       }
     }

   // Store key/value pairs in MetaDataDictionary
   char key[AIR_STRLEN_SMALL];
   const char *val;
   char *keyPtr = NULL;
   char *valPtr = NULL;
   MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
   std::string classname(this->GetNameOfClass());
   EncapsulateMetaData<std::string>(thisDic, ITK_InputFilterName, classname);
   int kvpi;
   for (kvpi=0; kvpi < nrrdKeyValueSize(nrrd); kvpi++)
     {
     nrrdKeyValueIndex(nrrd, &keyPtr, &valPtr, kvpi);
     EncapsulateMetaData<std::string>(thisDic, std::string(keyPtr), 
                                      std::string(valPtr));
     keyPtr = (char *)airFree(keyPtr);
     valPtr = (char *)airFree(valPtr);
     }
   
   // save in MetaDataDictionary those important nrrd fields that
   // (currently) have no ITK equivalent
   NrrdAxisInfo *naxis;
   for (axii=0; axii < indAxisNum; axii++)
     {
     axi = indAxisIdx[axii];
     naxis = nrrd->axis + axi;
     if (AIR_EXISTS(naxis->thickness))
       {
       sprintf(key, "%s%s[%d]", KEY_PREFIX,
               airEnumStr(nrrdField, nrrdField_thicknesses), axi);
       EncapsulateMetaData<double>(thisDic, std::string(key),
                                   naxis->thickness);
       }
     if (naxis->center)
       {
       sprintf(key, "%s%s[%d]", KEY_PREFIX, 
               airEnumStr(nrrdField, nrrdField_centers), axi);
       val = airEnumStr(nrrdCenter, naxis->center);
       EncapsulateMetaData<std::string>(thisDic, std::string(key),
                                        std::string(val));
       }
     if (naxis->kind)
       {
       sprintf(key, "%s%s[%d]", KEY_PREFIX, 
               airEnumStr(nrrdField, nrrdField_kinds), axi);
       val = airEnumStr(nrrdKind, naxis->kind);
       EncapsulateMetaData<std::string>(thisDic, std::string(key),
                                        std::string(val));
       }
     if (airStrlen(naxis->label))
       {
       sprintf(key, "%s%s[%d]", KEY_PREFIX, 
               airEnumStr(nrrdField, nrrdField_labels), axi);
       EncapsulateMetaData<std::string>(thisDic, std::string(key),
                                        std::string(naxis->label));
       }
     }
   if (airStrlen(nrrd->content))
     {
     sprintf(key, "%s%s", KEY_PREFIX, 
             airEnumStr(nrrdField, nrrdField_content));
     EncapsulateMetaData<std::string>(thisDic, std::string(key), 
                                      std::string(nrrd->content));
     }
   if (AIR_EXISTS(nrrd->oldMin))
     {
     sprintf(key, "%s%s", KEY_PREFIX,
             airEnumStr(nrrdField, nrrdField_old_min));
     EncapsulateMetaData<double>(thisDic, std::string(key), nrrd->oldMin);
     }
   if (AIR_EXISTS(nrrd->oldMax))
     {
     sprintf(key, "%s%s", KEY_PREFIX,
             airEnumStr(nrrdField, nrrdField_old_max));
     EncapsulateMetaData<double>(thisDic, std::string(key), nrrd->oldMax);
     }
   if (nrrd->space)
     {
     sprintf(key, "%s%s", KEY_PREFIX,
             airEnumStr(nrrdField, nrrdField_space));
     val = airEnumStr(nrrdSpace, nrrd->space);
     EncapsulateMetaData<std::string>(thisDic, std::string(key),
                                      std::string(val));
     }
   if (AIR_EXISTS(nrrd->measurementFrame[0][0]))
     {
     // sprintf(key, "%s%s", KEY_PREFIX,
     //         airEnumStr(nrrdField, nrrdField_measurement_frame));
     // HEY: should the measurement frame be saved as vnl_matrix ??
     }
   
   nrrdNix(nrrd);
   nrrdIoStateNix(nio);
} 


void NrrdImageIO::Read(void* buffer)
{
  Nrrd *nrrd = nrrdNew();

  // The data buffer has already been allocated.  Hand this off to the nrrd,
  // and set just enough info in the nrrd so that it knows the size of 
  // allocated data.  Internal to nrrdLoad, the given buffer will be used
  // instead of allocating new data.
  nrrd->data = buffer;
  nrrd->type = this->ITKToNrrdComponentType( this->m_ComponentType );
  nrrd->dim = static_cast<int>(this->GetNumberOfDimensions());
  for (unsigned int axi = 0; axi < static_cast<unsigned int>(nrrd->dim); axi++)
    {
    nrrd->axis[axi].size = static_cast<int>(this->GetDimensions(axi));
    }

  // Read in the nrrd.  Yes, this means that the header is being read
  // twice: once by NrrdImageIO::ReadImageInformation, and once here
  if ( nrrdLoad(nrrd, this->GetFileName(), NULL) != 0 )
    {
    char *err =  biffGetDone(NRRD); // would be nice to free(err)
    itkExceptionMacro("Read: Error reading " 
                      << this->GetFileName() << ": " << err);
    }

  // Free the nrrd struct but not nrrd->data
  nrrdNix(nrrd);
} 


bool NrrdImageIO::CanWriteFile( const char * name )
 {
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

  
void NrrdImageIO::WriteImageInformation(void)
{
  // Nothing needs doing here.
}


void NrrdImageIO::Write( const void* buffer) 
{
  Nrrd *nrrd = nrrdNew();
  NrrdIoState *nio = nrrdIoStateNew();
  int nrrdDim, baseDim, spaceDim, kind[NRRD_DIM_MAX], size[NRRD_DIM_MAX];
  double origin[NRRD_DIM_MAX];

  if (this->GetNumberOfComponents() > 1)
    {
    // HEY we're assuming that everything is contiguous in memory
    size[0] = this->GetNumberOfComponents();
    // HEY we need better logic to determine the correct kind
    kind[0] = nrrdKindList;
    baseDim = 1;
    }
  else
    {
    baseDim = 0;
    }
  spaceDim = this->GetNumberOfDimensions();
  nrrdDim = baseDim + spaceDim;
  for (int axi=0; axi < spaceDim; axi++)
    {
    size[axi+baseDim] = this->GetDimensions(axi) - baseDim;
    kind[axi+baseDim] = nrrdKindDomain;
    origin[axi] = this->GetOrigin(axi);
    // HEY GetDirection
    }
  if (nrrdWrap_nva(nrrd, const_cast<void *>(buffer),
                   this->ITKToNrrdComponentType( m_ComponentType ),
                   nrrdDim, size)
      || nrrdSpaceDimensionSet(nrrd, spaceDim)
      || nrrdSpaceOriginSet(nrrd, origin))
    {
    char *err = biffGetDone(NRRD); // would be nice to free(err)
    itkExceptionMacro("Write: Error wrapping for " 
                      << this->GetFileName() << ": " << err);
    }

  // Go through MetaDataDictionary and set either specific nrrd field
  // or a key/value pair
#ifndef __BORLANDC__
  MetaDataDictionary &thisDic = this->GetMetaDataDictionary();
  std::vector<std::string> keys = thisDic.GetKeys();
  std::vector<std::string>::const_iterator keyIt;
  std::string value;
  const char *keyField, *field;
  int axi;
  for( keyIt = keys.begin(); keyIt != keys.end(); keyIt++ )
    {
    if (!strncmp(KEY_PREFIX, (*keyIt).c_str(), strlen(KEY_PREFIX)))
      {
      keyField = (*keyIt).c_str() + strlen(KEY_PREFIX);
      // only of one of these can succeed
      field = airEnumStr(nrrdField, nrrdField_thicknesses);
      if (!strncmp(keyField, field, strlen(field)))
        {
        if (1 == sscanf(keyField + strlen(field), "[%d]", &axi)
            && 0 <= axi && axi < nrrd->dim - baseDim)
          {
          ExposeMetaData<double>(thisDic, *keyIt,
                                 nrrd->axis[axi-baseDim].thickness);
          }
        }
      field = airEnumStr(nrrdField, nrrdField_centers);
      if (!strncmp(keyField, field, strlen(field)))
        {
        if (1 == sscanf(keyField + strlen(field), "[%d]", &axi)
            && 0 <= axi && axi < nrrd->dim - baseDim)
          {
          ExposeMetaData<std::string>(thisDic, *keyIt, value);
          nrrd->axis[axi-baseDim].center = airEnumVal(nrrdCenter,
                                                      value.c_str());
          }
        }
      field = airEnumStr(nrrdField, nrrdField_kinds);
      if (!strncmp(keyField, field, strlen(field)))
        {
        if (1 == sscanf(keyField + strlen(field), "[%d]", &axi)
            && 0 <= axi && axi < nrrd->dim - baseDim)
          {
          ExposeMetaData<std::string>(thisDic, *keyIt, value);
          nrrd->axis[axi-baseDim].kind = airEnumVal(nrrdKind,
                                                    value.c_str());
          }
        }
      field = airEnumStr(nrrdField, nrrdField_labels);
      if (!strncmp(keyField, field, strlen(field)))
        {
        if (1 == sscanf(keyField + strlen(field), "[%d]", &axi)
            && 0 <= axi && axi < nrrd->dim - baseDim)
          {
          ExposeMetaData<std::string>(thisDic, *keyIt, value);
          nrrd->axis[axi-baseDim].label = airStrdup(value.c_str());
          }
        }
      field = airEnumStr(nrrdField, nrrdField_old_min);
      if (!strncmp(keyField, field, strlen(field)))
        {
        ExposeMetaData<double>(thisDic, *keyIt, nrrd->oldMin);
        }
      field = airEnumStr(nrrdField, nrrdField_old_max);
      if (!strncmp(keyField, field, strlen(field)))
        {
        ExposeMetaData<double>(thisDic, *keyIt, nrrd->oldMax);
        }
      field = airEnumStr(nrrdField, nrrdField_space);
      if (!strncmp(keyField, field, strlen(field)))
        {
        int space;
        ExposeMetaData<std::string>(thisDic, *keyIt, value);
        space = airEnumVal(nrrdSpace, value.c_str());
        if (nrrdSpaceDimension(space) == nrrd->spaceDim)
          {
          // sanity check
          nrrd->space = space;
          }
        }
      field = airEnumStr(nrrdField, nrrdField_content);
      if (!strncmp(keyField, field, strlen(field)))
        {
        ExposeMetaData<std::string>(thisDic, *keyIt, value);
        nrrd->content = airStrdup(value.c_str());
        }
      // HEY have to grab measurement frame if its been defined
      }
    else
      {
      // not a NRRD field packed into meta data; just a regular key/value
      ExposeMetaData<std::string>(thisDic, *keyIt, value);
      nrrdKeyValueAdd(nrrd, (*keyIt).c_str(), value.c_str());
      }
    }
#endif

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

#if 0
  // HEY this is disabled until ITK's NrrdIO is build with zlib
  //
  // Should we use compression?  Enabling compression always produces
  // binary output.
  if (this->GetUseCompression() == true)
    {
    // enable default nrrd compression flags
    nio->encoding = nrrdEncodingGzip;
    }
#endif

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
  
  // Write the nrrd to file.
  if (nrrdSave(this->GetFileName(), nrrd, nio))
    {
    char *err = biffGetDone(NRRD); // would be nice to free(err)
    itkExceptionMacro("Write: Error writing " 
                      << this->GetFileName() << ": " << err);
    }
  
  // Release the nrrd struct memory.  Do not free nrrd.data.
  nrrdNix(nrrd);
  nrrdIoStateNix(nio);

}
 
} // end namespace itk
