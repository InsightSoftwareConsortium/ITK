/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGDCMImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGDCMImageIO.h"

#include "itkMetaDataObject.h"
#include "gdcm/src/gdcmValEntry.h" //internal of gdcm
#include "gdcm/src/gdcmFile.h"
#include "gdcm/src/gdcmHeader.h"
#include <fstream>

namespace itk
{

GDCMImageIO::GDCMImageIO()
{
  this->SetNumberOfDimensions(3); //needed for getting the 3 coordinates of 
                                  // the origin, even if it is a 2D slice.

  m_ByteOrder = LittleEndian; //default
  m_FileType = Binary;  //default...always true
  m_RescaleSlope = 1.0;
  m_RescaleIntercept = 0.0;
  m_GdcmHeader = NULL;
}

GDCMImageIO::~GDCMImageIO()
{
  delete m_GdcmHeader;
}

bool GDCMImageIO::OpenGDCMFileForReading(std::ifstream& os, 
                                         const char* filename)
                                       
{
  // Make sure that we have a file to 
  if ( filename == "" )
    {
    itkExceptionMacro(<<"A FileName must be specified.");
    return false;
    }

  // Close file from any previous image
  if ( os.is_open() )
    {
    os.close();
    }
  
  // Open the new file for reading
  itkDebugMacro(<< "Initialize: opening file " << filename);

  // Actually open the file
  os.open( filename, std::ios::in | std::ios::binary );

  if ( os.fail() )
    {
    return false;
    }

  return true;
}


bool GDCMImageIO::OpenGDCMFileForWriting(std::ofstream& os, 
                                         const char* filename)
                                       
{
  // Make sure that we have a file to 
  if ( filename == "" )
    {
    itkExceptionMacro(<<"A FileName must be specified.");
    return false;
    }

  // Close file from any previous image
  if ( os.is_open() )
    {
    os.close();
    }
  
  // Open the new file for writing
  itkDebugMacro(<< "Initialize: opening file " << filename);

#ifdef __sgi
  // Create the file. This is required on some older sgi's
  std::ofstream tFile(filename,std::ios::out);
  tFile.close();                    
#endif

  // Actually open the file
  os.open( filename, std::ios::out | std::ios::binary );

  if( os.fail() )
    {
    itkExceptionMacro(<< "Could not open file for writing: " << filename);
    return false;
    }


  return true;
}

// This method will only test if the header looks like a
// GDCM image file.
bool GDCMImageIO::CanReadFile(const char* filename) 
{ 
  std::ifstream file;
  char buffer[256];
  std::string fname(filename);

  if(  fname == "" )
    {
    itkDebugMacro(<<"No filename specified.");
    return false;
    }

  bool extensionFound = false;
  std::string::size_type sprPos = fname.rfind(".dcm");  //acr nema ??
  if ((sprPos != std::string::npos)
      && (sprPos == fname.length() - 4))
    {
    extensionFound = true;
    }

  if( !extensionFound )
    {
    itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  //Check for file existence:
  if ( ! this->OpenGDCMFileForReading(file, filename))
    {
    return false;
    }

  // Check to see if its a valid dicom file gdcm is able to parse:
  //We are parsing the header one time here:

#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  gdcmHeader GdcmHeader( fname );
#else
  gdcm::Header GdcmHeader( fname );
#endif
  if (!GdcmHeader.IsReadable())
    {
    itkExceptionMacro("Gdcm cannot parse file " << filename );
    return false;
    }

  return true;
}

void GDCMImageIO::Read(void* buffer)
{
  std::ifstream file;

  //read header information file:
  this->InternalReadImageInformation(file);
  
  //Should I handle differently dicom lut ?
  //GdcmHeader.HasLUT()

  if( !m_GdcmHeader )
    {
#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
    m_GdcmHeader = new gdcmHeader( m_FileName );
#else
    m_GdcmHeader = new gdcm::Header( m_FileName );
#endif
    }

#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  gdcmFile GdcmFile( m_FileName );
#else
  gdcm::File GdcmFile( m_FileName );
#endif
  size_t size = GdcmFile.GetImageDataSize();
  //== this->GetImageSizeInComponents()
  unsigned char *Source = (unsigned char*)GdcmFile.GetImageData();
  memcpy(buffer, (void*)Source, size);
  delete[] Source;

  //closing files:
  file.close();
}

void GDCMImageIO::InternalReadImageInformation(std::ifstream& file)
{
  //read header
  if ( ! this->OpenGDCMFileForReading(file, m_FileName.c_str()) )
    {
    itkExceptionMacro(<< "Cannot read requested file");
    }

#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  gdcmHeader GdcmHeader( m_FileName );
#else
  gdcm::Header GdcmHeader( m_FileName );
#endif

  // We don't need to positionate the Endian related stuff (by using
  // this->SetDataByteOrderToBigEndian() or SetDataByteOrderToLittleEndian()
  // since the reading of the file is done by gdcm.
  // But we do need to set up the data type for downstream filters:

  std::string type = GdcmHeader.GetPixelType();
  if( type == "8U")
    {
    SetPixelType(SCALAR);
    SetComponentType(UCHAR);
    }
  else if( type == "8S")
    {
    SetPixelType(SCALAR);
    SetComponentType(CHAR);
    }
  else if( type == "16U")
    {
    SetPixelType(SCALAR);
    SetComponentType(USHORT);
    }
  else if( type == "16S")
    {
    SetPixelType(SCALAR);
    SetComponentType(SHORT);
    }
  else if( type == "32U")
    {
    SetPixelType(SCALAR);
    SetComponentType(UINT);
    }
  else if( type == "32S")
    {
    SetPixelType(SCALAR);
    SetComponentType(INT);
    }
  else if ( type == "FD" )
    {
    //64 bits Double image
    SetPixelType(SCALAR);
    SetComponentType(DOUBLE);
    }
  else
    {
    itkExceptionMacro(<<"Unrecognized type:" << type << " in file " << m_FileName);
    }

  // set values in case we don't find them
  m_Dimensions[0] = GdcmHeader.GetXSize();
  m_Dimensions[1] = GdcmHeader.GetYSize();
  m_Dimensions[2] = GdcmHeader.GetZSize();

  m_Spacing[0] = GdcmHeader.GetXSpacing();
  m_Spacing[1] = GdcmHeader.GetYSpacing();
  m_Spacing[2] = GdcmHeader.GetZSpacing();

  m_Origin[0] = GdcmHeader.GetXOrigin();
  m_Origin[1] = GdcmHeader.GetYOrigin();
  m_Origin[2] = GdcmHeader.GetZOrigin();

  //For grayscale image :
  m_RescaleSlope = GdcmHeader.GetRescaleSlope();
  m_RescaleIntercept = GdcmHeader.GetRescaleIntercept();

  //Now copying the gdcm dictionary to the itk dictionary:
  MetaDataDictionary & dico = this->GetMetaDataDictionary();

#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  TagDocEntryHT & nameHt = GdcmHeader.GetEntry();
  for (TagDocEntryHT::iterator tag = nameHt.begin(); tag != nameHt.end(); ++tag)
#else
  const gdcm::TagDocEntryHT & nameHt = GdcmHeader.GetTagHT();
  for (gdcm::TagDocEntryHT::const_iterator tag = nameHt.begin(); tag != nameHt.end(); ++tag)
#endif
    {
    // Do not copy field from private (unknown) dictionary.
    // In the longer term we might want to (but we need the Private dictionary
    // from manufacturer)
     if (tag->second->GetVR().find("SQ") == 0)
     {
       // skip DICOM SeQuence, otherwise following cast will crash
       continue;
     }
#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
    std::string temp = ((gdcmValEntry*)(tag->second))->GetValue();
#else
    std::string temp = ((gdcm::ValEntry*)(tag->second))->GetValue();
#endif
    if( tag->second->GetName() != "unkn" 
     && temp.find( "gdcm::NotLoaded" ) != 0
     && temp.find( "gdcm::Loaded" ) != 0 )
      {
      EncapsulateMetaData<std::string>(dico, tag->second->GetName(),
#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
                         ((gdcmValEntry*)(tag->second))->GetValue() );
#else
                         ((gdcm::ValEntry*)(tag->second))->GetValue() );
#endif
      }
    }
}

void GDCMImageIO::ReadImageInformation()
{
  std::ifstream file;
  this->InternalReadImageInformation(file);
}


bool GDCMImageIO::CanWriteFile(const char* name)
{
  std::string filename = name;

  if(  filename == "" )
    {
    itkDebugMacro(<<"No filename specified.");
    return false;
    }

  bool extensionFound = false;
  std::string::size_type sprPos = filename.rfind(".dcm");
  if ((sprPos != std::string::npos)
      && (sprPos == filename.length() - 4))
    {
    extensionFound = true;
    }

  if( !extensionFound )
    {
    itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  return true;
}

void GDCMImageIO::WriteImageInformation() 
{
}

void GDCMImageIO::Write(const void* buffer)
{
  std::ofstream file;
  if ( !this->OpenGDCMFileForWriting(file, m_FileName.c_str()) )
    {
    return;
    }
  file.close();

  const unsigned long numberOfBytes = this->GetImageSizeInBytes();

#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  gdcmFile *myGdcmFile = new gdcmFile( m_GdcmHeader );
  myGdcmFile->GetImageData();  //API problem
  m_GdcmHeader->SetEntryVoidAreaByNumber( (void*)buffer, 
         m_GdcmHeader->GetGrPixel(), m_GdcmHeader->GetNumPixel());//Another API problem
  myGdcmFile->SetImageData((void*)buffer, numberOfBytes );
#else
  gdcm::File *myGdcmFile = new gdcm::File( m_GdcmHeader );
  uint8_t* imageData = myGdcmFile->GetImageData();

  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  // Using real iterators (instead of the GetKeys() method)
  itk::MetaDataDictionary::ConstIterator itr = dict.Begin();
  itk::MetaDataDictionary::ConstIterator end = dict.End();

  std::string value;
  while( itr != end )
    {
    ExposeMetaData<std::string>(dict, itr->first, value);

    // Convert DICOM name to DICOM (group,element)
#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
    gdcmDictEntry *dictEntry =
#else
    gdcm::DictEntry *dictEntry =
#endif
       m_GdcmHeader->GetPubDict()->GetDictEntryByName(itr->first);
     // Anything that has been change in the MetaData Dict will be pushed
     // into the DICOM header:
     m_GdcmHeader->ReplaceOrCreateByNumber( value, dictEntry->GetGroup(), 
                                                   dictEntry->GetElement());
     ++itr;
     }

  // Handle the bitDepth:
  std::string bitsAllocated;
  std::string bitsStored;
  std::string highBit;
  std::string pixelRep;
  switch (this->GetComponentType())
    {
    case CHAR:
      bitsAllocated = "8"; // Bits Allocated
      bitsStored    = "8"; // Bits Stored
      highBit       = "7"; // High Bit
      // 8bits DICOM cannot be signed
      pixelRep      = "0"; //Pixel Representation
      break;

    case UCHAR:
      bitsAllocated = "8"; // Bits Allocated
      bitsStored    = "8"; // Bits Stored
      highBit       = "7"; // High Bit
      pixelRep      = "0"; //Pixel Representation
      break;

    case SHORT:
      bitsAllocated = "16"; // Bits Allocated
      bitsStored    = "16"; // Bits Stored
      highBit       = "15"; // High Bit
      pixelRep      = "1"; //Pixel Representation
      break;    

    case USHORT:
      bitsAllocated = "16"; // Bits Allocated
      bitsStored    = "16"; // Bits Stored
      highBit       = "15"; // High Bit
      pixelRep      = "0"; //Pixel Representation
      break;

    default:
      itkExceptionMacro(<<"DICOM does not support this component type");
    }

  // Write component specific information in the header:
  m_GdcmHeader->ReplaceOrCreateByNumber( bitsAllocated, 0x0028, 0x0100 ); //Bits Allocated
  m_GdcmHeader->ReplaceOrCreateByNumber( bitsStored, 0x0028, 0x0101 ); //Bits Stored
  m_GdcmHeader->ReplaceOrCreateByNumber( highBit, 0x0028, 0x0102 ); //High Bit
  m_GdcmHeader->ReplaceOrCreateByNumber( pixelRep, 0x0028, 0x0103 ); //Pixel Representation

  //copy data from buffer to DICOM buffer
  memcpy(imageData,buffer,numberOfBytes); 
  m_GdcmHeader->SetImageDataSize(numberOfBytes); //update data size
#endif

  myGdcmFile->WriteDcmExplVR( m_FileName );
  delete myGdcmFile;
}

void GDCMImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "RescaleSlope: " << m_RescaleSlope << "\n";
  os << indent << "RescaleIntercept: " << m_RescaleIntercept << "\n";
}


} // end namespace itk
