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
#include <itksys/Base64.h>
#include "gdcm/src/gdcmValEntry.h" //internal of gdcm
#include "gdcm/src/gdcmBinEntry.h" //internal of gdcm
#include "gdcm/src/gdcmFile.h"
#include "gdcm/src/gdcmHeader.h"
#include "gdcm/src/gdcmUtil.h"

#include <fstream>
#include <math.h>   //for fabs on SGI
#include <itksys/ios/sstream>

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
#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  m_GdcmHeader = NULL;
#endif
}

GDCMImageIO::~GDCMImageIO()
{
#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  delete m_GdcmHeader;
#endif
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

// Internal function to rescale pixel according to Rescale Slope/Intercept
template<class TBuffer, class TSource>
void RescaleFunction(TBuffer* buffer, TSource *source,
                     double slope, double intercept, size_t size)
{
  size /= sizeof(TSource);
  for(unsigned int i=0; i<size; i++)
   {
   buffer[i] = (TBuffer)(source[i]*slope + intercept);
   }
}

void GDCMImageIO::Read(void* buffer)
{
  std::ifstream file;

  //read header information file:
  this->InternalReadImageInformation(file);
  
  //Should I handle differently dicom lut ?
  //GdcmHeader.HasLUT()

#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  if( !m_GdcmHeader )
    {
    m_GdcmHeader = new gdcmHeader( m_FileName );
    }
#endif

#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  gdcmFile GdcmFile( m_FileName );
#else
  gdcm::File GdcmFile( m_FileName );
#endif
  size_t size = GdcmFile.GetImageDataSize();
  //== this->GetImageSizeInComponents()
  unsigned char *source = (unsigned char*)GdcmFile.GetImageData();

  switch(m_ComponentType)
    {
    case UCHAR:
      {
      unsigned char *ucbuffer = (unsigned char*)buffer;
      unsigned char *ucsource = (unsigned char*)source;
      RescaleFunction(ucbuffer, ucsource, m_RescaleSlope, m_RescaleIntercept, size);
      }
      break;
    case CHAR:
      {
      char *cbuffer = (char*)buffer;
      char *csource = (char*)source;
      RescaleFunction(cbuffer, csource, m_RescaleSlope, m_RescaleIntercept, size);
      }
      break;
    case USHORT:
      {
      unsigned short *usbuffer = (unsigned short*)buffer;
      unsigned short *ussource = (unsigned short*)source;
      RescaleFunction(usbuffer, ussource, m_RescaleSlope, m_RescaleIntercept, size);
      }
      break;
    case SHORT:
      {
      short *sbuffer = (short*)buffer;
      short *ssource = (short*)source;
      RescaleFunction(sbuffer, ssource, m_RescaleSlope, m_RescaleIntercept, size);
      }
      break;
    case UINT:
      {
      unsigned int *uibuffer = (unsigned int*)buffer;
      unsigned int *uisource = (unsigned int*)source;
      RescaleFunction(uibuffer, uisource, m_RescaleSlope, m_RescaleIntercept, size);
      }
      break;
    case INT:
      {
      int *ibuffer = (int*)buffer;
      int *isource = (int*)source;
      RescaleFunction(ibuffer, isource, m_RescaleSlope, m_RescaleIntercept, size);
      }
      break;
    case FLOAT:
      {
      // Particular case for PET image that need to be return as FLOAT image
      float *fbuffer = (float*)buffer;
      unsigned short *fsource = (unsigned short*)source;
      RescaleFunction(fbuffer, fsource, m_RescaleSlope, m_RescaleIntercept, size);
      }
      break;
    case DOUBLE:
      {
      double *dbuffer = (double*)buffer;
      double *dsource = (double*)source;
      RescaleFunction(dbuffer, dsource, m_RescaleSlope, m_RescaleIntercept, size);
      }
      break;
     default:
      itkExceptionMacro(<< "Unknown component type :" << m_ComponentType);
    }

//  NOTE: source should not be deleted. gdcm controls the pointer.

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

  // Before copying the image we need to check the slope/offset
  // If they are not integer the scalar become FLOAT:
  // Copy paste from DICOMAppHelper.cxx:
  // 0028 1052 DS IMG Rescale Intercept
  // 0028 1053 DS IMG Rescale Slope

  int s = int(m_RescaleSlope);
  int i = int(m_RescaleIntercept);
  float fs = float(s);
  float fi = float(i);

  double slope_dif = fabs(fs - m_RescaleSlope);
  double inter_dif = fabs(fi - m_RescaleIntercept);
  if (slope_dif > 0.0 || inter_dif > 0.0)
    {
    this->SetComponentType(ImageIOBase::FLOAT);
    }

  //Now copying the gdcm dictionary to the itk dictionary:
  MetaDataDictionary & dico = this->GetMetaDataDictionary();

#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  TagDocEntryHT & nameHt = GdcmHeader.GetEntry();
  for (TagDocEntryHT::iterator tag = nameHt.begin(); tag != nameHt.end(); ++tag)
    {
    // Do not copy field from private (unknown) dictionary.
    // In the longer term we might want to (but we need the Private dictionary
    // from manufacturer)
     if (tag->second->GetVR().find("SQ") == 0)
     {
       // skip DICOM SeQuence, otherwise following cast will crash
       continue;
     }
    std::string temp = ((gdcmValEntry*)(tag->second))->GetValue();
    if( tag->second->GetName() != "unkn" 
     && temp.find( "gdcm::NotLoaded" ) != 0
     && temp.find( "gdcm::Binary" ) != 0
     && temp.find( "gdcm::Loaded" ) != 0 )
      {
      EncapsulateMetaData<std::string>(dico, tag->second->GetName(),
                         ((gdcmValEntry*)(tag->second))->GetValue() );
      }
    }
#else
  GdcmHeader.Initialize();
  gdcm::DocEntry* d = GdcmHeader.GetNextEntry();

  // Copy of the header content
  while(d)
  {
    // Because BinEntry is a ValEntry...
    if ( gdcm::BinEntry* b = dynamic_cast<gdcm::BinEntry*>(d) )
      {
      if (b->GetName() != "Pixel Data" && b->GetName() != "unkn")
        {
        if (b->GetValue() == "gdcm::Binary data loaded")
          {
          // base64 streams have to be a multiple of 4 bytes long
          int encodedLengthEstimate = 2 * b->GetLength();
          encodedLengthEstimate = ((encodedLengthEstimate / 4) + 1) * 4;
            
          char *bin = new char[encodedLengthEstimate];
          int encodedLengthActual = itksysBase64_Encode(
            (const unsigned char *) b->GetBinArea(),
            b->GetLength(),
            (unsigned char *) bin,
            0);
          std::string encodedValue(bin, encodedLengthActual);
          EncapsulateMetaData<std::string>(dico, b->GetName(), encodedValue); 
          delete []bin;
          }      
        }
      }
    else if ( gdcm::ValEntry* v = dynamic_cast<gdcm::ValEntry*>(d) )
      {   
      // Only copying field from the public DICOM dictionary
      if( v->GetName() != "unkn")
        {       
        EncapsulateMetaData<std::string>(dico, v->GetName(), v->GetValue() );
        }
      }
    //else
    // We skip pb of SQ recursive exploration, and we do not copy binary entries

    d = GdcmHeader.GetNextEntry();
  }
#endif
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
         m_GdcmHeader->GetGrPixel(), m_GdcmHeader->GetNumPixel()); //Another API problem
  myGdcmFile->SetImageData((void*)buffer, numberOfBytes );

  myGdcmFile->WriteDcmExplVR( m_FileName );
  delete myGdcmFile;
#else
  gdcm::Header *myGdcmHeader = new gdcm::Header();
  gdcm::File *myGdcmFile = new gdcm::File( myGdcmHeader );

  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  // Using real iterators (instead of the GetKeys() method)
  itk::MetaDataDictionary::ConstIterator itr = dict.Begin();
  itk::MetaDataDictionary::ConstIterator end = dict.End();

  std::string value;
  while( itr != end )
    {
    const std::string &key = itr->first; //Needed for bcc32
    ExposeMetaData<std::string>(dict, key, value);

    // Convert DICOM name to DICOM (group,element)
#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
    gdcmDictEntry *dictEntry =
#else
    gdcm::DictEntry *dictEntry =
#endif
      myGdcmHeader->GetPubDict()->GetDictEntryByName(key);
    // Anything that has been change in the MetaData Dict will be pushed
    // into the DICOM header:
    if (dictEntry)
      {
      if (dictEntry->GetVR() != "OB" && dictEntry->GetVR() != "OW")
        {
        if(dictEntry->GetGroup() != 0 || dictEntry->GetElement() != 0)
          {
          myGdcmHeader->ReplaceOrCreateByNumber( value,
                                                 dictEntry->GetGroup(), 
                                                 dictEntry->GetElement());
          }
        }
      else
        {
        // convert value from Base64
        uint8_t *bin = new uint8_t[value.size()];
        unsigned int decodedLengthActual = itksysBase64_Decode(
          (const unsigned char *) value.c_str(),
          0,
          (unsigned char *) bin,
          value.size());
        if(dictEntry->GetGroup() != 0 || dictEntry->GetElement() != 0)
          {
          myGdcmHeader->ReplaceOrCreateByNumber( bin,
                                                 decodedLengthActual,
                                                 dictEntry->GetGroup(), 
                                                 dictEntry->GetElement());
          }
        }
      }
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
      pixelRep      = "1"; //Pixel Representation
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
      pixelRep      = "1"; //Pixel Representation
      break;

    default:
      itkExceptionMacro(<<"DICOM does not support this component type");
    }

  // Write component specific information in the header:
  myGdcmHeader->ReplaceOrCreateByNumber( bitsAllocated, 0x0028, 0x0100 ); //Bits Allocated
  myGdcmHeader->ReplaceOrCreateByNumber( bitsStored, 0x0028, 0x0101 ); //Bits Stored
  myGdcmHeader->ReplaceOrCreateByNumber( highBit, 0x0028, 0x0102 ); //High Bit
  myGdcmHeader->ReplaceOrCreateByNumber( pixelRep, 0x0028, 0x0103 ); //Pixel Representation

  static int imageNumber = 0;
  std::string itkradical = "1.2.826.0.1.3680043.2.1125";
  std::string uid = gdcm::Util::CreateUniqueUID( itkradical );
  itksys_ios::ostringstream s;
  s << uid << "." << imageNumber++;

  std::string SOPInstanceUID = s.str();
  //std::cerr << "Replacing:" << value << " by " << uid << std::endl;
  std::cerr << "Replacing:" << SOPInstanceUID << std::endl;
  myGdcmHeader->ReplaceOrCreateByNumber( SOPInstanceUID, 0x0008, 0x0018); //[SOP Instance UID]
  myGdcmHeader->ReplaceOrCreateByNumber( SOPInstanceUID, 0x0002, 0x0003); //[Media Stored SOP Instance UID]
  myGdcmHeader->ReplaceOrCreateByNumber( uid + ".1", 0x0020, 0x000d); //[Study Instance UID]
  myGdcmHeader->ReplaceOrCreateByNumber( uid + ".2", 0x0020, 0x000e); //[Series Instance UID]
  myGdcmHeader->ReplaceOrCreateByNumber( uid + ".3", 0x0020, 0x0052); //[Frame of Reference UID] 

  //copy data from buffer to DICOM buffer
  uint8_t* imageData = new uint8_t[numberOfBytes];
  memcpy(imageData, buffer, numberOfBytes);
  
  // Here we are passsing directly a pointer, this should
  myGdcmHeader->ReplaceOrCreateByNumber( imageData, numberOfBytes, 0x7fe0, 0x0010, "PXL" );
  myGdcmFile->WriteDcmExplVR( m_FileName );

  //myGdcmFile->SetWriteTypeToDcmExplVR();
  //myGdcmFile->Write( m_FileName );

  // DO NOT DELETE "imageData" since GDCM will delete it when the
  // GdcmHeader is deleted.
  
  delete myGdcmFile;
  delete myGdcmHeader;
#endif
}

void GDCMImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "RescaleSlope: " << m_RescaleSlope << "\n";
  os << indent << "RescaleIntercept: " << m_RescaleIntercept << "\n";
}


} // end namespace itk
