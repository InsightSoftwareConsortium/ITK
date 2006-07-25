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
#include "itkIOCommon.h"
#include "itkPoint.h"
#include "itkArray.h"
#include "itkMatrix.h"
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_cross.h>

#include "itkMetaDataObject.h"

#include <itksys/SystemTools.hxx>
#include <itksys/Base64.h>
#include "gdcmValEntry.h" //internal of gdcm
#include "gdcmBinEntry.h" //internal of gdcm
#include "gdcmFile.h"
#include "gdcmFileHelper.h"
#include "gdcmUtil.h"
#include "gdcmGlobal.h"   // access to dictionary
#include "gdcmDictSet.h"  // access to dictionary


#include <fstream>
#include <math.h>   //for fabs on SGI
#include <itksys/ios/sstream>

namespace itk
{
class InternalHeader
{
public:
  InternalHeader() : m_Header(0) {}
  gdcm::File *m_Header;
};

// Initialize static members
bool GDCMImageIO::m_LoadSequencesDefault = false;
bool GDCMImageIO::m_LoadPrivateTagsDefault = false;


GDCMImageIO::GDCMImageIO()
  : m_LoadSequences( m_LoadSequencesDefault ),
    m_LoadPrivateTags( m_LoadPrivateTagsDefault )
{
  this->DICOMHeader = new InternalHeader;
  this->SetNumberOfDimensions(3); //needed for getting the 3 coordinates of
                                  // the origin, even if it is a 2D slice.
  m_ByteOrder = LittleEndian; //default
  m_FileType = Binary;  //default...always true
  m_RescaleSlope = 1.0;
  m_RescaleIntercept = 0.0;
  // UIDPrefix is the ITK root id tacked with a ".1"
  // allowing to designate a subspace of the id space for ITK generated DICOM
  m_UIDPrefix = "1.2.826.0.1.3680043.2.1125." "1";

  // Purely internal use, no user access:
  m_StudyInstanceUID = "";
  m_SeriesInstanceUID = "";
  m_FrameOfReferenceInstanceUID = "";

  m_KeepOriginalUID = false;
  m_MaxSizeLoadEntry = 0xfff;

  m_InternalComponentType = UNKNOWNCOMPONENTTYPE;

  // by default assume that images will be 2D.
  // This number is updated according the information 
  // received through the MetaDataDictionary
  m_GlobalNumberOfDimensions = 2;
  // By default use JPEG2000. For legacy system, one should prefer JPEG since
  // JPEG2000 was only recently added to the DICOM standart
  m_CompressionType = JPEG2000;
}

GDCMImageIO::~GDCMImageIO()
{
  if (this->DICOMHeader->m_Header)
    {
    delete this->DICOMHeader->m_Header;
    }
  delete this->DICOMHeader;
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
    itkExceptionMacro(<< "Could not open file: "
                      << filename << " for writing."
                      << std::endl
                      << "Reasion: "
                      << itksys::SystemTools::GetLastSystemError());
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

  //Check for file existence:
  if ( ! this->OpenGDCMFileForReading(file, filename))
    {
    return false;
    }

  // Check to see if its a valid dicom file gdcm is able to parse:
  // We are parsing the header one time here:

  bool pre;
  return gdcm::Document::CanReadFile(file, pre);
}

// Internal function to rescale pixel according to Rescale Slope/Intercept
template<class TBuffer, class TSource>
void RescaleFunction(TBuffer* buffer, TSource *source,
                     double slope, double intercept, size_t size)
{
  size /= sizeof(TSource);

  if (slope != 1.0 && intercept != 0.0)
    {
    // Duff's device.  Instead of this code:
    //
    //   for(unsigned int i=0; i<size; i++)
    //    {
    //    buffer[i] = (TBuffer)(source[i]*slope + intercept);
    //    }
    //
    // use Duff's device which exploits "fall through"
    register size_t n = (size + 7) / 8;
    switch ( size % 8)
      {
      case 0: do { *buffer++ = (TBuffer)((*source++)*slope + intercept);
      case 7:      *buffer++ = (TBuffer)((*source++)*slope + intercept);
      case 6:      *buffer++ = (TBuffer)((*source++)*slope + intercept);
      case 5:      *buffer++ = (TBuffer)((*source++)*slope + intercept);
      case 4:      *buffer++ = (TBuffer)((*source++)*slope + intercept);
      case 3:      *buffer++ = (TBuffer)((*source++)*slope + intercept);
      case 2:      *buffer++ = (TBuffer)((*source++)*slope + intercept);
      case 1:      *buffer++ = (TBuffer)((*source++)*slope + intercept);
                 }  while (--n > 0);
      }
    }
  else if (slope == 1.0 && intercept != 0.0)
    {
    // Duff's device.  Instead of this code:
    //
    //   for(unsigned int i=0; i<size; i++)
    //    {
    //    buffer[i] = (TBuffer)(source[i] + intercept);
    //    }
    //
    // use Duff's device which exploits "fall through"
    register size_t n = (size + 7) / 8;
    switch ( size % 8)
      {
      case 0: do { *buffer++ = (TBuffer)(*source++ + intercept);
      case 7:      *buffer++ = (TBuffer)(*source++ + intercept);
      case 6:      *buffer++ = (TBuffer)(*source++ + intercept);
      case 5:      *buffer++ = (TBuffer)(*source++ + intercept);
      case 4:      *buffer++ = (TBuffer)(*source++ + intercept);
      case 3:      *buffer++ = (TBuffer)(*source++ + intercept);
      case 2:      *buffer++ = (TBuffer)(*source++ + intercept);
      case 1:      *buffer++ = (TBuffer)(*source++ + intercept);
                 }  while (--n > 0);
      }
    }
  else if (slope != 1.0 && intercept == 0.0)
    {
    // Duff's device.  Instead of this code:
    //
    //   for(unsigned int i=0; i<size; i++)
    //    {
    //    buffer[i] = (TBuffer)(source[i]*slope);
    //    }
    //
    // use Duff's device which exploits "fall through"
    register size_t n = (size + 7) / 8;
    switch ( size % 8)
      {
      case 0: do { *buffer++ = (TBuffer)((*source++)*slope);
      case 7:      *buffer++ = (TBuffer)((*source++)*slope);
      case 6:      *buffer++ = (TBuffer)((*source++)*slope);
      case 5:      *buffer++ = (TBuffer)((*source++)*slope);
      case 4:      *buffer++ = (TBuffer)((*source++)*slope);
      case 3:      *buffer++ = (TBuffer)((*source++)*slope);
      case 2:      *buffer++ = (TBuffer)((*source++)*slope);
      case 1:      *buffer++ = (TBuffer)((*source++)*slope);
                 }  while (--n > 0);
      }
    }
  else
    {
    // Duff's device.  Instead of this code:
    //
    //   for(unsigned int i=0; i<size; i++)
    //    {
    //    buffer[i] = (TBuffer)(source[i]);
    //    }
    //
    // use Duff's device which exploits "fall through"
    register size_t n = (size + 7) / 8;
    switch ( size % 8)
      {
      case 0: do { *buffer++ = (TBuffer)(*source++);
      case 7:      *buffer++ = (TBuffer)(*source++);
      case 6:      *buffer++ = (TBuffer)(*source++);
      case 5:      *buffer++ = (TBuffer)(*source++);
      case 4:      *buffer++ = (TBuffer)(*source++);
      case 3:      *buffer++ = (TBuffer)(*source++);
      case 2:      *buffer++ = (TBuffer)(*source++);
      case 1:      *buffer++ = (TBuffer)(*source++);
                 }  while (--n > 0);
      }
    }
}


template<class TSource>
void RescaleFunction(ImageIOBase::IOComponentType bufferType,
                     void* buffer, TSource *source,
                     double slope, double intercept, size_t size)
{
  switch (bufferType)
    {
    case ImageIOBase::UCHAR:
      RescaleFunction( (unsigned char *)buffer, source, slope, intercept, size);
      break;
    case ImageIOBase::CHAR:
      RescaleFunction( (char *)buffer, source, slope, intercept, size);
      break;
    case ImageIOBase::USHORT:
      RescaleFunction( (unsigned short *)buffer, source, slope, intercept,size);
      break;
    case ImageIOBase::SHORT:
      RescaleFunction( (short *)buffer, source, slope, intercept, size);
      break;
    case ImageIOBase::UINT:
      RescaleFunction( (unsigned int *)buffer, source, slope, intercept, size);
      break;
    case ImageIOBase::INT:
      RescaleFunction( (int *)buffer, source, slope, intercept, size);
      break;
    case ImageIOBase::FLOAT:
      RescaleFunction( (float *)buffer, source, slope, intercept, size);
      break;
    case ImageIOBase::DOUBLE:
      RescaleFunction( (double *)buffer, source, slope, intercept, size);
      break;
    default:
      ::itk::OStringStream message;
      message << "itk::ERROR: GDCMImageIO: Unknown component type : " << bufferType;
      ::itk::ExceptionObject e(__FILE__, __LINE__, message.str().c_str(),ITK_LOCATION);
      throw e;
    }
}


void GDCMImageIO::Read(void* buffer)
{
  std::ifstream file;

  //Should I handle differently dicom lut ?
  //GdcmHeader.HasLUT()

  gdcm::File *header = this->DICOMHeader->m_Header;
  gdcm::FileHelper gfile(header);

  size_t size = gfile.GetImageDataSize();
  unsigned char *source = (unsigned char*)gfile.GetImageData();

  // We can rescale pixel only in grayscale image
  if( m_NumberOfComponents == 1 )
    {
    switch(m_InternalComponentType)
      {
      case ImageIOBase::UCHAR:
        {
        RescaleFunction(m_ComponentType, buffer, (unsigned char*)source,
                        m_RescaleSlope, m_RescaleIntercept, size);
        }
        break;
      case ImageIOBase::CHAR:
        {
        RescaleFunction(m_ComponentType, buffer, (char*)source,
                        m_RescaleSlope, m_RescaleIntercept, size);
        }
        break;
      case ImageIOBase::USHORT:
        {
        RescaleFunction(m_ComponentType, buffer, (unsigned short*)source,
                        m_RescaleSlope, m_RescaleIntercept, size);
        }
        break;
      case ImageIOBase::SHORT:
        {
        RescaleFunction(m_ComponentType, buffer, (short*)source,
                        m_RescaleSlope, m_RescaleIntercept, size);
        }
        break;
      case ImageIOBase::UINT:
        {
        RescaleFunction(m_ComponentType, buffer, (unsigned int*)source,
                        m_RescaleSlope, m_RescaleIntercept, size);
        }
        break;
      case ImageIOBase::INT:
        {
        RescaleFunction(m_ComponentType, buffer, (int*)source,
                        m_RescaleSlope, m_RescaleIntercept, size);
        }
        break;
      case ImageIOBase::FLOAT:
        {
        RescaleFunction(m_ComponentType, buffer, (float*)source,
                        m_RescaleSlope, m_RescaleIntercept, size);
        }
        break;
      case ImageIOBase::DOUBLE:
        {
        RescaleFunction(m_ComponentType, buffer, (double *)source,
                        m_RescaleSlope, m_RescaleIntercept, size);
        }
        break;
       default:
        itkExceptionMacro(<< "Unknown component type :" << m_ComponentType);
      }
    }
  else
    {
    // This is a RGB buffer, only do a straight copy:
    memcpy(buffer, source, size);
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

  gdcm::File *header = new gdcm::File;
  delete this->DICOMHeader->m_Header;
  this->DICOMHeader->m_Header = header;
  header->SetMaxSizeLoadEntry(m_MaxSizeLoadEntry);
  header->SetFileName( m_FileName );
  header->SetLoadMode( (m_LoadSequences ? 0 : gdcm::LD_NOSEQ)
                       | (m_LoadPrivateTags ? 0 : gdcm::LD_NOSHADOW));
  header->Load();

  // We don't need to positionate the Endian related stuff (by using
  // this->SetDataByteOrderToBigEndian() or SetDataByteOrderToLittleEndian()
  // since the reading of the file is done by gdcm.
  // But we do need to set up the data type for downstream filters:

  int numComp = header->GetNumberOfScalarComponents();
  this->SetNumberOfComponents(numComp);
  if (numComp == 1)
    {
    this->SetPixelType(SCALAR);
    }
  else
    {
    this->SetPixelType(RGB);
    }
  std::string type = header->GetPixelType();
  if( type == "8U")
    {
    SetComponentType(ImageIOBase::UCHAR);
    }
  else if( type == "8S")
    {
    SetComponentType(ImageIOBase::CHAR);
    }
  else if( type == "16U")
    {
    SetComponentType(ImageIOBase::USHORT);
    }
  else if( type == "16S")
    {
    SetComponentType(ImageIOBase::SHORT);
    }
  else if( type == "32U")
    {
    SetComponentType(ImageIOBase::UINT);
    }
  else if( type == "32S")
    {
    SetComponentType(ImageIOBase::INT);
    }
  else if ( type == "FD" )
    {
    //64 bits Double image
    SetComponentType(ImageIOBase::DOUBLE);
    }
  else
    {
    itkExceptionMacro(<<"Unrecognized type:" << type << " in file " << m_FileName);
    }
  // The internal component type (as on disk) will by default match
  // the external component type.  The external component type may
  // change (below) as a function of the rescale slope and intersept.
  m_InternalComponentType = m_ComponentType;

  // set values in case we don't find them
  m_Dimensions[0] = header->GetXSize();
  m_Dimensions[1] = header->GetYSize();
  m_Dimensions[2] = header->GetZSize();

  m_Spacing[0] = header->GetXSpacing();
  m_Spacing[1] = header->GetYSpacing();
  m_Spacing[2] = header->GetZSpacing();


  float imageOrientation[6];
  header->GetImageOrientationPatient(imageOrientation);
  vnl_vector<double> rowDirection(3), columnDirection(3);

  rowDirection[0] = imageOrientation[0];
  rowDirection[1] = imageOrientation[1];
  rowDirection[2] = imageOrientation[2];

  columnDirection[0] = imageOrientation[3];
  columnDirection[1] = imageOrientation[4];
  columnDirection[2] = imageOrientation[5];

  vnl_vector<double> sliceDirection = vnl_cross_3d(rowDirection, columnDirection);
  m_Direction.resize(3);
  this->SetDirection(0, rowDirection);
  this->SetDirection(1, columnDirection);
  this->SetDirection(2, sliceDirection);

  // Dicom's origin is always in LPS
  m_Origin[0] = header->GetXOrigin();
  m_Origin[1] = header->GetYOrigin();
  m_Origin[2] = header->GetZOrigin();

  //For grayscale image :
  m_RescaleSlope = header->GetRescaleSlope();
  m_RescaleIntercept = header->GetRescaleIntercept();

  // Before copying the image we need to check the slope/offset
  // If they are not integer the scalar become FLOAT:
  // Copy paste from DICOMAppHelper.cxx:
  // 0028 1052 DS IMG Rescale Intercept
  // 0028 1053 DS IMG Rescale Slope

  int s = int(m_RescaleSlope);
  int i = int(m_RescaleIntercept);
  float fs = float(s);
  float fi = float(i);

  double slope_dif = vcl_fabs(fs - m_RescaleSlope);
  double inter_dif = vcl_fabs(fi - m_RescaleIntercept);
  if (slope_dif > 0.0 || inter_dif > 0.0)
    {
    if (m_ComponentType != ImageIOBase::DOUBLE)
      {
      this->SetComponentType(ImageIOBase::FLOAT);
      }
    }

  // If the intercept is negative, unsigned integral values need to be
  // changed to signed integral values. Otherwise, the rescale intercept
  // code will overflow.

  if (m_RescaleIntercept < 0)
    {
    switch (m_ComponentType)
      {
      case ImageIOBase::UCHAR:
        m_ComponentType = ImageIOBase::CHAR;
        break;
      case ImageIOBase::UINT:
        m_ComponentType = ImageIOBase::INT;
        break;
      case ImageIOBase::USHORT:
        m_ComponentType = ImageIOBase::SHORT;
        break;
      case ImageIOBase::ULONG:
        m_ComponentType = ImageIOBase::LONG;
        break;
      default:
        break;
      }
    }

  //Now copying the gdcm dictionary to the itk dictionary:
  MetaDataDictionary & dico = this->GetMetaDataDictionary();

  gdcm::DocEntry* d = header->GetFirstEntry();

  // Copy of the header->content
  while(d)
  {
    // Because BinEntry is a ValEntry...
    if ( gdcm::BinEntry* b = dynamic_cast<gdcm::BinEntry*>(d) )
      {
      if (b->GetName() != "Pixel Data" && b->GetName() != "unkn")
        {
        if (b->GetValue() == gdcm::GDCM_BINLOADED )
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
          EncapsulateMetaData<std::string>(dico, b->GetKey(), encodedValue);
          delete []bin;
          }
        }
      }
    else if ( gdcm::ValEntry* v = dynamic_cast<gdcm::ValEntry*>(d) )
      {
      // Only copying field from the public DICOM dictionary
      if( v->GetName() != "unkn")
        {
        EncapsulateMetaData<std::string>(dico, v->GetKey(), v->GetValue() );
        }
      }
    //else
    // We skip pb of SQ recursive exploration, and we do not copy binary entries

    d = header->GetNextEntry();
  }

  // Now is a good time to fill in the class member:
  char name[512];
  this->GetPatientName(name);
  this->GetPatientID(name);
  this->GetPatientSex(name);
  this->GetPatientAge(name);
  this->GetStudyID(name);
  this->GetPatientDOB(name);
  this->GetStudyDescription(name);
  this->GetBodyPart(name);
  this->GetNumberOfSeriesInStudy(name);
  this->GetNumberOfStudyRelatedSeries(name);
  this->GetStudyDate(name);
  this->GetModality(name);
  this->GetManufacturer(name);
  this->GetInstitution(name);
  this->GetModel(name);
  this->GetScanOptions(name);

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

  std::string::size_type dcmPos = filename.rfind(".dcm");
  if ( (dcmPos != std::string::npos)
       && (dcmPos == filename.length() - 4) )
    {
    return true;
    }

  dcmPos = filename.rfind(".DCM");
  if ( (dcmPos != std::string::npos)
       && (dcmPos == filename.length() - 4) )
    {
    return true;
    }

  std::string::size_type dicomPos = filename.rfind(".dicom");
  if ( (dicomPos != std::string::npos)
       && (dicomPos == filename.length() - 6) )
    {
    return true;
    }

  dicomPos = filename.rfind(".DICOM");
  if ( (dicomPos != std::string::npos)
       && (dicomPos == filename.length() - 6) )
    {
    return true;
    }

  return false;
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

  const size_t numberOfBytes = this->GetImageSizeInBytes();

  gdcm::File *header = new gdcm::File();
  gdcm::FileHelper *gfile = new gdcm::FileHelper( header );

  std::string value;
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
#if defined(_MSC_VER) && _MSC_VER < 1300
  // Not using real iterators, but instead the GetKeys() method
  // since VS6 is broken and does not export properly iterators
  // GetKeys will duplicate the entire DICOM header
  std::vector<std::string> keys = dict.GetKeys();
  for( std::vector<std::string>::const_iterator it = keys.begin();
    it != keys.end(); ++it )
    {
    const std::string &key = *it; //Needed for bcc32
#else
  //Smarter approach using real iterators
  itk::MetaDataDictionary::ConstIterator itr = dict.Begin();
  itk::MetaDataDictionary::ConstIterator end = dict.End();
  while(itr != end)
    {
    const std::string &key = itr->first; //Needed for bcc32
#endif
    ExposeMetaData<std::string>(dict, key, value);

    // Convert DICOM name to DICOM (group,element)
    gdcm::DictEntry *dictEntry =
      header->GetPubDict()->GetEntry(key);
    // Anything that has been changed in the MetaData Dict will be pushed
    // into the DICOM header:
    if (dictEntry)
      {
      if (dictEntry->GetVR() != "OB" && dictEntry->GetVR() != "OW")
        {
        if(dictEntry->GetGroup() != 0 && dictEntry->GetElement() != 0)
          {
          header->InsertValEntry( value,
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
          header->InsertBinEntry( bin,
                                  decodedLengthActual,
                                  dictEntry->GetGroup(),
                                  dictEntry->GetElement());
          }
        delete []bin;
        }
      }
    else
      {
      // This is not a DICOM entry, then check if it is one of the ITK standard ones
      if( key == ITK_NumberOfDimensions )
        {
        unsigned int numberOfDimensions;
        ExposeMetaData<unsigned int>(dict, key, numberOfDimensions);
        m_GlobalNumberOfDimensions = numberOfDimensions;
        }
      else
        {
        if( key == ITK_Origin )
          {
          typedef Array< double > DoubleArrayType;
          DoubleArrayType originArray;
          ExposeMetaData< DoubleArrayType >( dict, key, originArray );
          m_Origin[0] = originArray[0];
          m_Origin[1] = originArray[1];
          m_Origin[2] = originArray[2];
          }
        else
          {
          if( key == ITK_Spacing )
            {
            typedef Array< double > DoubleArrayType;
            DoubleArrayType spacingArray;
            ExposeMetaData< DoubleArrayType >( dict, key, spacingArray );
            m_Spacing[0] = spacingArray[0];
            m_Spacing[1] = spacingArray[1];
            m_Spacing[2] = spacingArray[2];
            }
          else
            {
            std::cerr << "GDCMImageIO: non-DICOM and non-ITK standard key = ";
            std::cerr << key << std::endl;
            }
          }
        }
      }

#if !(defined(_MSC_VER) && _MSC_VER < 1300)
    ++itr;
#endif
    }

  // Handle the dimension of image:
  itksys_ios::ostringstream str;
  str << m_Dimensions[0];
  header->InsertValEntry( str.str(), 0x0028,0x0011); // Columns

  str.str("");
  str << m_Dimensions[1];
  header->InsertValEntry( str.str(), 0x0028,0x0010); // Rows

  if( m_Dimensions.size() > 2 && m_Dimensions[2] > 1 )
    {
    str.str("");
    str << m_Dimensions[2];
    //header->Insert(str.str(),0x0028,0x0012); // Planes
    header->InsertValEntry(str.str(),0x0028,0x0008); // Number of Frames
    }

  // Handle pixel spacing:
  str.str("");
  str.setf( itksys_ios::ios::fixed ); //forcing precision to 6 digits
  str << m_Spacing[1] << "\\" << m_Spacing[0];
  header->InsertValEntry(str.str(),0x0028,0x0030); // Pixel Spacing

  // Anyway we will still allow writting of the 3d component of the spacing
  // when we are writing 3d images:
  if(m_Dimensions.size() > 2 && m_Dimensions[2]>1)
    {
    str.str("");
    str << m_Spacing[2];
    header->InsertValEntry(str.str(),0x0018,0x0088); // Spacing Between Slices
    }

// This code still needs work. Spacing, origin and direction are all 3D, yet
// the image is 2D. If the user set these, all is well, because the user will
// pass in the proper number (3) of elements. However, ImageSeriesWriter will
// call its ImageIO with 2D images and only pass in spacing, origin and
// direction with 2 elements. For now, we expect that the MetaDataDictionary
// will have the proper settings for pixel spacing, spacing between slices,
// image position patient and the row/column direction cosines.

  if( ( m_Dimensions.size() > 2 && m_Dimensions[2]>1 ) || 
        m_GlobalNumberOfDimensions == 3 )
    {
    str.str("");
    str << m_Origin[0] << "\\" << m_Origin[1] << "\\" << m_Origin[2];
    header->InsertValEntry(str.str(),0x0020,0x0032); // Image Position (Patient)

    // Handle Direction = Image Orientation Patient
    str.str("");
    str << m_Direction[0][0] << "\\"
      << m_Direction[1][0] << "\\"
      << m_Direction[2][0] << "\\"
      << m_Direction[0][1] << "\\"
      << m_Direction[1][1] << "\\"
      << m_Direction[2][1];
    header->InsertValEntry(str.str(),0x0020,0x0037); // Image Orientation (Patient)
    }

  str.unsetf( itksys_ios::ios::fixed ); // back to normal
  // Handle the bitDepth:
  std::string bitsAllocated;
  std::string bitsStored;
  std::string highBit;
  std::string pixelRep;

  if( m_NumberOfComponents == 1 )
    {
    gfile->SetWriteTypeToDcmExplVR();

    switch (this->GetComponentType())
      {
      case ImageIOBase::CHAR:
        bitsAllocated = "8"; // Bits Allocated
        bitsStored    = "8"; // Bits Stored
        highBit       = "7"; // High Bit
        pixelRep      = "1"; // Pixel Representation
        break;

      case ImageIOBase::UCHAR:
        bitsAllocated = "8"; // Bits Allocated
        bitsStored    = "8"; // Bits Stored
        highBit       = "7"; // High Bit
        pixelRep      = "0"; // Pixel Representation
        break;

      case ImageIOBase::SHORT:
        bitsAllocated = "16"; // Bits Allocated
        bitsStored    = "16"; // Bits Stored
        highBit       = "15"; // High Bit
        pixelRep      = "1";  // Pixel Representation
        break;

      case ImageIOBase::USHORT:
        bitsAllocated = "16"; // Bits Allocated
        bitsStored    = "16"; // Bits Stored
        highBit       = "15"; // High Bit
        pixelRep      = "0";  // Pixel Representation
        break;

      default:
        itkExceptionMacro(<<"DICOM does not support this component type");
      }
    }
  else if( m_NumberOfComponents == 3 )
    {
    // Write the image as RGB DICOM
    gfile->SetWriteModeToRGB();
    switch (this->GetComponentType())
      {
      case ImageIOBase::CHAR:
        bitsAllocated = "8"; // Bits Allocated
        bitsStored    = "8"; // Bits Stored
        highBit       = "7"; // High Bit
        pixelRep      = "1"; // Pixel Representation
        break;

      case ImageIOBase::UCHAR:
        bitsAllocated = "8"; // Bits Allocated
        bitsStored    = "8"; // Bits Stored
        highBit       = "7"; // High Bit
        pixelRep      = "0"; // Pixel Representation
        break;

      default:
        itkExceptionMacro(<<"DICOM does not support this component type");
      }
    }
  else
    {
    itkExceptionMacro(<<"DICOM does not support RGBPixels with components != 3");
    }

  // Write component specific information in the header:
  header->InsertValEntry( bitsAllocated, 0x0028, 0x0100 ); //Bits Allocated
  header->InsertValEntry( bitsStored, 0x0028, 0x0101 ); //Bits Stored
  header->InsertValEntry( highBit, 0x0028, 0x0102 ); //High Bit
  header->InsertValEntry( pixelRep, 0x0028, 0x0103 ); //Pixel Representation

  str.str("");
  str << m_NumberOfComponents;
  header->InsertValEntry(str.str(),0x0028,0x0002); // Samples per Pixel

  if( !m_KeepOriginalUID )
  {
    // UID generation part:
    // We only create *ONE* Study/Series.Frame of Reference Instance UID
    if( m_StudyInstanceUID.empty() )
    {
      // As long as user maintain there gdcmIO they will keep the same
      // Study/Series instance UID.
      m_StudyInstanceUID = gdcm::Util::CreateUniqueUID( m_UIDPrefix );
      m_SeriesInstanceUID = gdcm::Util::CreateUniqueUID( m_UIDPrefix );
      m_FrameOfReferenceInstanceUID = gdcm::Util::CreateUniqueUID( m_UIDPrefix );
    }
    std::string uid = gdcm::Util::CreateUniqueUID( m_UIDPrefix );

    header->InsertValEntry( uid, 0x0008, 0x0018); //[SOP Instance UID]
    header->InsertValEntry( uid, 0x0002, 0x0003); //[Media Stored SOP Instance UID]
    header->InsertValEntry( m_StudyInstanceUID, 0x0020, 0x000d); //[Study Instance UID]
    header->InsertValEntry( m_SeriesInstanceUID, 0x0020, 0x000e); //[Series Instance UID]
    header->InsertValEntry( m_FrameOfReferenceInstanceUID, 0x0020, 0x0052); //[Frame of Reference UID] 
    // Secondary Capture Image Storage SOP Class
    header->InsertValEntry( "1.2.840.10008.5.1.4.1.1.7", 0x0002, 0x0012); //[Implementation Class UID]
  }

  //copy data from buffer to DICOM buffer
  uint8_t* imageData = new uint8_t[numberOfBytes];
  memcpy(imageData, buffer, numberOfBytes);

  // If user ask to use compression:
  if( m_UseCompression )
    {
    if( m_CompressionType == JPEG )
      {
      gfile->SetWriteTypeToJPEG();
      }
    else if ( m_CompressionType == JPEG2000 )
      {
      gfile->SetWriteTypeToJPEG2000();
      }
    else
      {
      itkExceptionMacro(<< "Unknow compression type" );
      }
    }
  gfile->SetUserData( imageData, numberOfBytes);
  if( ! gfile->Write( m_FileName ) )
    {
    itkExceptionMacro(<< "Cannot write the requested file:"
                      << m_FileName
                      << std::endl
                      << "Reason: "
                      << itksys::SystemTools::GetLastSystemError());
    }

  // Clean up
  if( gfile->GetUserData() && gfile->GetUserDataSize()>0 && !m_UseCompression)
  {
     delete[] gfile->GetUserData();
  }

  delete gfile;
  delete header;
}

// Convenience methods to query patient and scanner information. These
// methods are here for compatibility with the DICOMImageIO2 class.
void GDCMImageIO::GetPatientName( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0010|0010", m_PatientName);
  strcpy (name, m_PatientName.c_str());
}

void GDCMImageIO::GetPatientID( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0010|0020", m_PatientID);
  strcpy (name, m_PatientID.c_str());
}

void GDCMImageIO::GetPatientSex( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0010|0040", m_PatientSex);
  strcpy (name, m_PatientSex.c_str());
}

void GDCMImageIO::GetPatientAge( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0010|1010", m_PatientAge);
  strcpy (name, m_PatientAge.c_str());
}

void GDCMImageIO::GetStudyID( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0020|0010", m_StudyID);
  strcpy (name, m_StudyID.c_str());
}

void GDCMImageIO::GetPatientDOB( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0010|0030", m_PatientDOB);
  strcpy (name, m_PatientDOB.c_str());
}

void GDCMImageIO::GetStudyDescription( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0008|1030", m_StudyDescription);
  strcpy (name, m_StudyDescription.c_str());
}

void GDCMImageIO::GetBodyPart( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0018|0015", m_BodyPart);
  strcpy (name, m_BodyPart.c_str());
}

void GDCMImageIO::GetNumberOfSeriesInStudy( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0020|1000", m_NumberOfSeriesInStudy);
  strcpy (name, m_NumberOfSeriesInStudy.c_str());
}

void GDCMImageIO::GetNumberOfStudyRelatedSeries( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0020|1206", m_NumberOfStudyRelatedSeries);
  strcpy (name, m_NumberOfStudyRelatedSeries.c_str());
}

void GDCMImageIO::GetStudyDate( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0008|0020", m_StudyDate);
  strcpy (name, m_StudyDate.c_str());
}

void GDCMImageIO::GetModality( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0008|0060", m_Modality);
  strcpy (name, m_Modality.c_str());
}

void GDCMImageIO::GetManufacturer( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0008|0070", m_Manufacturer);
  strcpy (name, m_Manufacturer.c_str());
}

void GDCMImageIO::GetInstitution( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0008|0080", m_Institution);
  strcpy (name, m_Institution.c_str());
}

void GDCMImageIO::GetModel( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0008|1090", m_Model);
  strcpy (name, m_Model.c_str());
}

void GDCMImageIO::GetScanOptions( char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  ExposeMetaData<std::string>(dict, "0018|0022", m_ScanOptions);
  strcpy (name, m_ScanOptions.c_str());
}

bool GDCMImageIO::GetValueFromTag(const std::string & tag, std::string & value)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  return ExposeMetaData<std::string>(dict, tag, value);
}

bool GDCMImageIO::GetLabelFromTag( const std::string & tagkey,
                                         std::string & labelId )
{
  gdcm::Dict *pubDict = gdcm::Global::GetDicts()->GetDefaultPubDict();

  gdcm::DictEntry *dictentry = pubDict->GetEntry( tagkey );

  bool found;

  // If tagkey was found (ie DICOM tag from public dictionary),
  // then return the name:
  if( dictentry )
    {
    labelId = dictentry->GetName();
    found = true;
    }
  else
    {
    labelId = "Unknown";
    found = false;
    }
  return found;
}


void GDCMImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Internal Component Type: " << this->GetComponentTypeAsString(m_InternalComponentType)
     << std::endl;
  os << indent << "RescaleSlope: " << m_RescaleSlope << std::endl;
  os << indent << "RescaleIntercept: " << m_RescaleIntercept << std::endl;
  os << indent << "MaxSizeLoadEntry: " << m_MaxSizeLoadEntry << std::endl;
  os << indent << "KeepOriginalUID:" << (m_KeepOriginalUID ? "On" : "Off") << std::endl;
  os << indent << "UIDPrefix: " << m_UIDPrefix << std::endl;
  os << indent << "StudyInstanceUID: " << m_StudyInstanceUID << std::endl;
  os << indent << "SeriesInstanceUID: " << m_SeriesInstanceUID << std::endl;
  os << indent << "FrameOfReferenceInstanceUID: " << m_FrameOfReferenceInstanceUID << std::endl;
  os << indent << "LoadSequences:" << m_LoadSequences << std::endl;
  os << indent << "LoadPrivateTags:" << m_LoadPrivateTags << std::endl;
  os << indent << "CompressionType:" << m_CompressionType << std::endl;

  os << indent << "Patient Name:" << m_PatientName << std::endl;
  os << indent << "Patient ID:" << m_PatientID << std::endl;
  os << indent << "Patient Sex:" << m_PatientSex << std::endl;
  os << indent << "Patient Age:" << m_PatientAge << std::endl;
  os << indent << "Study ID:" << m_StudyID << std::endl;
  os << indent << "Patient DOB:" << m_PatientDOB << std::endl;
  os << indent << "Study Description:" << m_StudyDescription << std::endl;
  os << indent << "Body Part:" << m_BodyPart << std::endl;
  os << indent << "Number Of Series In Study:" << m_NumberOfSeriesInStudy << std::endl;
  os << indent << "Number Of Study Related Series:" << m_NumberOfStudyRelatedSeries << std::endl;
  os << indent << "Study Date:" << m_StudyDate << std::endl;
  os << indent << "Modality:" << m_Modality << std::endl;
  os << indent << "Manufacturer:" << m_Manufacturer << std::endl;
  os << indent << "Institution Name:" << m_Institution << std::endl;
  os << indent << "Model:" << m_Model << std::endl;
  os << indent << "Scan Options:" << m_ScanOptions << std::endl;


}


} // end namespace itk
