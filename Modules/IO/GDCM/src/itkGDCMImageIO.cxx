/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/

#include "itkVersion.h"
#include "itkGDCMImageIO.h"
#include "itkIOCommon.h"
#include "itkArray.h"
#include "itkByteSwapper.h"
#include "vnl/vnl_cross.h"

#include "itkMetaDataObject.h"

#include "itksys/SystemTools.hxx"
#include "itksys/Base64.h"

#include "gdcmImageHelper.h"
#include "gdcmFileExplicitFilter.h"
#include "gdcmImageChangeTransferSyntax.h"
#include "gdcmDataSetHelper.h"
#include "gdcmStringFilter.h"
#include "gdcmImageApplyLookupTable.h"
#include "gdcmImageChangePlanarConfiguration.h"
#include "gdcmRescaler.h"
#include "gdcmImageReader.h"
#include "gdcmImageWriter.h"
#include "gdcmUIDGenerator.h"
#include "gdcmAttribute.h"
#include "gdcmGlobal.h"

#include <fstream>

namespace itk
{
class InternalHeader
{
public:
  InternalHeader():m_Header(0) {}
  gdcm::File *m_Header;
};

GDCMImageIO::GDCMImageIO()
{
  this->m_DICOMHeader = new InternalHeader;
  this->SetNumberOfDimensions(3); //needed for getting the 3 coordinates of
                                  // the origin, even if it is a 2D slice.
  m_ByteOrder = LittleEndian;     //default
  m_FileType = Binary;            //default...always true
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

  m_InternalComponentType = UNKNOWNCOMPONENTTYPE;

  // by default assume that images will be 2D.
  // This number is updated according the information
  // received through the MetaDataDictionary
  m_GlobalNumberOfDimensions = 2;
  // By default use JPEG2000. For legacy system, one should prefer JPEG since
  // JPEG2000 was only recently added to the DICOM standard
  m_CompressionType = JPEG2000;
}

GDCMImageIO::~GDCMImageIO()
{
  if ( this->m_DICOMHeader->m_Header )
    {
    delete this->m_DICOMHeader->m_Header;
    }
  delete this->m_DICOMHeader;
}

bool GDCMImageIO::OpenGDCMFileForReading(std::ifstream & os,
                                         const char *filename)
{
  // Make sure that we have a file to
  if ( *filename == 0 )
    {
    itkExceptionMacro(<< "A FileName must be specified.");
    }

  // Close file from any previous image
  if ( os.is_open() )
    {
    os.close();
    }

  // Open the new file for reading
  itkDebugMacro(<< "Initialize: opening file " << filename);

  // Actually open the file
  os.open(filename, std::ios::in | std::ios::binary);

  if ( os.fail() )
    {
    return false;
    }

  return true;
}

bool GDCMImageIO::OpenGDCMFileForWriting(std::ofstream & os,
                                         const char *filename)
{
  // Make sure that we have a file to
  if ( *filename == 0 )
    {
    itkExceptionMacro(<< "A FileName must be specified.");
    }

  // Close file from any previous image
  if ( os.is_open() )
    {
    os.close();
    }

  // Open the new file for writing
  itkDebugMacro(<< "Initialize: opening file " << filename);

  // Actually open the file
  os.open(filename, std::ios::out | std::ios::binary);

  if ( os.fail() )
    {
    itkExceptionMacro( << "Could not open file: "
                       << filename << " for writing."
                       << std::endl
                       << "Reason: "
                       << itksys::SystemTools::GetLastSystemError() );
    }

  return true;
}

// This method will only test if the header looks like a
// GDCM image file.
bool GDCMImageIO::CanReadFile(const char *filename)
{
  std::ifstream file;
  std::string   fname(filename);

  if (  fname == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    return false;
    }

  //Check for file existence:
  if ( !this->OpenGDCMFileForReading(file, filename) )
    {
    return false;
    }
  //
  // sniff for the DICM signature first at 128
  // then at zero, and if either place has it then
  // ask GDCM to try reading it.
  //
  // There isn't a definitive way to check for DICOM files;
  // This was actually cribbed from DICOMParser in VTK
  bool dicomsig(false);
  for(long int off = 128; off >= 0; off -= 128)
    {
    file.seekg(off,std::ios_base::beg);
    if(file.fail() || file.eof())
      {
      return false;
      }
    char buf[5];
    file.read(buf,4);
    if(file.fail())
      {
      return false;
      }
    buf[4] = '\0';
    std::string sig(buf);
    if(sig == "DICM")
      {
      dicomsig = true;
      }
    }
  if(!dicomsig)
    {
    file.seekg(0,std::ios_base::beg);
    unsigned short groupNo;
    file.read(reinterpret_cast<char *>(&groupNo),sizeof(unsigned short));
    ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&groupNo);
    if(groupNo == 0x0002 || groupNo == 0x0008)
      {
      dicomsig = true;
      }
    }
  if(dicomsig)
    {
    // Check to see if its a valid dicom file gdcm is able to parse:
    // We are parsing the header one time here:
    gdcm::ImageReader reader;
    reader.SetFileName(filename);
    if ( reader.Read() )
      {
      return true;
      }
    }
  return false;
}

void GDCMImageIO::Read(void *pointer)
{
  const char *filename = m_FileName.c_str();

  itkAssertInDebugAndIgnoreInReleaseMacro( gdcm::ImageHelper::GetForceRescaleInterceptSlope() );
  gdcm::ImageReader reader;
  reader.SetFileName(filename);
  if ( !reader.Read() )
    {
    itkExceptionMacro(<< "Cannot read requested file");
    return;
    }

  gdcm::Image & image = reader.GetImage();
#ifndef NDEBUG
  gdcm::PixelFormat pixeltype_debug = image.GetPixelFormat();
  itkAssertInDebugAndIgnoreInReleaseMacro(image.GetNumberOfDimensions() == 2 || image.GetNumberOfDimensions() == 3);
#endif
  SizeValueType len = image.GetBufferLength();

  // I think ITK only allow RGB image by pixel (and not by plane)
  if ( image.GetPlanarConfiguration() == 1 )
    {
    gdcm::ImageChangePlanarConfiguration icpc;
    icpc.SetInput(image);
    icpc.SetPlanarConfiguration(0);
    icpc.Change();
    image = icpc.GetOutput();
    }

  if ( image.GetPhotometricInterpretation() == gdcm::PhotometricInterpretation::PALETTE_COLOR )
    {
    gdcm::ImageApplyLookupTable ialut;
    ialut.SetInput(image);
    ialut.Apply();
    image = ialut.GetOutput();
    len *= 3;
    }

  if ( !image.GetBuffer( (char*)pointer ) )
    {
    itkExceptionMacro(<< "Failed to get the buffer!");
    return;
    }

  const gdcm::PixelFormat & pixeltype = image.GetPixelFormat();
  itkAssertInDebugAndIgnoreInReleaseMacro( pixeltype_debug == pixeltype );

  if ( m_RescaleSlope != 1.0 || m_RescaleIntercept != 0.0 )
    {
    gdcm::Rescaler r;
    r.SetIntercept(m_RescaleIntercept);
    r.SetSlope(m_RescaleSlope);
    r.SetPixelFormat(pixeltype);
    gdcm::PixelFormat outputpt = r.ComputeInterceptSlopePixelType();
    char *            copy = new char[len];
    memcpy(copy, (char *)pointer, len);
    r.Rescale( (char *)pointer, copy, len );
    delete[] copy;
    // WARNING: sizeof(Real World Value) != sizeof(Stored Pixel)
    len = len * outputpt.GetPixelSize() / pixeltype.GetPixelSize();
    }

#ifndef NDEBUG
  // \postcondition
  // Now that len was updated (after unpacker 12bits -> 16bits, rescale...) ,
  // can now check compat:
  const SizeValueType numberOfBytesToBeRead =
    static_cast< SizeValueType >( this->GetImageSizeInBytes() );
  itkAssertInDebugAndIgnoreInReleaseMacro(numberOfBytesToBeRead == len);   // programmer error
#endif
}

// TODO: this function was not part of gdcm::Tag API as of gdcm 2.0.10:
static std::string PrintAsPipeSeparatedString(const gdcm::Tag & tag)
{
  std::string ret = tag.PrintAsPipeSeparatedString();
  return ret;
}

void GDCMImageIO::InternalReadImageInformation(std::ifstream & file)
{
  //read header
  if ( !this->OpenGDCMFileForReading( file, m_FileName.c_str() ) )
    {
    itkExceptionMacro(<< "Cannot read requested file");
    }

  // In general this should be relatively safe to assume
  gdcm::ImageHelper::SetForceRescaleInterceptSlope(true);

  const char *      filename = m_FileName.c_str();
  gdcm::ImageReader reader;
  reader.SetFileName(filename);
  if ( !reader.Read() )
    {
    itkExceptionMacro(<< "Cannot read requested file");
    }
  const gdcm::Image &   image = reader.GetImage();
  const gdcm::File &    f = reader.GetFile();
  const gdcm::DataSet & ds = f.GetDataSet();
  const unsigned int *  dims = image.GetDimensions();

  const gdcm::PixelFormat & pixeltype = image.GetPixelFormat();

  m_RescaleIntercept = image.GetIntercept();
  m_RescaleSlope = image.GetSlope();
  gdcm::Rescaler r;
  r.SetIntercept(m_RescaleIntercept);
  r.SetSlope(m_RescaleSlope);
  r.SetPixelFormat(pixeltype);
  gdcm::PixelFormat::ScalarType outputpt = r.ComputeInterceptSlopePixelType();

  itkAssertInDebugAndIgnoreInReleaseMacro(pixeltype <= outputpt);

  m_ComponentType = UNKNOWNCOMPONENTTYPE;
  switch ( outputpt )
    {
    case gdcm::PixelFormat::INT8:
      m_ComponentType = ImageIOBase::CHAR; // Is it signed char ?
      break;
    case gdcm::PixelFormat::UINT8:
      m_ComponentType = ImageIOBase::UCHAR;
      break;
    /* INT12 / UINT12 should not happen anymore in any modern DICOM */
    case gdcm::PixelFormat::INT12:
      m_ComponentType = ImageIOBase::SHORT;
      break;
    case gdcm::PixelFormat::UINT12:
      m_ComponentType = ImageIOBase::USHORT;
      break;
    case gdcm::PixelFormat::INT16:
      m_ComponentType = ImageIOBase::SHORT;
      break;
    case gdcm::PixelFormat::UINT16:
      m_ComponentType = ImageIOBase::USHORT;
      break;
    // RT / SC have 32bits
    case gdcm::PixelFormat::INT32:
      m_ComponentType = ImageIOBase::INT;
      break;
    case gdcm::PixelFormat::UINT32:
      m_ComponentType = ImageIOBase::UINT;
      break;
    //case gdcm::PixelFormat::FLOAT16: // TODO
    case gdcm::PixelFormat::FLOAT32:
      m_ComponentType = ImageIOBase::FLOAT;
      break;
    case gdcm::PixelFormat::FLOAT64:
      m_ComponentType = ImageIOBase::DOUBLE;
      break;
    default:
      itkExceptionMacro("Unhandled PixelFormat: " << outputpt);
    }

  m_NumberOfComponents = pixeltype.GetSamplesPerPixel();
  if ( image.GetPhotometricInterpretation() ==
       gdcm::PhotometricInterpretation::PALETTE_COLOR )
    {
    itkAssertInDebugAndIgnoreInReleaseMacro(m_NumberOfComponents == 1);
    // TODO: need to do the LUT ourself...
    //itkExceptionMacro(<< "PALETTE_COLOR is not implemented yet");
    // AFAIK ITK user don't care about the palette so always apply it and fake a
    // RGB image for them
    m_NumberOfComponents = 3;
    }
  if ( m_NumberOfComponents == 1 )
    {
    this->SetPixelType(SCALAR);
    }
  else
    {
    this->SetPixelType(RGB); // What if image is YBR ? This is a problem since
                             // integer conversion is lossy
    }

  // set values in case we don't find them
  //this->SetNumberOfDimensions(  image.GetNumberOfDimensions() );
  m_Dimensions[0] = dims[0];
  m_Dimensions[1] = dims[1];

  const double *spacing = image.GetSpacing();
  m_Spacing[0] = spacing[0];
  m_Spacing[1] = spacing[1];
  m_Spacing[2] = spacing[2];

  const double *origin = image.GetOrigin();
  m_Origin[0] = origin[0];
  m_Origin[1] = origin[1];
  m_Origin[2] = origin[2];

  if ( image.GetNumberOfDimensions() == 3 )
    {
    m_Dimensions[2] = dims[2];
    }
  else
    {
    m_Dimensions[2] = 1;
    }

  const double *       dircos = image.GetDirectionCosines();
  vnl_vector< double > rowDirection(3), columnDirection(3);
  rowDirection[0] = dircos[0];
  rowDirection[1] = dircos[1];
  rowDirection[2] = dircos[2];
  columnDirection[0] = dircos[3];
  columnDirection[1] = dircos[4];
  columnDirection[2] = dircos[5];

  vnl_vector< double > sliceDirection = vnl_cross_3d(rowDirection, columnDirection);
  this->SetDirection(0, rowDirection);
  this->SetDirection(1, columnDirection);
  this->SetDirection(2, sliceDirection);

  //Now copying the gdcm dictionary to the itk dictionary:
  MetaDataDictionary & dico = this->GetMetaDataDictionary();

  // in the case of re-use by ImageSeriesReader, clear the dictionary
  // before populating it.
  dico.Clear();

  gdcm::StringFilter sf;
  sf.SetFile(f);
  gdcm::DataSet::ConstIterator it = ds.Begin();

  // Copy of the header->content
  for (; it != ds.End(); ++it )
    {
    const gdcm::DataElement & ref = *it;
    const gdcm::Tag &         tag = ref.GetTag();
    // Compute VR from the toplevel file, and the currently processed dataset:
    gdcm::VR vr = gdcm::DataSetHelper::ComputeVR(f, ds, tag);

    // Process binary field and encode them as mime64: only when we do not know
    // of any better
    // representation. VR::US is binary, but user want ASCII representation.
    if ( vr & ( gdcm::VR::OB | gdcm::VR::OF | gdcm::VR::OW | gdcm::VR::SQ | gdcm::VR::UN ) )
      {
      // itkAssertInDebugAndIgnoreInReleaseMacro( vr & gdcm::VR::VRBINARY );
      /*
       * Old behavior was to skip SQ, Pixel Data element. I decided that it is not safe to mime64
       * VR::UN element. There used to be a bug in gdcm 1.2.0 and VR:UN element.
       */
      if ( tag.IsPublic() && vr != gdcm::VR::SQ
           && tag != gdcm::Tag(0x7fe0, 0x0010) /* && vr != gdcm::VR::UN*/ )
        {
        const gdcm::ByteValue *bv = ref.GetByteValue();
        if ( bv )
          {
          // base64 streams have to be a multiple of 4 bytes in length
          int encodedLengthEstimate = 2 * bv->GetLength();
          encodedLengthEstimate = ( ( encodedLengthEstimate / 4 ) + 1 ) * 4;

          char *       bin = new char[encodedLengthEstimate];
          unsigned int encodedLengthActual = static_cast< unsigned int >(
            itksysBase64_Encode(
              (const unsigned char *)bv->GetPointer(),
              static_cast< SizeValueType >( bv->GetLength() ),
              (unsigned char *)bin,
              static_cast< int >( 0 ) ) );
          std::string encodedValue(bin, encodedLengthActual);
          EncapsulateMetaData< std::string >(dico, PrintAsPipeSeparatedString(tag), encodedValue);
          delete[] bin;
          }
        }
      }
    else /* if ( vr & gdcm::VR::VRASCII ) */
      {
      // Only copying field from the public DICOM dictionary
      if ( tag.IsPublic() )
        {
        EncapsulateMetaData< std::string >( dico, PrintAsPipeSeparatedString(tag), sf.ToString(tag) );
        }
      }
    }

#if defined( ITKIO_DEPRECATED_GDCM1_API )
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
#endif
}

void GDCMImageIO::ReadImageInformation()
{
  std::ifstream file;

  this->InternalReadImageInformation(file);
}

bool GDCMImageIO::CanWriteFile(const char *name)
{
  std::string filename = name;

  if (  filename == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    return false;
    }

  std::string::size_type dcmPos = filename.rfind(".dcm");
  if ( ( dcmPos != std::string::npos )
       && ( dcmPos == filename.length() - 4 ) )
    {
    return true;
    }

  dcmPos = filename.rfind(".DCM");
  if ( ( dcmPos != std::string::npos )
       && ( dcmPos == filename.length() - 4 ) )
    {
    return true;
    }

  std::string::size_type dicomPos = filename.rfind(".dicom");
  if ( ( dicomPos != std::string::npos )
       && ( dicomPos == filename.length() - 6 ) )
    {
    return true;
    }

  dicomPos = filename.rfind(".DICOM");
  if ( ( dicomPos != std::string::npos )
       && ( dicomPos == filename.length() - 6 ) )
    {
    return true;
    }

  return false;
}

void GDCMImageIO::WriteImageInformation()
{}

void GDCMImageIO::Write(const void *buffer)
{
  std::ofstream file;

  if ( !this->OpenGDCMFileForWriting( file, m_FileName.c_str() ) )
    {
    return;
    }
  file.close();
  // global static:
  gdcm::UIDGenerator::SetRoot( m_UIDPrefix.c_str() );

  // echo "ITK" | od -b
  gdcm::FileMetaInformation::AppendImplementationClassUID("111.124.113");
  const std::string project_name = std::string("GDCM/ITK ") + itk::Version::GetITKVersion();
  gdcm::FileMetaInformation::SetSourceApplicationEntityTitle( project_name.c_str() );

  gdcm::ImageWriter   writer;
  gdcm::FileMetaInformation &     fmi = writer.GetFile().GetHeader();
  gdcm::DataSet &     header = writer.GetFile().GetDataSet();
  gdcm::Global &      g = gdcm::Global::GetInstance();
  const gdcm::Dicts & dicts = g.GetDicts();
  const gdcm::Dict &  pubdict = dicts.GetPublicDict();

  std::string          value;
  MetaDataDictionary & dict = this->GetMetaDataDictionary();
  gdcm::Tag            tag;
  //Smarter approach using real iterators
  itk::MetaDataDictionary::ConstIterator itr = dict.Begin();
  itk::MetaDataDictionary::ConstIterator end = dict.End();
  gdcm::StringFilter                     sf;
  sf.SetFile( writer.GetFile() );

  while ( itr != end )
    {
    const std::string & key = itr->first; //Needed for bcc32
    ExposeMetaData< std::string >(dict, key, value);

    // Convert DICOM name to DICOM (group,element)
    bool b = tag.ReadFromPipeSeparatedString( key.c_str() );

    // Anything that has been changed in the MetaData Dict will be pushed
    // into the DICOM header:
    if ( b /*tag != gdcm::Tag(0xffff,0xffff)*/ /*dictEntry*/ )
      {
      const gdcm::DictEntry & dictEntry = pubdict.GetDictEntry(tag);
      gdcm::VR::VRType        vrtype = dictEntry.GetVR();
      if ( dictEntry.GetVR() == gdcm::VR::SQ )
        {
        // How did we reach here ?
        }
      else if ( vrtype & ( gdcm::VR::OB | gdcm::VR::OF | gdcm::VR::OW /*|
                                                                        gdcm::VR::SQ*/   | gdcm::VR::UN ) )
        {
        // Custom VR::VRBINARY
        // convert value from Base64
        uint8_t *    bin = new uint8_t[value.size()];
        unsigned int decodedLengthActual = static_cast< unsigned int >(
          itksysBase64_Decode(
            (const unsigned char *)value.c_str(),
            static_cast< SizeValueType >( 0 ),
            (unsigned char *)bin,
            static_cast< SizeValueType >( value.size() ) ) );
        if ( /*tag.GetGroup() != 0 ||*/ tag.GetElement() != 0 ) // ?
          {
          gdcm::DataElement de(tag);
          de.SetByteValue( (char *)bin, decodedLengthActual );
          de.SetVR( dictEntry.GetVR() );
          if ( tag.GetGroup() == 0x2 ) fmi.Insert(de);
          else header.Insert(de);
          }
        delete[] bin;
        }
      else // VRASCII
        {
        // TODO, should we keep:
        // (0028,0106) US/SS 0                                        #2, 1
        // SmallestImagePixelValue
        // (0028,0107) US/SS 4095                                     #2, 1
        // LargestImagePixelValue
        if ( !tag.IsGroupLength() ) // Get rid of group length, they are not
                                    // useful
          {
          gdcm::DataElement de(tag);
          if ( dictEntry.GetVR().IsVRFile() )
            {
            de.SetVR( dictEntry.GetVR() );
            }
#if GDCM_MAJOR_VERSION == 2 && GDCM_MINOR_VERSION <= 12
          // This will not work in the vast majority of cases but to get at
          // least something working in GDCM 2.0.12
          de.SetByteValue( value.c_str(), static_cast<uint32_t>(value.size()) );
#else
          std::string si = sf.FromString( tag, value.c_str(), value.size() );
          de.SetByteValue( si.c_str(), si.size() );
#endif
          if ( tag.GetGroup() == 0x2 ) fmi.Insert(de);
          else header.Insert(de);   //value, tag.GetGroup(), tag.GetElement());
          }
        }
      }
    else
      {
      // This is not a DICOM entry, then check if it is one of the
      // ITK standard ones
      if ( key == ITK_NumberOfDimensions )
        {
        unsigned int numberOfDimensions = 0;
        ExposeMetaData< unsigned int >(dict, key, numberOfDimensions);
        m_GlobalNumberOfDimensions = numberOfDimensions;
        m_Origin.resize(m_GlobalNumberOfDimensions);
        m_Spacing.resize(m_GlobalNumberOfDimensions);
        }
      else if ( key == ITK_Origin )
        {
        typedef Array< double > DoubleArrayType;
        DoubleArrayType originArray;
        ExposeMetaData< DoubleArrayType >(dict, key, originArray);
        m_Origin[0] = originArray[0];
        m_Origin[1] = originArray[1];
        m_Origin[2] = originArray[2];
        }
      else if ( key == ITK_Spacing )
        {
        typedef Array< double > DoubleArrayType;
        DoubleArrayType spacingArray;
        ExposeMetaData< DoubleArrayType >(dict, key, spacingArray);
        m_Spacing[0] = spacingArray[0];
        m_Spacing[1] = spacingArray[1];
        m_Spacing[2] = spacingArray[2];
        }
      else
        {
        itkDebugMacro(
          << "GDCMImageIO: non-DICOM and non-ITK standard key = " << key);
        }
      }

    ++itr;
    }
  //std::cout << header << std::endl;

  //this->SetNumberOfDimensions(3);
  //gdcm::Image &image = writer.GetImage();
  gdcm::SmartPointer< gdcm::Image > simage = new gdcm::Image;
  gdcm::Image &                     image = *simage;
  image.SetNumberOfDimensions(2);   // good default
  image.SetDimension(0, static_cast<unsigned int>(m_Dimensions[0]));
  image.SetDimension(1, static_cast<unsigned int>(m_Dimensions[1]));
  //image.SetDimension(2, m_Dimensions[2] );
  image.SetSpacing(0, m_Spacing[0]);
  image.SetSpacing(1, m_Spacing[1]);
  if ( m_NumberOfDimensions > 2 && m_Dimensions[2] != 1 )
    {
    image.SetSpacing(2, m_Spacing[2]);
    }
  // Set the origin (image position patient)
  // If the meta dictionary contains the tag "0020 0032", use it
  std::string tempString;
  const bool hasIPP = ExposeMetaData<std::string>(dict,"0020|0032",tempString);
  if( hasIPP)
    {
    double origin3D[3];
    sscanf(tempString.c_str(), "%lf\\%lf\\%lf", &(origin3D[0]), &(origin3D[1]), &(origin3D[2]) );
    image.SetOrigin(0, origin3D[0]);
    image.SetOrigin(1, origin3D[1]);
    image.SetOrigin(2, origin3D[2]);
    }
  else
    {
    image.SetOrigin(0, m_Origin[0]);
    image.SetOrigin(1, m_Origin[1]);
    if ( m_Origin.size() == 3 )
      {
      image.SetOrigin(2, m_Origin[2]);
      }
    else
      {
      image.SetOrigin(2, 0);
      }
    }
  if ( m_NumberOfDimensions > 2 && m_Dimensions[2] != 1 )
    {
    // resize num of dim to 3:
    image.SetNumberOfDimensions(3);
    image.SetDimension(2, static_cast<unsigned int>(m_Dimensions[2]));
    }

  // Do the direction now:
  image.SetDirectionCosines(0, m_Direction[0][0]);
  image.SetDirectionCosines(1, m_Direction[0][1]);
  if ( m_Direction.size() == 3 )
    {
    image.SetDirectionCosines(2, m_Direction[0][2]);
    }
  else
    {
    image.SetDirectionCosines(2, 0);
    }
  image.SetDirectionCosines(3, m_Direction[1][0]);
  image.SetDirectionCosines(4, m_Direction[1][1]);
  if ( m_Direction.size() == 3 )
    {
    image.SetDirectionCosines(5, m_Direction[1][2]);
    }
  else
    {
    image.SetDirectionCosines(5, 0);
    }

  // reset any previous value:
  m_RescaleSlope = 1.0;
  m_RescaleIntercept = 0.0;

  // Get user defined rescale slope/intercept
  std::string rescaleintercept;
  ExposeMetaData< std::string >(dict, "0028|1052", rescaleintercept);
  std::string rescaleslope;
  ExposeMetaData< std::string >(dict, "0028|1053", rescaleslope);
  if ( rescaleintercept != "" && rescaleslope != "" )
    {
    itksys_ios::stringstream sstr1;
    sstr1 << rescaleintercept;
    if ( !( sstr1 >> m_RescaleIntercept ) )
      {
      itkExceptionMacro("Problem reading RescaleIntercept: " << rescaleintercept);
      }
    itksys_ios::stringstream sstr2;
    sstr2 << rescaleslope;
    if ( !( sstr2 >> m_RescaleSlope ) )
      {
      itkExceptionMacro("Problem reading RescaleSlope: " << rescaleslope);
      }
    // header->InsertValEntry( "US", 0x0028, 0x1054 ); // Rescale Type
    }
  else if ( rescaleintercept != "" || rescaleslope != "" ) // xor
    {
    itkExceptionMacro("Both RescaleSlope & RescaleIntercept need to be present");
    }

  // Handle the bitDepth:
  std::string bitsAllocated;
  std::string bitsStored;
  std::string highBit;
  std::string pixelRep;
  // Get user defined bit representation:
  ExposeMetaData< std::string >(dict, "0028|0100", bitsAllocated);
  ExposeMetaData< std::string >(dict, "0028|0101", bitsStored);
  ExposeMetaData< std::string >(dict, "0028|0102", highBit);
  ExposeMetaData< std::string >(dict, "0028|0103", pixelRep);

  gdcm::PixelFormat pixeltype = gdcm::PixelFormat::UNKNOWN;
  switch ( this->GetComponentType() )
    {
    case ImageIOBase::CHAR:
      pixeltype = gdcm::PixelFormat::INT8;
      break;
    case ImageIOBase::UCHAR:
      pixeltype = gdcm::PixelFormat::UINT8;
      break;
    case ImageIOBase::SHORT:
      pixeltype = gdcm::PixelFormat::INT16;
      break;
    case ImageIOBase::USHORT:
      pixeltype = gdcm::PixelFormat::UINT16;
      break;
    case ImageIOBase::INT:
      pixeltype = gdcm::PixelFormat::INT32;
      break;
    case ImageIOBase::UINT:
      pixeltype = gdcm::PixelFormat::UINT32;
      break;
    //Disabling FLOAT and DOUBLE for now...
    case ImageIOBase::FLOAT:
      pixeltype = gdcm::PixelFormat::FLOAT32;
      break;
    case ImageIOBase::DOUBLE:
      pixeltype = gdcm::PixelFormat::FLOAT64;
      break;
    default:
      itkExceptionMacro(<< "DICOM does not support this component type");
    }
  itkAssertInDebugAndIgnoreInReleaseMacro(pixeltype != gdcm::PixelFormat::UNKNOWN);
  gdcm::PhotometricInterpretation pi;
  if ( this->GetNumberOfComponents() == 1 )
    {
    pi = gdcm::PhotometricInterpretation::MONOCHROME2;
    }
  else if ( this->GetNumberOfComponents() == 3 )
    {
    pi = gdcm::PhotometricInterpretation::RGB;
    // (0028,0006) US 0                                        #2, 1
    // PlanarConfiguration
    }
  else
    {
    itkExceptionMacro(<< "DICOM does not support this component type");
    }
  pixeltype.SetSamplesPerPixel( static_cast<short unsigned int>( this->GetNumberOfComponents() ) );

  // Compute the outpixeltype
  gdcm::PixelFormat outpixeltype = gdcm::PixelFormat::UNKNOWN;
  if ( pixeltype == gdcm::PixelFormat::FLOAT32 || pixeltype == gdcm::PixelFormat::FLOAT64 )
    {
    if ( bitsAllocated != "" && bitsStored != "" && highBit != "" && pixelRep != "" )
      {
      outpixeltype.SetBitsAllocated( static_cast<unsigned short int>(atoi( bitsAllocated.c_str() ) ));
      outpixeltype.SetBitsStored( static_cast<unsigned short int>(atoi( bitsStored.c_str() )) );
      outpixeltype.SetHighBit( static_cast<unsigned short int>(atoi( highBit.c_str()) ) );
      outpixeltype.SetPixelRepresentation( static_cast<unsigned short int>(atoi( pixelRep.c_str() )) );
      if ( this->GetNumberOfComponents() != 1 )
        {
        itkExceptionMacro(<< "Sorry Dave I can't do that");
        }
      itkAssertInDebugAndIgnoreInReleaseMacro(outpixeltype != gdcm::PixelFormat::UNKNOWN);
      }
    else
      {
      itkExceptionMacro(<< "A Floating point buffer was passed but the stored pixel type was not specified."
                           "This is currently not supported");
      }
    }

  image.SetPhotometricInterpretation(pi);
  if ( outpixeltype != gdcm::PixelFormat::UNKNOWN )
    {
    image.SetPixelFormat(outpixeltype);
    }
  else
    {
    image.SetPixelFormat(pixeltype);
    }
  SizeValueType len = image.GetBufferLength();

  size_t numberOfBytes = this->GetImageSizeInBytes();

  gdcm::DataElement pixeldata( gdcm::Tag(0x7fe0, 0x0010) );
  // Handle rescaler here:
  // Whenever shift / scale is needed... do it !
  if( outpixeltype != gdcm::PixelFormat::UNKNOWN )
    {
    itkAssertInDebugAndIgnoreInReleaseMacro( m_RescaleIntercept != 0 || m_RescaleSlope != 1 );
    // rescale from float to unsigned short
    gdcm::Rescaler ir;
    ir.SetIntercept(m_RescaleIntercept);
    ir.SetSlope(m_RescaleSlope);
    ir.SetPixelFormat(pixeltype);
    ir.SetMinMaxForPixelType( static_cast<double>(outpixeltype.GetMin()), static_cast<double>(outpixeltype.GetMax()) );
    image.SetIntercept(m_RescaleIntercept);
    image.SetSlope(m_RescaleSlope);
    char *copy = new char[len];
    ir.InverseRescale(copy, (char *)buffer, numberOfBytes);
    pixeldata.SetByteValue(copy, static_cast<uint32_t>(len));
    delete[] copy;
    }
  else
    {
    itkAssertInDebugAndIgnoreInReleaseMacro(len == numberOfBytes);
    // only do a straight copy:
    pixeldata.SetByteValue( (char *)buffer, static_cast<unsigned int>(numberOfBytes) );
    }
  image.SetDataElement(pixeldata);

  // Handle compression here:
  // If user ask to use compression:
  if ( m_UseCompression )
    {
    gdcm::ImageChangeTransferSyntax change;
    if ( m_CompressionType == JPEG )
      {
      change.SetTransferSyntax(gdcm::TransferSyntax::JPEGLosslessProcess14_1);
      }
    else if ( m_CompressionType == JPEG2000 )
      {
      change.SetTransferSyntax(gdcm::TransferSyntax::JPEG2000Lossless);
      }
    else
      {
      itkExceptionMacro(<< "Unknown compression type");
      }
    change.SetInput(image);
    bool b = change.Change();
    if ( !b )
      {
      itkExceptionMacro(<< "Could not change the Transfer Syntax for Compression");
      }
    writer.SetImage( change.GetOutput() );
    }
  else
    {
    writer.SetImage(image);
    }

  if ( !m_KeepOriginalUID )
    {
    // UID generation part:
    // We only create *ONE* Study/Series.Frame of Reference Instance UID
    if ( m_StudyInstanceUID.empty() )
      {
      // As long as user maintain there gdcmIO they will keep the same
      // Study/Series instance UID.
      gdcm::UIDGenerator uid;
      m_StudyInstanceUID = uid.Generate();
      m_SeriesInstanceUID = uid.Generate();
      m_FrameOfReferenceInstanceUID = uid.Generate();
      }
    //std::string uid = uid.Generate();
    const char *studyuid = m_StudyInstanceUID.c_str();
      {
      gdcm::DataElement de( gdcm::Tag(0x0020, 0x000d) ); // Study
      de.SetByteValue( studyuid, static_cast<unsigned int>(strlen(studyuid)) );
      de.SetVR( gdcm::Attribute< 0x0020, 0x000d >::GetVR() );
      header.Insert(de);
      }
    const char *seriesuid = m_SeriesInstanceUID.c_str();
      {
      gdcm::DataElement de( gdcm::Tag(0x0020, 0x000e) ); // Series
      de.SetByteValue( seriesuid, static_cast<unsigned int>(strlen(seriesuid)) );
      de.SetVR( gdcm::Attribute< 0x0020, 0x000e >::GetVR() );
      header.Insert(de);
      }
    const char *frameofreferenceuid = m_FrameOfReferenceInstanceUID.c_str();
      {
      gdcm::DataElement de( gdcm::Tag(0x0020, 0x0052) ); // Frame of Reference
      de.SetByteValue( frameofreferenceuid, static_cast<unsigned int>(strlen( frameofreferenceuid)) );
      de.SetVR( gdcm::Attribute< 0x0020, 0x0052 >::GetVR() );
      header.Insert(de);
      }
    }

  if ( image.GetTransferSyntax() != gdcm::TransferSyntax::ImplicitVRLittleEndian )
    {
    gdcm::FileExplicitFilter fef;
    //fef.SetChangePrivateTags( true );
    fef.SetFile( writer.GetFile() );
    if ( !fef.Change() )
      {
      itkExceptionMacro(<< "Failed to change to Explicit Transfer Syntax");
      }
    }

  const char *filename = m_FileName.c_str();
  writer.SetFileName(filename);
  if ( !writer.Write() )
    {
    itkExceptionMacro(<< "DICOM does not support this component type");
    }
}

#if defined( ITKIO_DEPRECATED_GDCM1_API )
// Convenience methods to query patient and scanner information. These
// methods are here for compatibility with the DICOMImageIO2 class.
void GDCMImageIO::GetPatientName(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0010|0010", m_PatientName);
  strcpy ( name, m_PatientName.c_str() );
}

void GDCMImageIO::GetPatientID(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0010|0020", m_PatientID);
  strcpy ( name, m_PatientID.c_str() );
}

void GDCMImageIO::GetPatientSex(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0010|0040", m_PatientSex);
  strcpy ( name, m_PatientSex.c_str() );
}

void GDCMImageIO::GetPatientAge(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0010|1010", m_PatientAge);
  strcpy ( name, m_PatientAge.c_str() );
}

void GDCMImageIO::GetStudyID(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0020|0010", m_StudyID);
  strcpy ( name, m_StudyID.c_str() );
}

void GDCMImageIO::GetPatientDOB(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0010|0030", m_PatientDOB);
  strcpy ( name, m_PatientDOB.c_str() );
}

void GDCMImageIO::GetStudyDescription(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0008|1030", m_StudyDescription);
  strcpy ( name, m_StudyDescription.c_str() );
}

void GDCMImageIO::GetBodyPart(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0018|0015", m_BodyPart);
  strcpy ( name, m_BodyPart.c_str() );
}

void GDCMImageIO::GetNumberOfSeriesInStudy(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0020|1000", m_NumberOfSeriesInStudy);
  strcpy ( name, m_NumberOfSeriesInStudy.c_str() );
}

void GDCMImageIO::GetNumberOfStudyRelatedSeries(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0020|1206", m_NumberOfStudyRelatedSeries);
  strcpy ( name, m_NumberOfStudyRelatedSeries.c_str() );
}

void GDCMImageIO::GetStudyDate(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0008|0020", m_StudyDate);
  strcpy ( name, m_StudyDate.c_str() );
}

void GDCMImageIO::GetModality(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0008|0060", m_Modality);
  strcpy ( name, m_Modality.c_str() );
}

void GDCMImageIO::GetManufacturer(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0008|0070", m_Manufacturer);
  strcpy ( name, m_Manufacturer.c_str() );
}

void GDCMImageIO::GetInstitution(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0008|0080", m_Institution);
  strcpy ( name, m_Institution.c_str() );
}

void GDCMImageIO::GetModel(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0008|1090", m_Model);
  strcpy ( name, m_Model.c_str() );
}

void GDCMImageIO::GetScanOptions(char *name)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  ExposeMetaData< std::string >(dict, "0018|0022", m_ScanOptions);
  strcpy ( name, m_ScanOptions.c_str() );
}
#endif

bool GDCMImageIO::GetValueFromTag(const std::string & tag, std::string & value)
{
  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  return ExposeMetaData< std::string >(dict, tag, value);
}

bool GDCMImageIO::GetLabelFromTag(const std::string & tag,
                                  std::string & labelId)
{
  gdcm::Tag t;

  if ( t.ReadFromPipeSeparatedString( tag.c_str() ) && t.IsPublic() )
    {
    const gdcm::Global &    g = gdcm::Global::GetInstance();
    const gdcm::Dicts &     dicts = g.GetDicts();
    const gdcm::DictEntry & entry = dicts.GetDictEntry(t);
    labelId = entry.GetName();
    return true;
    }
  return false;
}

void GDCMImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Internal Component Type: " << this->GetComponentTypeAsString(m_InternalComponentType)
     << std::endl;
  os << indent << "RescaleSlope: " << m_RescaleSlope << std::endl;
  os << indent << "RescaleIntercept: " << m_RescaleIntercept << std::endl;
  os << indent << "KeepOriginalUID:" << ( m_KeepOriginalUID ? "On" : "Off" ) << std::endl;
  os << indent << "UIDPrefix: " << m_UIDPrefix << std::endl;
  os << indent << "StudyInstanceUID: " << m_StudyInstanceUID << std::endl;
  os << indent << "SeriesInstanceUID: " << m_SeriesInstanceUID << std::endl;
  os << indent << "FrameOfReferenceInstanceUID: " << m_FrameOfReferenceInstanceUID << std::endl;
  os << indent << "CompressionType:" << m_CompressionType << std::endl;

#if defined( ITKIO_DEPRECATED_GDCM1_API )
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
#endif
}
} // end namespace itk
