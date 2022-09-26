/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkDCMTKImageIO.h"

#include "itkByteSwapper.h"
#include "itksys/SystemTools.hxx"
#include "itkDCMTKFileReader.h"
#include <iostream>
#include "vnl/vnl_cross.h"
#include "itkMath.h"

#include "dcmtk/dcmimgle/dcmimage.h"
#include "dcmtk/dcmjpeg/djdecode.h"
#include "dcmtk/dcmjpls/djdecode.h"
#include "dcmtk/dcmdata/dcrledrg.h"
#include "dcmtk/oflog/oflog.h"

namespace itk
{
/** Constructor */
DCMTKImageIO::DCMTKImageIO()
{
  m_DImage = nullptr;

  // standard ImageIOBase variables
  m_ByteOrder = IOByteOrderEnum::BigEndian;
  this->SetNumberOfDimensions(3); // otherwise, things go crazy w/dir cosines
  m_PixelType = IOPixelEnum::SCALAR;
  m_ComponentType = IOComponentEnum::UCHAR;
  // m_FileType =

  // specific members
  m_UseJPEGCodec = false;
  m_UseJPLSCodec = false;
  m_UseRLECodec = false;
  m_DicomImageSetByUser = false;


  const char * readExtensions[] = { ".dcm", ".DCM", ".dicom", ".DICOM" };


  for (auto ext : readExtensions)
  {
    this->AddSupportedReadExtension(ext);
  }

  // DCMTK loves printing warnings, turn off by default.
  this->SetLogLevel(LogLevelEnum::FATAL_LOG_LEVEL);
}

void
DCMTKImageIO::SetLogLevel(LogLevelEnum level)
{
  switch (level)
  {
    case LogLevelEnum::TRACE_LOG_LEVEL:
      OFLog::configure(OFLogger::TRACE_LOG_LEVEL);
      break;
    case LogLevelEnum::DEBUG_LOG_LEVEL:
      OFLog::configure(OFLogger::DEBUG_LOG_LEVEL);
      break;
    case LogLevelEnum::INFO_LOG_LEVEL:
      OFLog::configure(OFLogger::INFO_LOG_LEVEL);
      break;
    case LogLevelEnum::WARN_LOG_LEVEL:
      OFLog::configure(OFLogger::WARN_LOG_LEVEL);
      break;
    case LogLevelEnum::ERROR_LOG_LEVEL:
      OFLog::configure(OFLogger::ERROR_LOG_LEVEL);
      break;
    case LogLevelEnum::FATAL_LOG_LEVEL:
      OFLog::configure(OFLogger::FATAL_LOG_LEVEL);
      break;
    case LogLevelEnum::OFF_LOG_LEVEL:
      OFLog::configure(OFLogger::OFF_LOG_LEVEL);
      break;
    default:
      itkExceptionMacro(<< "Unknown DCMTK Logging constant " << static_cast<int>(level));
  }
}

DCMTKImageIO::LogLevelEnum
DCMTKImageIO::GetLogLevel() const
{
  dcmtk::log4cplus::Logger rootLogger = dcmtk::log4cplus::Logger::getRoot();
  switch (rootLogger.getLogLevel())
  {
    case OFLogger::TRACE_LOG_LEVEL:
      return LogLevelEnum::TRACE_LOG_LEVEL;
    case OFLogger::DEBUG_LOG_LEVEL:
      return LogLevelEnum::DEBUG_LOG_LEVEL;
    case OFLogger::INFO_LOG_LEVEL:
      return LogLevelEnum::INFO_LOG_LEVEL;
    case OFLogger::WARN_LOG_LEVEL:
      return LogLevelEnum::WARN_LOG_LEVEL;
    case OFLogger::ERROR_LOG_LEVEL:
      return LogLevelEnum::ERROR_LOG_LEVEL;
    case OFLogger::FATAL_LOG_LEVEL:
      return LogLevelEnum::FATAL_LOG_LEVEL;
    case OFLogger::OFF_LOG_LEVEL:
      return LogLevelEnum::OFF_LOG_LEVEL;
  }
  // will never happen
  return LogLevelEnum::FATAL_LOG_LEVEL;
}

/** Destructor */
DCMTKImageIO::~DCMTKImageIO()
{
  if (!this->m_DicomImageSetByUser)
  {
    delete m_DImage;
  }
  DJDecoderRegistration::cleanup();
  DcmRLEDecoderRegistration::cleanup();
}

/**
 * Helper function to test for some dicom like formatting.
 * @param file A stream to test if the file is dicom like
 * @return true if the structure of the file is dicom like
 */
static bool
readNoPreambleDicom(std::ifstream & file) // NOTE: This file is duplicated in itkGDCMImageIO.cxx
{
  // Adapted from https://stackoverflow.com/questions/2381983/c-how-to-read-parts-of-a-file-dicom
  /* This heuristic tries to determine if the file follows the basic structure of a dicom file organization.
   * Any file that begins with the a byte sequence
   * where groupNo matches below will be then read several SOP Instance sections.
   */

  unsigned short groupNo = 0xFFFF;
  unsigned short tagElementNo = 0xFFFF;
  do
  {
    file.read(reinterpret_cast<char *>(&groupNo), sizeof(unsigned short));
    ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&groupNo);
    file.read(reinterpret_cast<char *>(&tagElementNo), sizeof(unsigned short));
    ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&tagElementNo);

    if (groupNo != 0x0002 && groupNo != 0x0008) // Only groupNo 2 & 8 are supported without preambles
    {
      return false;
    }

    char vrcode[3] = { '\0', '\0', '\0' };
    file.read(vrcode, 2);

    long              length = std::numeric_limits<long>::max();
    const std::string vr{ vrcode };
    if (vr == "AE" || vr == "AS" || vr == "AT" || vr == "CS" || vr == "DA" || vr == "DS" || vr == "DT" || vr == "FL" ||
        vr == "FD" || vr == "IS" || vr == "LO" || vr == "PN" || vr == "SH" || vr == "SL" || vr == "SS" || vr == "ST" ||
        vr == "TM" || vr == "UI" || vr == "UL" || vr == "US")
    {
      // Explicit VR (value representation stored in the file)
      unsigned short uslength = 0;
      file.read(reinterpret_cast<char *>(&uslength), sizeof(unsigned short));
      ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&uslength);
      length = uslength;
    }
    else
    {
      // Implicit VR (value representation not stored in the file)
      char lengthChars[4] = { vrcode[0], vrcode[1], '\0', '\0' };
      file.read(lengthChars + 2, 2);

      auto * uilength = reinterpret_cast<unsigned int *>(lengthChars);
      ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian(uilength);

      length = (*uilength);
    }
    if (length <= 0)
    {
      return false;
    }
    file.ignore(length);
    if (file.eof())
    {
      return false;
    }
  } while (groupNo == 2);

#if defined(NDEBUG)
  std::ostringstream itkmsg;
  itkmsg << "No DICOM magic number found, but the file appears to be DICOM without a preamble.\n"
         << "Proceeding without caution.";
  itk::OutputWindowDisplayDebugText(itkmsg.str().c_str());
#endif
  return true;
}


bool
DCMTKImageIO::CanReadFile(const char * filename)
{
  // First check the filename extension
  std::string fname = filename;

  if (fname.empty())
  {
    itkDebugMacro(<< "No filename specified.");
  }

#if !defined(__EMSCRIPTEN__)
  {
    std::ifstream file;
    try
    {
      this->OpenFileForReading(file, filename);
    }
    catch (const ExceptionObject &)
    {
      return false;
    }
    const bool hasdicomsig = readNoPreambleDicom(file);
    file.close();
    if (!hasdicomsig)
    {
      return false;
    }
  }
#endif
  return DCMTKFileReader::IsImageFile(filename);
}

bool
DCMTKImageIO::CanWriteFile(const char * itkNotUsed(name))
{
  // writing is currently not implemented
  return false;
}

void
DCMTKImageIO::OpenDicomImage()
{
  if (this->m_DImage != nullptr)
  {
    if (!this->m_DicomImageSetByUser && this->m_FileName != this->m_LastFileName)
    {
      delete m_DImage;
      this->m_DImage = nullptr;
    }
  }
  if (m_DImage == nullptr)
  {
    m_DImage = new DicomImage(m_FileName.c_str());
    this->m_LastFileName = this->m_FileName;
  }
  if (this->m_DImage == nullptr)
  {
    itkExceptionMacro(<< "Can't create DicomImage for " << this->m_FileName);
  }
}


//------------------------------------------------------------------------------
void
DCMTKImageIO::Read(void * buffer)
{
  this->OpenDicomImage();
  if (m_DImage->getStatus() != EIS_Normal)
  {
    itkExceptionMacro(<< "Error: cannot load DICOM image (" << DicomImage::getString(m_DImage->getStatus()) << ")");
  }

  m_Dimensions[0] = static_cast<unsigned int>(m_DImage->getWidth());
  m_Dimensions[1] = static_cast<unsigned int>(m_DImage->getHeight());

  switch (this->m_ComponentType)
  {
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
    case IOComponentEnum::FLOAT:
    case IOComponentEnum::DOUBLE:
      itkExceptionMacro(<< "Bad component type" << ImageIOBase::GetComponentTypeAsString(this->m_ComponentType));
      break;
    default: // scalarSize already set
      break;
  }
  // get the image in the DCMTK buffer
  const DiPixel * const interData = m_DImage->getInterData();
  const void *          data = interData->getData();
  size_t                count = interData->getCount();
  if (this->m_PixelType == IOPixelEnum::RGB || this->m_PixelType == IOPixelEnum::RGBA)
  {
    ReorderRGBValues(buffer, data, count, this->GetNumberOfComponents());
  }
  else
  {
    memcpy(buffer, data, count * this->GetComponentSize() * this->GetNumberOfComponents());
  }
}

void
DCMTKImageIO::ReorderRGBValues(void * buffer, const void * data, size_t count, unsigned int voxel_size)
{
  switch (this->m_ComponentType)
  {
    // DCMTK only supports unsigned integer types for RGB(A) images.
    // see DCMTK file dcmimage/libsrc/dicoimg.cc (function const void *DiColorImage::getData(...) )
    // DCMTK only supports uint8_t, uint16_t, and uint32_t, but we leave LONG (at least 32bits but
    // could be 64bits) for future support.
    case IOComponentEnum::UCHAR:
      ReorderRGBValues<unsigned char>(buffer, data, count, voxel_size);
      break;
    case IOComponentEnum::USHORT:
      ReorderRGBValues<unsigned short>(buffer, data, count, voxel_size);
      break;
    case IOComponentEnum::UINT:
      ReorderRGBValues<unsigned int>(buffer, data, count, voxel_size);
      break;
    case IOComponentEnum::ULONG:
      ReorderRGBValues<unsigned long>(buffer, data, count, voxel_size);
      break;
    default:
      itkExceptionMacro(<< "Only unsigned integer pixel types are supported. Bad component type for color image"
                        << ImageIOBase::GetComponentTypeAsString(this->m_ComponentType));
      break;
  }
}
/**
 *  Read Information about the DICOM file
 */
void
DCMTKImageIO::ReadImageInformation()
{

  DJDecoderRegistration::registerCodecs();
  DcmRLEDecoderRegistration::registerCodecs();

  DCMTKFileReader reader;
  reader.SetFileName(this->m_FileName);
  try
  {
    reader.LoadFile();
  }
  catch (...)
  {
    std::cerr << "DCMTKImageIO::ReadImageInformation: "
              << "DicomImage could not read the file." << std::endl;
  }

  // check for multiframe > 3D
  itk::int32_t numPhases;
  unsigned int numDim(3);

  if (reader.GetElementSL(0x2001, 0x1017, numPhases, false) != EXIT_SUCCESS)
  {
    numPhases = 1;
  }
  if (numPhases > 1)
  {
    this->SetNumberOfDimensions(4);
    numDim = 4;
  }

  unsigned short rows, columns;
  reader.GetDimensions(rows, columns);
  this->m_Dimensions[0] = columns;
  this->m_Dimensions[1] = rows;
  if (numPhases == 1)
  {
    this->m_Dimensions[2] = reader.GetFrameCount();
  }
  else
  {
    this->m_Dimensions[2] = reader.GetFrameCount() / numPhases;
    this->m_Dimensions[3] = numPhases;
  }
  vnl_vector<double> rowDirection(3);
  vnl_vector<double> columnDirection(3);
  vnl_vector<double> sliceDirection(3);

  rowDirection.fill(0.0);
  columnDirection.fill(0.0);
  sliceDirection.fill(0.0);
  rowDirection[0] = 1.0;
  columnDirection[1] = 1.0;
  sliceDirection[2] = 1.0;

  reader.GetDirCosines(rowDirection, columnDirection, sliceDirection);
  // orthogonalize
  sliceDirection.normalize();
  rowDirection = vnl_cross_3d(columnDirection, sliceDirection).normalize();
  columnDirection.normalize();

  if (numDim < 4)
  {
    this->SetDirection(0, rowDirection);
    this->SetDirection(1, columnDirection);
    if (this->m_NumberOfDimensions > 2)
    {
      this->SetDirection(2, sliceDirection);
    }
  }
  else
  {
    vnl_vector<double> rowDirection4(4), columnDirection4(4), sliceDirection4(4), phaseDirection4(4);
    for (unsigned int i = 0; i < 3; ++i)
    {
      rowDirection4[i] = rowDirection[i];
      columnDirection4[i] = columnDirection[i];
      sliceDirection4[i] = sliceDirection[i];
      phaseDirection4[i] = 0.0;
    }
    rowDirection4[3] = 0.0;
    columnDirection4[3] = 0.0;
    sliceDirection4[3] = 0.0;
    phaseDirection4[3] = 1.0;
    this->SetDirection(0, rowDirection4);
    this->SetDirection(1, columnDirection4);
    this->SetDirection(2, sliceDirection4);
    this->SetDirection(3, phaseDirection4);
  }

  // get slope and intercept
  reader.GetSlopeIntercept(this->m_RescaleSlope, this->m_RescaleIntercept);

  double spacing[3];
  double origin[3];
  reader.GetSpacing(spacing);
  reader.GetOrigin(origin);
  this->m_Origin.resize(numDim);

  for (unsigned int i = 0; i < 3; ++i)
  {
    this->m_Origin[i] = origin[i];
  }

  this->m_Spacing.clear();
  for (double i : spacing)
  {
    this->m_Spacing.push_back(i);
  }
  if (numDim == 4)
  {
    this->m_Origin[3] = 0.0;
    this->m_Spacing.push_back(1.0);
  }


  this->OpenDicomImage();
  const DiPixel * interData = this->m_DImage->getInterData();

  if (interData == nullptr)
  {
    itkExceptionMacro(<< "Missing Image Data in " << this->m_FileName);
  }

  EP_Representation pixelRep = this->m_DImage->getInterData()->getRepresentation();
  switch (pixelRep)
  {
    case EPR_Uint8:
      this->m_ComponentType = IOComponentEnum::UCHAR;
      break;
    case EPR_Sint8:
      this->m_ComponentType = IOComponentEnum::CHAR;
      break;
    case EPR_Uint16:
      this->m_ComponentType = IOComponentEnum::USHORT;
      break;
    case EPR_Sint16:
      this->m_ComponentType = IOComponentEnum::SHORT;
      break;
    case EPR_Uint32:
      this->m_ComponentType = IOComponentEnum::UINT;
      break;
    case EPR_Sint32:
      this->m_ComponentType = IOComponentEnum::INT;
      break;
    default: // HACK should throw exception
      this->m_ComponentType = IOComponentEnum::USHORT;
      break;
  }
  int numPlanes = this->m_DImage->getInterData()->getPlanes();
  switch (numPlanes)
  {
    case 1:
      this->m_PixelType = IOPixelEnum::SCALAR;
      break;
    case 2:
      // hack, supposedly Luminence/Alpha
      this->SetNumberOfComponents(2);
      this->m_PixelType = IOPixelEnum::VECTOR;
      break;
    case 3:
      this->SetNumberOfComponents(3);
      this->m_PixelType = IOPixelEnum::RGB;
      break;
    case 4:
      this->SetNumberOfComponents(4);
      this->m_PixelType = IOPixelEnum::RGBA;
      break;
  }
}

void
DCMTKImageIO::WriteImageInformation()
{}

/** */
void
DCMTKImageIO::Write(const void * buffer)
{
  (void)(buffer);
}

/** Print Self Method */
void
DCMTKImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const DCMTKImageIOEnums::LogLevel value)
{
  return out << [value] {
    switch (value)
    {
      case DCMTKImageIOEnums::LogLevel::TRACE_LOG_LEVEL:
        return "itk::DCMTKImageIOEnums::LogLevel::TRACE_LOG_LEVEL";
      case DCMTKImageIOEnums::LogLevel::DEBUG_LOG_LEVEL:
        return "itk::DCMTKImageIOEnums::LogLevel::DEBUG_LOG_LEVEL";
      case DCMTKImageIOEnums::LogLevel::INFO_LOG_LEVEL:
        return "itk::DCMTKImageIOEnums::LogLevel::INFO_LOG_LEVEL";
      case DCMTKImageIOEnums::LogLevel::WARN_LOG_LEVEL:
        return "itk::DCMTKImageIOEnums::LogLevel::WARN_LOG_LEVEL";
      case DCMTKImageIOEnums::LogLevel::ERROR_LOG_LEVEL:
        return "itk::DCMTKImageIOEnums::LogLevel::ERROR_LOG_LEVEL";
      case DCMTKImageIOEnums::LogLevel::FATAL_LOG_LEVEL:
        return "itk::DCMTKImageIOEnums::LogLevel::FATAL_LOG_LEVEL";
      case DCMTKImageIOEnums::LogLevel::OFF_LOG_LEVEL:
        return "itk::DCMTKImageIOEnums::LogLevel::OFF_LOG_LEVEL";
      default:
        return "INVALID VALUE FOR itk::DCMTKImageIOEnums::LogLevel";
    }
  }();
}
} // end namespace itk
