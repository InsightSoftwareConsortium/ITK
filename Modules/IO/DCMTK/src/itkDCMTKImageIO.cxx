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
  m_DImage = ITK_NULLPTR;

  // standard ImageIOBase variables
  m_ByteOrder = BigEndian;
  this->SetNumberOfDimensions(3); // otherwise, things go crazy w/dir cosines
  m_PixelType = SCALAR;
  m_ComponentType = UCHAR;
  //m_FileType =

  // specific members
  m_UseJPEGCodec = false;
  m_UseJPLSCodec = false;
  m_UseRLECodec  = false;
  m_DicomImageSetByUser = 0;
  this->AddSupportedWriteExtension(".dcm");
  this->AddSupportedWriteExtension(".DCM");
  this->AddSupportedWriteExtension(".dicom");
  this->AddSupportedWriteExtension(".DICOM");

  // this->AddSupportedReadExtension(".dcm");
  // this->AddSupportedReadExtension(".DCM");
  // this->AddSupportedReadExtension(".dicom");
  // this->AddSupportedReadExtension(".DICOM");

  // DCMTK loves printing warnings, turn off by default.
  this->SetLogLevel(FATAL_LOG_LEVEL);
}

void
DCMTKImageIO
::SetLogLevel(LogLevel level)
{
  switch(level)
    {
    case TRACE_LOG_LEVEL:
      OFLog::configure(OFLogger::TRACE_LOG_LEVEL);
      break;
    case DEBUG_LOG_LEVEL:
      OFLog::configure(OFLogger::DEBUG_LOG_LEVEL);
      break;
    case INFO_LOG_LEVEL:
      OFLog::configure(OFLogger::INFO_LOG_LEVEL);
      break;
    case WARN_LOG_LEVEL:
      OFLog::configure(OFLogger::WARN_LOG_LEVEL);
      break;
    case ERROR_LOG_LEVEL:
      OFLog::configure(OFLogger::ERROR_LOG_LEVEL);
      break;
    case FATAL_LOG_LEVEL:
      OFLog::configure(OFLogger::FATAL_LOG_LEVEL);
      break;
    case OFF_LOG_LEVEL:
      OFLog::configure(OFLogger::OFF_LOG_LEVEL);
      break;
    default:
      itkExceptionMacro(<< "Unknown DCMTK Logging constant "
                        << level);
    }
}

DCMTKImageIO::LogLevel
DCMTKImageIO
::GetLogLevel() const
{
  dcmtk::log4cplus::Logger rootLogger = dcmtk::log4cplus::Logger::getRoot();
  switch(rootLogger.getLogLevel())
    {
    case OFLogger::TRACE_LOG_LEVEL:
      return TRACE_LOG_LEVEL;
    case OFLogger::DEBUG_LOG_LEVEL:
      return DEBUG_LOG_LEVEL;
    case OFLogger::INFO_LOG_LEVEL:
      return INFO_LOG_LEVEL;
    case OFLogger::WARN_LOG_LEVEL:
      return WARN_LOG_LEVEL;
    case OFLogger::ERROR_LOG_LEVEL:
      return ERROR_LOG_LEVEL;
    case OFLogger::FATAL_LOG_LEVEL:
      return FATAL_LOG_LEVEL;
    case OFLogger::OFF_LOG_LEVEL:
      return OFF_LOG_LEVEL;
    }
  // will never happen
  return FATAL_LOG_LEVEL;
}

/** Destructor */
DCMTKImageIO::~DCMTKImageIO()
{
  if(!this->m_DicomImageSetByUser)
    {
    delete m_DImage;
    }
  DJDecoderRegistration::cleanup();
  DcmRLEDecoderRegistration::cleanup();
}

bool DCMTKImageIO::CanReadFile(const char *filename)
{
  // First check the filename extension
  std::string fname = filename;

  if ( fname == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    }

  return DCMTKFileReader::IsImageFile(filename);
}

bool DCMTKImageIO::CanWriteFile(const char *name)
{
  std::string fname = name;

  if ( fname == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    }

  bool                   extensionFound = false;
  std::string::size_type dcmPos = fname.rfind(".dcm");
  if ( ( dcmPos != std::string::npos )
       && ( dcmPos == fname.length() - 4 ) )
    {
    extensionFound = true;
    }

  dcmPos = fname.rfind(".DCM");
  if ( ( dcmPos != std::string::npos )
       && ( dcmPos == fname.length() - 4 ) )
    {
    extensionFound = true;
    }

  dcmPos = fname.rfind(".dicom");
  if ( ( dcmPos != std::string::npos )
       && ( dcmPos == fname.length() - 6 ) )
    {
    extensionFound = true;
    }

  dcmPos = fname.rfind(".DICOM");
  if ( ( dcmPos != std::string::npos )
       && ( dcmPos == fname.length() - 6 ) )
    {
    extensionFound = true;
    }

  if ( !extensionFound )
    {
    itkDebugMacro(<< "The filename extension is not recognized");
    return false;
    }

  if ( extensionFound )
    {
    return true;
    }
  return false;
}

void
DCMTKImageIO
::OpenDicomImage()
{
  if(this->m_DImage != ITK_NULLPTR)
    {
    if( !this->m_DicomImageSetByUser &&
        this->m_FileName != this->m_LastFileName)
      {
      delete m_DImage;
      this->m_DImage = ITK_NULLPTR;
      }
    }
  if( m_DImage == ITK_NULLPTR )
    {
    m_DImage = new DicomImage( m_FileName.c_str() );
    this->m_LastFileName = this->m_FileName;
    }
  if(this->m_DImage == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't create DicomImage for "
                      << this->m_FileName)
    }
}


//------------------------------------------------------------------------------
void
DCMTKImageIO
::Read(void *buffer)
{
  this->OpenDicomImage();
  if (m_DImage->getStatus() != EIS_Normal)
    {
    itkExceptionMacro(<< "Error: cannot load DICOM image ("
                      << DicomImage::getString(m_DImage->getStatus())
                      << ")")
      }

  m_Dimensions[0] = (unsigned int)(m_DImage->getWidth());
  m_Dimensions[1] = (unsigned int)(m_DImage->getHeight());

  // pick a size for output image (should get it from DCMTK in the ReadImageInformation()))
  // NOTE ALEX: EP_Representation is made for that
  // but i don t know yet where to fetch it from
  size_t scalarSize = ImageIOBase::GetComponentSize();

  switch(this->m_ComponentType)
    {
    case UNKNOWNCOMPONENTTYPE:
    case FLOAT:
    case DOUBLE:
      itkExceptionMacro(<< "Bad component type" <<
                        ImageIOBase::GetComponentTypeAsString(this->m_ComponentType));
      break;
    default: // scalarSize already set
      break;
    }
  // get the image in the DCMTK buffer
  const DiPixel * const interData = m_DImage->getInterData();
  const void *data = interData->getData();
  unsigned long count = interData->getCount();
  size_t voxelSize(scalarSize);
  switch(this->m_PixelType)
    {
    case RGB:
      ReorderRGBValues(buffer, data, count, 3);
      return;
    case RGBA:
      ReorderRGBValues(buffer, data, count, 4);
      return;
    case VECTOR:
      voxelSize *= this->GetNumberOfComponents();
      break;
    default:
      voxelSize *= 1;
      break;
    }
    memcpy(buffer,
         data,
         count * voxelSize);
}

void
DCMTKImageIO
::ReorderRGBValues(void *buffer, const void* data, unsigned long count, unsigned int voxel_size)
{
    switch(this->m_ComponentType)
      {
      // DCMTK only supports unsigned integer types for RGB(A) images.
      // see DCMTK file dcmimage/libsrc/dicoimg.cc (function const void *DiColorImage::getData(...) )
      // DCMTK only supports uint8, uint16, and uint32, but we leave LONG (at least 32bits but
      // could be 64bits) for future support.
      case UCHAR:
        ReorderRGBValues<unsigned char>(buffer, data, count, voxel_size);
        break;
      case USHORT:
        ReorderRGBValues<unsigned short>(buffer, data, count, voxel_size);
        break;
      case UINT:
        ReorderRGBValues<unsigned int>(buffer, data, count, voxel_size);
        break;
      case ULONG:
        ReorderRGBValues<unsigned long>(buffer, data, count, voxel_size);
        break;
      default:
        itkExceptionMacro(<< "Only unsigned integer pixel types are supported. Bad component type for color image" <<
                        ImageIOBase::GetComponentTypeAsString(this->m_ComponentType));
      break;
    }
}
/**
 *  Read Information about the DICOM file
 */
void DCMTKImageIO::ReadImageInformation()
{

  DJDecoderRegistration::registerCodecs();
  DcmRLEDecoderRegistration::registerCodecs();

  DCMTKFileReader reader;
  reader.SetFileName(this->m_FileName);
  try
    {
    reader.LoadFile();
    }
  catch(...)
    {
    std::cerr << "DCMTKImageIO::ReadImageInformation: "
              << "DicomImage could not read the file." << std::endl;
    }

  // check for multiframe > 3D
  ::itk::int32_t numPhases;
  unsigned      numDim(3);

  if(reader.GetElementSL(0x2001,0x1017,numPhases,false) != EXIT_SUCCESS)
    {
    numPhases = 1;
    }
  if(numPhases > 1)
    {
    this->SetNumberOfDimensions(4);
    numDim = 4;
    }

  unsigned short rows,columns;
  reader.GetDimensions(rows,columns);
  this->m_Dimensions[0] = columns;
  this->m_Dimensions[1] = rows;
  if(numPhases == 1)
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

  reader.GetDirCosines(rowDirection,columnDirection,sliceDirection);
  // orthogonalize
  sliceDirection.normalize();
  rowDirection = vnl_cross_3d(columnDirection,sliceDirection).normalize();
  columnDirection.normalize();

  if(numDim < 4)
    {
    this->SetDirection(0,rowDirection);
    this->SetDirection(1,columnDirection);
    if(this->m_NumberOfDimensions > 2)
      {
      this->SetDirection(2,sliceDirection);
      }
    }
  else
    {
    vnl_vector<double> rowDirection4(4),
      columnDirection4(4),
      sliceDirection4(4),
      phaseDirection4(4);
    for(unsigned i = 0; i < 3; ++i)
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
    this->SetDirection(0,rowDirection4);
    this->SetDirection(1,columnDirection4);
    this->SetDirection(2,sliceDirection4);
    this->SetDirection(3,phaseDirection4);
    }

  // get slope and intercept
  reader.GetSlopeIntercept(this->m_RescaleSlope,this->m_RescaleIntercept);

  double spacing[3];
  double origin[3];
  reader.GetSpacing(spacing);
  reader.GetOrigin(origin);
  this->m_Origin.resize(numDim);

  for(unsigned i = 0; i < 3; i++)
    {
    this->m_Origin[i] = origin[i];
    }

  this->m_Spacing.clear();
  for(unsigned i = 0; i < 3; i++)
    {
    this->m_Spacing.push_back(spacing[i]);
    }
  if(numDim == 4)
    {
    this->m_Origin[3] = 0.0;
    this->m_Spacing.push_back(1.0);
    }


  this->OpenDicomImage();
  const DiPixel *interData = this->m_DImage->getInterData();

  if(interData == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Missing Image Data in "
                      << this->m_FileName);
    }

  EP_Representation pixelRep = this->m_DImage->getInterData()->getRepresentation();
  switch(pixelRep)
    {
    case EPR_Uint8:
      this->m_ComponentType = UCHAR; break;
    case EPR_Sint8:
      this->m_ComponentType = CHAR; break;
    case EPR_Uint16:
      this->m_ComponentType = USHORT; break;
    case EPR_Sint16:
      this->m_ComponentType = SHORT; break;
    case EPR_Uint32:
      this->m_ComponentType = UINT; break;
    case EPR_Sint32:
      this->m_ComponentType = INT; break;
    default: // HACK should throw exception
      this->m_ComponentType = USHORT; break;
    }
  int numPlanes = this->m_DImage->getInterData()->getPlanes();
  switch(numPlanes)
    {
    case 1:
      this->m_PixelType = SCALAR; break;
    case 2:
      // hack, supposedly Luminence/Alpha
      this->SetNumberOfComponents(2);
      this->m_PixelType = VECTOR; break;
    case 3:
      this->SetNumberOfComponents(3);
      this->m_PixelType = RGB; break;
    case 4:
      this->SetNumberOfComponents(4);
      this->m_PixelType = RGBA; break;
    }
}

void
DCMTKImageIO
::WriteImageInformation(void)
{}

/** */
void
DCMTKImageIO
::Write(const void *buffer)
{
  (void)(buffer);
}

/** Print Self Method */
void DCMTKImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk
