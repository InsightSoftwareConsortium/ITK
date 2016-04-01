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
#include "itkVXLVideoIO.h"


namespace itk
{

///////////////////////////////////////////////////////////////////////////////
// Constructor, Destructor, and Print
//


//
// Constructor
//
VXLVideoIO::VXLVideoIO()
{
  this->ResetMembers();
}


//
// Destructor
//
VXLVideoIO::~VXLVideoIO()
{
  this->FinishReadingOrWriting();
}


//
// PrintSelf
//
void VXLVideoIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Reader Open: "<< this->m_ReaderOpen << std::endl;
  os << indent << "Writer Open: "<< this->m_WriterOpen << std::endl;
  os << indent << "Image dimensions: [" << this->m_Dimensions[0] <<","
     << this->m_Dimensions[1]<<"]" << std::endl;
  os << indent << "Frame Total: " << this->m_FrameTotal << std::endl;
}

//
// FinishReadingOrWriting
//
void VXLVideoIO::FinishReadingOrWriting()
{
  delete this->m_Writer;
  this->m_Writer = ITK_NULLPTR;
  delete this->m_Reader;
  this->m_Reader = ITK_NULLPTR;

  this->ResetMembers();
}


///////////////////////////////////////////////////////////////////////////////
// Member Accessors
//


//
// GetPositionInMSec
//
VXLVideoIO::TemporalOffsetType VXLVideoIO::GetPositionInMSec() const
{
  return this->m_PositionInMSec;
}

//
// GetRatio
//
VXLVideoIO::TemporalRatioType VXLVideoIO::GetRatio() const
{
  return this->m_Ratio;
}

//
// GetFrameTotal
//
VXLVideoIO::FrameOffsetType  VXLVideoIO::GetFrameTotal() const
{
  return this->m_FrameTotal;
}

//
// GetFramesPerSecond
//
VXLVideoIO::TemporalOffsetType VXLVideoIO::GetFramesPerSecond() const
{
  return this->m_FramesPerSecond;
}

//
// GetCurrentFrame
//
VXLVideoIO::FrameOffsetType VXLVideoIO::GetCurrentFrame() const
{
  return this->m_CurrentFrame;
}

//
// GetIFrameInterval
//
VXLVideoIO::FrameOffsetType VXLVideoIO::GetIFrameInterval() const
{
  return this->m_IFrameInterval;
}

//
// GetLastIFrame
//
VXLVideoIO::FrameOffsetType VXLVideoIO::GetLastIFrame() const
{
  return this->m_LastIFrame;
}

//
// SetCameraIndex
//
void VXLVideoIO::SetCameraIndex(int idx)
{
  this->m_CameraIndex = idx;
}

//
// GetCameraIndex
//
int VXLVideoIO::GetCameraIndex()
{
  return this->m_CameraIndex;
}

///////////////////////////////////////////////////////////////////////////////
// Read related methods
//


//
// SetReadFromFile
//
void VXLVideoIO::SetReadFromFile()
{
  if (!this->m_ReaderOpen && !this->m_WriterOpen)
    {
    this->m_ReadType = ReadFromFile;
    }
  else
    {
    itkExceptionMacro("Cannot change read type while reader is open");
    }
}

//
// SetReadFromCamera
//
void VXLVideoIO::SetReadFromCamera()
{
  if (!this->m_ReaderOpen && !this->m_WriterOpen)
    {
    this->m_ReadType = ReadFromCamera;
    }
  else
    {
    itkExceptionMacro("Cannot change read type while reader is open");
    }
}

//
// CanReadFile
//
bool VXLVideoIO::CanReadFile(const char* filename)
{
  // Make sure filename is specified
  std::string fname = filename;
  if (fname == "")
    {
    itkDebugMacro(<< "NoFilename specified");
    return false;
    }

  // Check File Extension (container type)
  //
  // Note: For now we only allow avi format, but this isn't right. We need to
  //       support all formats that vxl does (which I believe is some subset
  //       of all the formats that ffmpeg supports)
  bool extensionFound = false;
  std::string::size_type extPos = fname.rfind(".avi");
  if ( extPos != std::string::npos && extPos == fname.length() - 4 )
    {
    extensionFound = true;
    }
  extPos = fname.rfind(".AVI");
  if ( extPos != std::string::npos && extPos == fname.length() - 4 )
    {
    extensionFound = true;
    }
  if (!extensionFound)
    {
    itkDebugMacro(<< "Unrecognized file extension");
    return false;
    }


  // Try opening to read
  vidl_ffmpeg_istream localStream(filename);
  if (!localStream.is_open())
    {
    return false;
    }

  // Return true if successful
  return true;
}

//
// CanReadCamera
//
bool VXLVideoIO::CanReadCamera( CameraIDType  cameraID ) const
{
  itkWarningMacro( << "For now, camera reading is not supported with VXL:"<<cameraID);
  return false;
}


//
// ReadImageInformation
//
void VXLVideoIO::ReadImageInformation()
{

  // Get information from camera
  if (this->m_ReadType == ReadFromCamera)
    {
    itkExceptionMacro( << "For now, camera reading is not supported with VXL");
    }

  // Get information from file
  else if (this->m_ReadType == ReadFromFile)
    {
    // Set up local stream
    vidl_ffmpeg_istream localStream(this->GetFileName());

    // Populate information
    this->m_FrameTotal = localStream.num_frames();
    this->m_Dimensions.clear();
    this->m_Dimensions.push_back( localStream.width() );
    this->m_Dimensions.push_back( localStream.height() );
    this->m_PixelFormat = localStream.format();
    this->m_FramesPerSecond = localStream.frame_rate();
    this->m_NumberOfComponents = this->GetNChannelsFromPixelFormat(this->m_PixelFormat);

    // Assing the component type
    unsigned int bytesPerPixel = this->GetSizeFromPixelFormat(this->m_PixelFormat);
    if (bytesPerPixel == 0)
      {
      itkExceptionMacro("Faile to load local steam. FFMPEG libraries seems to be missing in VXL installation.");
      }
    if (bytesPerPixel == 1)
      {
      this->m_ComponentType = UCHAR;
      }
    else if (bytesPerPixel == 2)
      {
      this->m_ComponentType = UINT;
      }
    else
      {
      itkExceptionMacro("Unknown Pixel Component Type");
      }

    // Try to figure out if there are I-Frame issues we need to worry about
    // and compensate accrodingly
    if (this->m_FrameTotal > 0)
      {
      localStream.advance();  // Advance to first frame (0)
      localStream.advance();  // Try to advance to frame 1 and see what we get
      this->m_IFrameInterval = localStream.frame_number();
      if (this->m_IFrameInterval == 0)
        {
        itkExceptionMacro(<< "I-Frame spacing for this video is zeror! Please check input data.");
        }
      this->m_LastIFrame =
        static_cast<FrameOffsetType>((float)this->m_FrameTotal / (float)this->m_IFrameInterval)
        * this->m_IFrameInterval -1;

      // If the I-Frame spacing is not 1, warn the user
      if (this->m_IFrameInterval != 1)
        {
        itkWarningMacro(<< "VXL can only seek to I-Frames. I-Frame spacing for this video is "
          << this->m_IFrameInterval << ". Last I-Frame is " << this->m_LastIFrame);
        }
      }
    }

  // Should never get here
  else
    {
    itkExceptionMacro(<< "Invalid Read Type... How did we get here?");
    }
}

//
// Read
//
void VXLVideoIO::Read(void *buffer)
{

  // Make sure we've already called ReadImageInformation (dimensions are non-zero)
  if (this->m_Dimensions.size() != 2 ||
      this->m_Dimensions[0] == 0 || this->m_Dimensions[1] == 0)
    {
    itkExceptionMacro(<< "Cannot read frame with zero dimension. May need to call ReadImageInformation");
    }

  // If video is not already open, open it and keep it open
  if (!this->m_ReaderOpen)
    {
    this->OpenReader();
    }

  // Advance to the next frame if possible
  if(!this->m_Reader->advance())
    {
    itkDebugMacro(<< "Could not advance to the next frame");
    }

  // Read the current frame
  this->m_VIDLFrame = this->m_Reader->current_frame();

  // Check to see if the pixel format needs converting at all
  if (!this->PixelFormatSupported(this->m_PixelFormat))
    {

    // Convert to RGBA (4 channels), RGB (3 channels), or mono (1 channel)
    unsigned int pixelSize = GetSizeFromPixelFormat(this->m_PixelFormat);
    if (this->m_NumberOfComponents == 4)
      {
      std::stringstream ss;
      ss << "RGBA " << pixelSize*8*4;
      this->m_VIDLFrame = vidl_convert_frame(this->m_VIDLFrame,
                            vidl_pixel_format_from_string(ss.str()));
      }
    else if (this->m_NumberOfComponents == 3)
      {
      std::stringstream ss;
      ss << "RGB " << pixelSize*8*3;
      this->m_VIDLFrame = vidl_convert_frame(this->m_VIDLFrame,
                            vidl_pixel_format_from_string(ss.str()));
      }
    else if (this->m_NumberOfComponents == 1)
      {
      std::stringstream ss;
      ss << "MONO " << pixelSize*8;
      this->m_VIDLFrame = vidl_convert_frame(this->m_VIDLFrame,
                            vidl_pixel_format_from_string(ss.str().c_str()));
      }
    else
      {
      itkExceptionMacro(<< "Unsupported Pixel Format " << vidl_pixel_format_to_string(this->m_PixelFormat));
      }

    }

  // Copy the data to the buffer
  size_t bufferSize = this->m_VIDLFrame->size();
  memcpy(buffer, this->m_VIDLFrame->data(), bufferSize);
}


//
// SetNextFrameToRead
//
bool VXLVideoIO::SetNextFrameToRead( FrameOffsetType frameNumber)
{
  // If the reader isn't open, open it
  if (!this->m_ReaderOpen)
    {
    this->OpenReader();
    }

  // Make sure we're not setting past the end
  if (frameNumber > this->m_LastIFrame)
    {
    itkDebugMacro(<< "Warning: Trying to seek past end of video (past last I-Frame)");
    return false;
    }

  if (this->m_Reader->is_open())
    {
    this->m_Reader->seek_frame(frameNumber);
    this->UpdateReaderProperties();
    this->Modified();

    return true;
    }
  return false;
}

///////////////////////////////////////////////////////////////////////////////
// Write related methods
//


//
// CanWriteFile
//
bool VXLVideoIO::CanWriteFile(const char* filename)
{

  // Make sure reader is closed
  if (this->m_ReaderOpen)
    {
    itkWarningMacro(<< "Can't write anything if reader is open");
    return false;
    }

  // Make sure filename is specified
  std::string fname = filename;
  if (fname == "")
    {
    itkWarningMacro(<< "No Filename specified");
    return false;
    }

  // Check File Extension (container type)
  //
  // Note: For now we only allow avi format, but this isn't right. We need to
  //       support all formats that vxl does (which I believe is some subset
  //       of all the formats that ffmpeg supports)
  bool extensionFound = false;
  std::string::size_type extPos = fname.rfind(".avi");
  if ( extPos != std::string::npos && extPos == fname.length() - 4 )
    {
    extensionFound = true;
    }
  extPos = fname.rfind(".AVI");
  if ( extPos != std::string::npos && extPos == fname.length() - 4 )
    {
    extensionFound = true;
    }
  if (!extensionFound)
    {
    itkWarningMacro(<< "Unrecognized file extension " << fname);
    return false;
    }

  return true;
}

//
// WriteImageInformation
//
void VXLVideoIO::WriteImageInformation()
{
  // Don't do anything
}

//
// SetWriterParameters
//
void VXLVideoIO::SetWriterParameters(TemporalRatioType fps, const std::vector<SizeValueType>& dim,
                                     const char* fourCC, unsigned int nChannels,
                                     IOComponentType componentType)
{
  if (this->m_ReaderOpen || this->m_WriterOpen)
    {
    itkExceptionMacro("Can not set the writer's parameters when either reader or writer is already open");
    }

  if (componentType != UCHAR && componentType != UINT)
    {
    itkExceptionMacro("VXL IO only supports writing video with pixels of UCHAR and UINT");
    }
  else
    {
    this->m_ComponentType = componentType;
    }

  if (dim.size() != 2)
    {
    itkExceptionMacro("VXL IO only supports 2D video");
    }
  this->m_Dimensions.clear();
  this->m_Dimensions.push_back(dim[0]);
  this->m_Dimensions.push_back(dim[1]);

  this->m_FramesPerSecond = fps;
  this->m_Encoder = this->FourCCtoEncoderType(fourCC);
  this->m_NumberOfComponents = nChannels;

  // Figure out the right pixel type to write out
  if (this->m_NumberOfComponents == 4)
    {
    this->m_PixelType = RGBA;
    std::stringstream ss;
    ss << "RGBA " << this->m_NumberOfComponents * this->GetComponentSize() * 8;
    this->m_PixelFormat = vidl_pixel_format_from_string(ss.str());
    }
  else if (this->m_NumberOfComponents == 3)
    {
    this->m_PixelType = RGB;
    std::stringstream ss;
    ss << "RGB " << this->m_NumberOfComponents * this->GetComponentSize() * 8;
    this->m_PixelFormat = vidl_pixel_format_from_string(ss.str());
    }
  else if (this->m_NumberOfComponents == 1)
    {
    this->m_PixelType = SCALAR;
    std::stringstream ss;
    ss << "MONO " << this->m_NumberOfComponents * this->GetComponentSize() * 8;
    this->m_PixelFormat = vidl_pixel_format_from_string(ss.str());
    }
  else
    {
    itkExceptionMacro(<< "Invalid number of channels " << this->m_NumberOfComponents);
    }
}

//
// Write
//
void VXLVideoIO::Write(const void *buffer)
{
  // Make sure parameters are specified
  if (this->m_FramesPerSecond == 0 || this->m_Dimensions.size() != 2 || this->m_Encoder == 0)
    {
    itkExceptionMacro("Can not write with empty parameters. You probably need to call SetWriterParameters");
    }

  // If the writer isn't open yet, open it
  if (!this->m_WriterOpen)
    {
    this->OpenWriter();
    }

  // Create the output frame
  this->m_VIDLFrame = new vidl_shared_frame(const_cast<void*>(buffer),
    this->m_Dimensions[0], this->m_Dimensions[1], this->m_PixelFormat);

  // Write the frame out
  this->m_Writer->write_frame(this->m_VIDLFrame);
}


///////////////////////////////////////////////////////////////////////////////
// Protected methods
//

//
// GetNChannelsFromPixelFormat
//
unsigned int VXLVideoIO::GetNChannelsFromPixelFormat(vidl_pixel_format fmt)
{

  vidl_pixel_traits traits = vidl_pixel_format_traits(fmt);
  return traits.num_channels;
}

//
// GetSizeFromPixelFormat
//
unsigned int VXLVideoIO::GetSizeFromPixelFormat(vidl_pixel_format fmt)
{

  vidl_pixel_traits traits = vidl_pixel_format_traits(fmt);
  return traits.bits_per_pixel/8;
}

//
// PixelFormatSupported
//
bool VXLVideoIO::PixelFormatSupported(vidl_pixel_format fmt)
{
  // Get a string representation of the format
  std::string s = vidl_pixel_format_to_string(fmt);
  vul_reg_exp reRGB(".*_RGB_.*");
  if (reRGB.find(s))
    {
    return true;
    }
  vul_reg_exp reRGBA(".*_RGBA_.*");
  if (reRGBA.find(s))
    {
    return true;
    }
  vul_reg_exp reMONO(".*_MONO_.*");
  if (reMONO.find(s))
    {
    return true;
    }

  return false;
}

//
// FourCCtoEncoderType
//
vidl_ffmpeg_ostream_params::encoder_type VXLVideoIO::FourCCtoEncoderType(const char* fourCC)
{
  if (!strcmp(fourCC, "DIVX"))
    {
    return vidl_ffmpeg_ostream_params::MPEG4;
    }
  else if (!strcmp(fourCC, "MP42"))
    {
    return vidl_ffmpeg_ostream_params::MSMPEG4V2;
    }
  else if (!strcmp(fourCC, "MP2V"))
    {
    return vidl_ffmpeg_ostream_params::MPEG2VIDEO;
    }
  else if (!strcmp(fourCC, "DVCS"))
    {
    return vidl_ffmpeg_ostream_params::DVVIDEO;
    }
  else if (!strcmp(fourCC, "Ljpg"))
    {
    return vidl_ffmpeg_ostream_params::LJPEG;
    }
  else if (!strcmp(fourCC, "raw "))
    {
    return vidl_ffmpeg_ostream_params::RAWVIDEO;
    }
  else
    {
    itkWarningMacro(<< "Unknown FourCC: " << fourCC);
    return vidl_ffmpeg_ostream_params::DEFAULT;
    }
}

//
// UpdateReaderProperties
//
void VXLVideoIO::UpdateReaderProperties()
{
  this->m_CurrentFrame = this->m_Reader->frame_number();

  this->m_Ratio = (double)this->m_CurrentFrame / (double)this->m_FrameTotal;
  this->m_PositionInMSec = this->m_Reader->duration() * this->m_Ratio;

}

//
// OpenReader
//
void VXLVideoIO::OpenReader()
{
  if (this->m_ReaderOpen)
    {
    itkExceptionMacro("Can not open reader while video is already open for reading");
    }

  if (this->m_WriterOpen)
    {
    itkExceptionMacro("Can not open reader while video is already open for writing");
    }

  // If neither reader nor writer is currently open, open the reader
  if (this->m_ReadType == ReadFromFile)
    {
    this->m_Reader = new vidl_ffmpeg_istream();
    this->m_Reader->open(this->GetFileName());
    if (this->m_Reader->is_open())
      {
      this->m_ReaderOpen = true;
      }
    else
      {
      itkExceptionMacro("Video failed to open");
      }
    }

  // Read from camera
  else if (this->m_ReadType == ReadFromCamera)
    {
    itkWarningMacro(<< "VXL camera not currently implemented");
    }
}

//
// OpenWriter
//
void VXLVideoIO::OpenWriter()
{
  if (this->m_WriterOpen)
    {
    itkExceptionMacro("Can not open writer while video is already open for writing");
    }

  if (this->m_ReaderOpen)
    {
    itkExceptionMacro("Can not open writer while video is already open for reading");
    }

  vidl_ffmpeg_ostream_params parameters;
  parameters.frame_rate_ = this->m_FramesPerSecond;
  parameters.ni_ = this->m_Dimensions[0];
  parameters.nj_ = this->m_Dimensions[1];
  parameters.encoder_ = this->m_Encoder;

  this->m_Writer = new vidl_ffmpeg_ostream(this->GetFileName(), parameters);

  this->m_WriterOpen = true;

}

//
// ResetMembers
//
void VXLVideoIO::ResetMembers()
{
  this->m_PixelFormat = VIDL_PIXEL_FORMAT_UNKNOWN;
  this->m_VIDLFrame = 0;
  this->m_VIDLFrame = 0;
  this->m_Encoder = vidl_ffmpeg_ostream_params::DEFAULT;
  this->m_Reader = ITK_NULLPTR;
  this->m_Writer = ITK_NULLPTR;
  this->m_WriterOpen = false;
  this->m_ReaderOpen = false;
  this->m_FramesPerSecond = 0;
  this->m_Dimensions.clear();
  this->m_FrameTotal = 0;
  this->m_CurrentFrame = 0;
  this->m_IFrameInterval = 0;
  this->m_LastIFrame = 0;

  // Default to reading from a file
  this->m_ReadType = ReadFromFile;
  this->m_CameraIndex = 0;

  // Members from ImageIOBase
  this->m_PixelType = SCALAR;
  this->m_ComponentType = UCHAR;
  this->SetNumberOfDimensions(2);
  this->m_Spacing[0] = 1.0;
  this->m_Spacing[1] = 1.0;
  this->m_Origin[0] = 0.0;
  this->m_Origin[1] = 0.0;
}


} // end namespace itk
