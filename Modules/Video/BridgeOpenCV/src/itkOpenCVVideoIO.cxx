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
#include "itkOpenCVVideoIO.h"

#include <algorithm>

#include "opencv2/imgproc.hpp"


namespace itk
{

OpenCVVideoIO::OpenCVVideoIO() { this->ResetMembers(); }

OpenCVVideoIO::~OpenCVVideoIO() { this->FinishReadingOrWriting(); }

void
OpenCVVideoIO::FinishReadingOrWriting()
{
  this->ResetMembers();
}

OpenCVVideoIO::TemporalOffsetType
OpenCVVideoIO::GetPositionInMSec() const
{
  return this->m_PositionInMSec;
}

OpenCVVideoIO::TemporalRatioType
OpenCVVideoIO::GetRatio() const
{
  return this->m_Ratio;
}

OpenCVVideoIO::FrameOffsetType
OpenCVVideoIO::GetFrameTotal() const
{
  return this->m_FrameTotal;
}

OpenCVVideoIO::TemporalRatioType
OpenCVVideoIO::GetFramesPerSecond() const
{
  return this->m_FramesPerSecond;
}

OpenCVVideoIO::FrameOffsetType
OpenCVVideoIO::GetCurrentFrame() const
{
  return this->m_CurrentFrame;
}

OpenCVVideoIO::FrameOffsetType
OpenCVVideoIO::GetIFrameInterval() const
{
  return this->m_IFrameInterval;
}

OpenCVVideoIO::FrameOffsetType
OpenCVVideoIO::GetLastIFrame() const
{
  return this->m_LastIFrame;
}

void
OpenCVVideoIO::SetCameraIndex(CameraIDType idx)
{
  this->m_CameraIndex = idx;
}

OpenCVVideoIO::CameraIDType
OpenCVVideoIO::GetCameraIndex() const
{
  return this->m_CameraIndex;
}

void
OpenCVVideoIO::SetReadFromFile()
{
  if (!this->m_ReaderOpen && !this->m_WriterOpen)
  {
    this->m_ReadFrom = ReadFromEnum::ReadFromFile;
  }
  else
  {
    itkExceptionStringMacro("Cannot change read type while reader is open");
  }
}

void
OpenCVVideoIO::SetReadFromCamera()
{
  if (!this->m_ReaderOpen && !this->m_WriterOpen)
  {
    this->m_ReadFrom = ReadFromEnum::ReadFromCamera;
  }
  else
  {
    itkExceptionStringMacro("Cannot change read type while reader is open");
  }
}

bool
OpenCVVideoIO::CanReadFile(const char * filename)
{
  // Make sure filename is specified
  std::string fname = filename;
  if (fname == "")
  {
    itkDebugMacro("NoFilename specified");
    return false;
  }

  // Check File Extension (container type)
  //
  // Note: For now we only allow avi format, but this isn't right. We need to
  //       support all formats that OpenCV does (which I believe is some subset
  //       of all the formats that ffmpeg supports)
  bool                   extensionFound = false;
  std::string::size_type extPos = fname.rfind(".avi");
  if (extPos != std::string::npos && extPos == fname.length() - 4)
  {
    extensionFound = true;
  }
  extPos = fname.rfind(".AVI");
  if (extPos != std::string::npos && extPos == fname.length() - 4)
  {
    extensionFound = true;
  }
  if (!extensionFound)
  {
    itkDebugMacro("Unrecognized file extension");
    return false;
  }

  // Try opening to read
  cv::VideoCapture localCapture(filename);
  return localCapture.isOpened();
}

bool
OpenCVVideoIO::CanReadCamera(CameraIDType cameraID) const
{
  // Try capture from current camera index
  cv::VideoCapture localCapture(static_cast<int>(cameraID));
  return localCapture.isOpened();
}

void
OpenCVVideoIO::ReadImageInformation()
{
  // Set up a local capture and frame
  cv::VideoCapture localCapture;
  cv::Mat          tempFrame;

  // Open capture from a file
  if (this->m_ReadFrom == ReadFromEnum::ReadFromFile)
  {

    // Make sure file can be read
    std::string filename = this->GetFileName();
    if (!this->CanReadFile(filename.c_str()))
    {
      itkExceptionMacro("Cannot read file: " << filename);
    }

    // Open the video file
    localCapture.open(filename.c_str());

    // Query the frame and get frame total (since this is only valid for reading from files)
    localCapture >> tempFrame;
    this->m_FrameTotal = static_cast<OpenCVVideoIO::FrameOffsetType>(localCapture.get(cv::CAP_PROP_FRAME_COUNT));

    // Try to figure out if there are I-Frame issues we need to worry about
    // and compensate accordingly
    if (this->m_FrameTotal > 0)
    {
      // Try setting frame to 1 and see what actually gets set
      localCapture.set(cv::CAP_PROP_POS_FRAMES, 1);
      localCapture >> tempFrame;

      this->m_IFrameInterval = static_cast<OpenCVVideoIO::FrameOffsetType>(localCapture.get(cv::CAP_PROP_POS_FRAMES));

      if (this->m_IFrameInterval == 0)
      {
        itkExceptionStringMacro(" I-Frame spacing for this video is zero! Please check input data.");
      }

      this->m_LastIFrame = (OpenCVVideoIO::FrameOffsetType)(static_cast<float>(this->m_FrameTotal) /
                                                            static_cast<float>(this->m_IFrameInterval)) *
                             this->m_IFrameInterval -
                           1; // Frame index should be 0-based index

      // If the I-Frame spacing is not 1, warn the user
      if (this->m_IFrameInterval != 1)
      {
        itkWarningMacro("OpenCV can only seek to I-Frames. I-Frame spacing for this video is "
                        << this->m_IFrameInterval << ". Last I-Frame is " << this->m_LastIFrame);
      }
    }
  }

  // Open capture from a camera
  else if (this->m_ReadFrom == ReadFromEnum::ReadFromCamera)
  {

    // Open the camera capture
    localCapture.open(this->m_CameraIndex);

    // Make sure it opened right
    if (!localCapture.isOpened())
    {
      itkExceptionMacro("Cannot read from camera " << this->m_CameraIndex);
    }

    // Query the frame
    localCapture >> tempFrame;

    // Make sure the image is not empty
    if (tempFrame.empty())
    {
      itkExceptionMacro("Empty image got from camera " << this->m_CameraIndex);
    }

    // Set the frame total to 1
    this->m_FrameTotal = 1;
  }

  // Should never get here
  else
  {
    itkExceptionStringMacro("Invalid Read Type... How did we get here?");
  }

  // Populate member variables
  this->m_FramesPerSecond = localCapture.get(cv::CAP_PROP_FPS);

  // Set width, height
  this->m_Dimensions.clear();
  this->m_Dimensions.push_back(static_cast<SizeValueType>(localCapture.get(cv::CAP_PROP_FRAME_WIDTH)));
  this->m_Dimensions.push_back(static_cast<SizeValueType>(localCapture.get(cv::CAP_PROP_FRAME_HEIGHT)));

  this->m_NumberOfComponents = tempFrame.channels();

  // Set the pixel type
  if (this->m_NumberOfComponents == 1)
  {
    this->m_PixelType = IOPixelEnum::SCALAR;
  }
  else if (this->m_NumberOfComponents == 3)
  {
    this->m_PixelType = IOPixelEnum::RGB;
  }
  else if (this->m_NumberOfComponents == 4)
  {
    this->m_PixelType = IOPixelEnum::RGBA;
  }
  else
  {
    itkExceptionStringMacro("OpenCV IO only supports Mono, RGB, and RGBA input");
  }
}

void
OpenCVVideoIO::Read(void * buffer)
{
  // Make sure we've already called ReadImageInformation (dimensions are non-zero)
  if (this->m_Dimensions[0] == 0 || this->m_Dimensions[1] == 0)
  {
    itkExceptionStringMacro("Cannot read frame with zero dimension. May need to call ReadImageInformation");
  }

  // If video is not already open, open it and keep it open
  if (!this->m_ReaderOpen)
  {
    this->OpenReader();
  }

  // Read the desired frame
  //
  // Note: This will advance to the next frame in the stream, so SetNextFrameToRead
  //       doesn't need to be called before Read is called again unless you want to
  //       skip to a different location. Be warned, though. SetNextFrameToRead can
  //       only skip to I-Frames, so there can be unexpected behavior
  cv::Mat tempIm;
  m_Capture >> tempIm;
  if (tempIm.empty())
  {
    itkExceptionMacro("Error reading frame " << this->m_CurrentFrame << ". May be out of bounds");
  }

  // Convert color format from OpenCV's native BGR to ITK's expected channel order
  if (this->m_NumberOfComponents == 1)
  {
    m_CVImage = tempIm;
  }
  else if (this->m_NumberOfComponents == 3)
  {
    cv::cvtColor(tempIm, m_CVImage, cv::COLOR_BGR2RGB);
  }
  else
  {
    cv::cvtColor(tempIm, m_CVImage, cv::COLOR_BGRA2RGBA);
  }

  // Update the frame-dependent properties
  this->UpdateReaderProperties();

  // Put the frame's buffer into the supplied output buffer
  const auto * tempBuffer = m_CVImage.data;
  const size_t bufferSize = m_CVImage.total() * m_CVImage.elemSize();
  std::copy_n(tempBuffer, bufferSize, static_cast<unsigned char *>(buffer));
}

bool
OpenCVVideoIO::SetNextFrameToRead(OpenCVVideoIO::FrameOffsetType frameNumber)
{
  // If the capture isn't open, open it
  if (!this->m_ReaderOpen)
  {
    this->OpenReader();
  }

  // Make sure we're not setting past the end
  if (frameNumber > this->m_LastIFrame)
  {
    itkDebugMacro("Warning: Trying to seek past end of video (past last I-Frame)");
    return false;
  }

  if (m_Capture.isOpened())
  {
    m_Capture.set(cv::CAP_PROP_POS_FRAMES, frameNumber);
    this->UpdateReaderProperties();
    this->Modified();

    return true;
  }
  return false;
}

bool
OpenCVVideoIO::CanWriteFile(const char * filename)
{

  // Make sure reader is closed
  if (this->m_ReaderOpen)
  {
    itkWarningMacro("Can't write anything if reader is open");
    return false;
  }

  // Make sure filename is specified
  std::string fname = filename;
  if (fname == "")
  {
    itkWarningMacro("No Filename specified");
    return false;
  }

  // Check File Extension (container type)
  //
  // Note: For now we only allow avi format, but this isn't right. We need to
  //       support all formats that OpenCV does (which I believe is some subset
  //       of all the formats that ffmpeg supports)
  bool                   extensionFound = false;
  std::string::size_type extPos = fname.rfind(".avi");
  if (extPos != std::string::npos && extPos == fname.length() - 4)
  {
    extensionFound = true;
  }
  extPos = fname.rfind(".AVI");
  if (extPos != std::string::npos && extPos == fname.length() - 4)
  {
    extensionFound = true;
  }
  if (!extensionFound)
  {
    itkWarningMacro("Unrecognized file extension " << fname);
    return false;
  }

  return true;
}

void
OpenCVVideoIO::WriteImageInformation()
{
  // Don't do anything
}

void
OpenCVVideoIO::SetWriterParameters(TemporalRatioType                  fps,
                                   const std::vector<SizeValueType> & dim,
                                   const char *                       fourCC,
                                   unsigned int                       nChannels,
                                   IOComponentEnum                    componentType)
{
  if (this->m_ReaderOpen || this->m_WriterOpen)
  {
    itkExceptionStringMacro("Can not set the writer's parameters when either reader or writer is already open");
  }

  // Make sure componentType is acceptable (right now we only support char)
  if (componentType != IOComponentEnum::UCHAR)
  {
    itkExceptionStringMacro("OpenCV IO only supports writing video with pixels of UCHAR");
  }

  if (dim.size() != 2)
  {
    itkExceptionStringMacro("OpenCV IO only supports 2D video");
  }
  this->m_Dimensions.clear();
  this->m_Dimensions.push_back(dim[0]);
  this->m_Dimensions.push_back(dim[1]);

  this->m_FramesPerSecond = fps;
  this->m_FourCC = cv::VideoWriter::fourcc(fourCC[0], fourCC[1], fourCC[2], fourCC[3]);
  this->m_NumberOfComponents = nChannels;
  if (nChannels == 1)
  {
    this->m_PixelType = IOPixelEnum::SCALAR;
  }
  else if (nChannels == 3)
  {
    this->m_PixelType = IOPixelEnum::RGB;
  }
  else if (nChannels == 4)
  {
    this->m_PixelType = IOPixelEnum::RGBA;
  }
  else
  {
    itkExceptionStringMacro("OpenCV IO only supports Mono, RGB, and RGBA output");
  }
}

void
OpenCVVideoIO::Write(const void * buffer)
{
  // Make sure parameters are specified
  if (this->m_FramesPerSecond == 0 || this->m_Dimensions.size() == 0 || this->m_FourCC == 0)
  {
    itkExceptionStringMacro("Can not write with empty parameters. You probably need to call SetWriterParameters");
  }

  // Make sure the number of channels is 1 or 3
  if (this->m_NumberOfComponents != 1 && this->m_NumberOfComponents != 3)
  {
    itkExceptionMacro(
      "OpenCV only supports 1 and 3 component images. NumberOfComponents = " << this->m_NumberOfComponents);
  }

  // If the writer isn't open yet, open it
  if (!this->m_WriterOpen)
  {
    this->OpenWriter();
  }

  // Wrap the input buffer in a cv::Mat without copying (external ownership)
  const int inputType = CV_MAKETYPE(CV_8U, static_cast<int>(this->m_NumberOfComponents));
  cv::Mat   tempMat(static_cast<int>(this->m_Dimensions[1]),
                  static_cast<int>(this->m_Dimensions[0]),
                  inputType,
                  const_cast<void *>(buffer));

  // The video writer always needs a 3-channel BGR image; convert accordingly
  if (this->m_NumberOfComponents == 1)
  {
    cv::cvtColor(tempMat, m_CVImage, cv::COLOR_GRAY2BGR);
  }
  else
  {
    cv::cvtColor(tempMat, m_CVImage, cv::COLOR_RGB2BGR);
  }

  // Write the frame
  m_Writer.write(m_CVImage);

  // Update the current frame
  this->m_CurrentFrame++;
}

void
OpenCVVideoIO::UpdateReaderProperties()
{
  // 0-based index of the frame to be decoded/captured next
  this->m_CurrentFrame = static_cast<FrameOffsetType>(m_Capture.get(cv::CAP_PROP_POS_FRAMES));
  this->m_PositionInMSec = m_Capture.get(cv::CAP_PROP_POS_MSEC);
  this->m_FramesPerSecond = m_Capture.get(cv::CAP_PROP_FPS);
  this->m_Ratio = m_Capture.get(cv::CAP_PROP_POS_AVI_RATIO);
  this->m_FourCC = static_cast<int>(m_Capture.get(cv::CAP_PROP_FOURCC));
}

void
OpenCVVideoIO::OpenReader()
{
  if (this->m_ReaderOpen)
  {
    itkExceptionStringMacro("Can not open reader while video is already open for reading");
  }

  if (this->m_WriterOpen)
  {
    itkExceptionStringMacro("Can not open reader while video is already open for writing");
  }

  // If neither reader nor writer is currently open, open the reader
  if (this->m_ReadFrom == ReadFromEnum::ReadFromFile)
  {
    m_Capture.open(this->GetFileName());
    if (m_Capture.isOpened())
    {
      this->m_ReaderOpen = true;
    }
    else
    {
      itkExceptionStringMacro("Video failed to open");
    }
  }
  else if (this->m_ReadFrom == ReadFromEnum::ReadFromCamera)
  {
    m_Capture.open(this->m_CameraIndex);
    if (m_Capture.isOpened())
    {
      this->m_ReaderOpen = true;
    }
    else
    {
      itkExceptionStringMacro("Video failed to open");
    }
  }
}

void
OpenCVVideoIO::OpenWriter()
{
  if (this->m_WriterOpen)
  {
    itkExceptionStringMacro("Can not open writer while video is already open for writing");
  }

  if (this->m_ReaderOpen)
  {
    itkExceptionStringMacro("Can not open writer while video is already open for reading");
  }

  // If neither reader nor writer is currently open, open the writer
  m_Writer.open(this->GetFileName(),
                this->m_FourCC,
                this->m_FramesPerSecond,
                cv::Size(static_cast<int>(this->m_Dimensions[0]), static_cast<int>(this->m_Dimensions[1])));
  if (m_Writer.isOpened())
  {
    this->m_WriterOpen = true;
  }
  else
  {
    itkExceptionStringMacro("Video failed to open for writing");
  }
}

void
OpenCVVideoIO::ResetMembers()
{
  m_CVImage = cv::Mat{};
  m_Capture.release();
  m_Writer.release();
  this->m_WriterOpen = false;
  this->m_ReaderOpen = false;
  this->m_FramesPerSecond = 0;
  this->m_Dimensions.clear();
  this->m_FourCC = 0;
  this->m_FrameTotal = 0;
  this->m_CurrentFrame = 0;
  this->m_NumberOfComponents = 0;
  this->m_IFrameInterval = 0;
  this->m_LastIFrame = 0;

  // Default to reading from a file
  this->m_ReadFrom = ReadFromEnum::ReadFromFile;
  this->m_CameraIndex = 0;

  // Members from ImageIOBase
  this->m_PixelType = IOPixelEnum::SCALAR;
  this->m_ComponentType = IOComponentEnum::UCHAR;
  this->SetNumberOfDimensions(2);
  this->m_Spacing[0] = 1.0;
  this->m_Spacing[1] = 1.0;
  this->m_Origin[0] = 0.0;
  this->m_Origin[1] = 0.0;
}

void
OpenCVVideoIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if (!m_CVImage.empty())
  {
    os << indent << "Image dimensions : [" << m_CVImage.cols << ',' << m_CVImage.rows << ']' << std::endl;
    os << indent << "Image Size : " << (m_CVImage.total() * m_CVImage.elemSize()) << std::endl;
    os << indent << "Color channels : " << m_CVImage.channels() << std::endl;
  }
}

} // end namespace itk
