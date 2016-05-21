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
#include "itkOpenCVVideoIO.h"


namespace itk
{

OpenCVVideoIO::OpenCVVideoIO()
{
  this->ResetMembers();
}

OpenCVVideoIO::~OpenCVVideoIO()
{
  this->FinishReadingOrWriting();
}

void OpenCVVideoIO::FinishReadingOrWriting()
{
  if (this->m_Writer != ITK_NULLPTR)
    {
    cvReleaseVideoWriter(&(this->m_Writer));
    }
  if (this->m_Capture != ITK_NULLPTR)
    {
    cvReleaseCapture(&(this->m_Capture));
    }
  if (this->m_CVImage != ITK_NULLPTR)
    {
    cvReleaseImage(&(this->m_CVImage));
    }

  this->ResetMembers();

}

OpenCVVideoIO::TemporalOffsetType OpenCVVideoIO::GetPositionInMSec() const
{
  return this->m_PositionInMSec;
}

OpenCVVideoIO::TemporalRatioType OpenCVVideoIO::GetRatio() const
{
  return this->m_Ratio;
}

OpenCVVideoIO::FrameOffsetType OpenCVVideoIO::GetFrameTotal() const
{
  return this->m_FrameTotal;
}

OpenCVVideoIO::TemporalRatioType OpenCVVideoIO::GetFramesPerSecond() const
{
  return this->m_FramesPerSecond;
}

OpenCVVideoIO::FrameOffsetType OpenCVVideoIO::GetCurrentFrame() const
{
  return this->m_CurrentFrame;
}

OpenCVVideoIO::FrameOffsetType OpenCVVideoIO::GetIFrameInterval() const
{
  return this->m_IFrameInterval;
}

OpenCVVideoIO::FrameOffsetType OpenCVVideoIO::GetLastIFrame() const
{
  return this->m_LastIFrame;
}

void OpenCVVideoIO::SetCameraIndex(CameraIDType idx)
{
  this->m_CameraIndex = idx;
}

OpenCVVideoIO::CameraIDType OpenCVVideoIO::GetCameraIndex() const
{
  return this->m_CameraIndex;
}

void OpenCVVideoIO::SetReadFromFile()
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

void OpenCVVideoIO::SetReadFromCamera()
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

bool OpenCVVideoIO::CanReadFile(const char* filename)
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
  //       support all formats that OpenCV does (which I believe is some subset
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
  CvCapture* localCapture = cvCaptureFromFile( filename );
  if (!localCapture)
    {
    return false;
    }

  // Close the file and return true if successful
  cvReleaseCapture(&localCapture);
  return true;
}

bool OpenCVVideoIO::CanReadCamera( CameraIDType cameraID ) const
{
  // Try capture from current camera index
  CvCapture* localCapture = cvCaptureFromCAM( cameraID );
  if (!localCapture)
    {
    return false;
    }

  // Close the file and return true if successful
  cvReleaseCapture(&localCapture);
  return true;
}

void OpenCVVideoIO::ReadImageInformation()
{

  // Set up a local capture and image
  CvCapture* localCapture;
  IplImage* tempImage;

  // Open capture from a file
  if (this->m_ReadType == ReadFromFile)
    {

    // Make sure file can be read
    std::string filename = this->GetFileName();
    if (!this->CanReadFile(filename.c_str()))
      {
      itkExceptionMacro(<< "Cannot read file: " << filename);
      }

    // Open the video file
    localCapture = cvCaptureFromFile( filename.c_str() );

    // Query the frame and get frame total (since this is only valid for reading from files)
    tempImage = cvQueryFrame(localCapture);
    this->m_FrameTotal = static_cast<OpenCVVideoIO::FrameOffsetType>
      (cvGetCaptureProperty( localCapture, CV_CAP_PROP_FRAME_COUNT ));

    // Try to figure out if there are I-Frame issues we need to worry about
    // and compensate accrodingly
    if (this->m_FrameTotal > 0)
      {
      // Try setting frame to 1 and see what actually gets set
      cvSetCaptureProperty(localCapture, CV_CAP_PROP_POS_FRAMES, 1);
      tempImage = cvQueryFrame(localCapture);

      this->m_IFrameInterval = cvGetCaptureProperty(localCapture, CV_CAP_PROP_POS_FRAMES);

      if (this->m_IFrameInterval == 0)
        {
        itkExceptionMacro(<< " I-Frame spacing for this video is zero! Please check input data.");
        }

      this->m_LastIFrame =
        (OpenCVVideoIO::FrameOffsetType)((float)this->m_FrameTotal / (float)this->m_IFrameInterval)
        * this->m_IFrameInterval - 1;//Frame index should be 0-based index

      // If the I-Frame spacing is not 1, warn the user
      if (this->m_IFrameInterval != 1)
        {
        itkWarningMacro(<< "OpenCV can only seek to I-Frames. I-Frame spacing for this video is "
          << this->m_IFrameInterval << ". Last I-Frame is " << this->m_LastIFrame);
        }
      }
    }

  // Open capture from a camera
  else if (this->m_ReadType == ReadFromCamera)
    {

    // Open the camera capture
    localCapture = cvCaptureFromCAM( this->m_CameraIndex );

    // Make sure it opened right
    if (!localCapture)
      {
      itkExceptionMacro(<< "Cannot read from camera " << this->m_CameraIndex);
      }

    // Query the frame
    tempImage = cvQueryFrame(localCapture);

    // Make sure the image is not empty
    if (!tempImage)
      {
      cvReleaseCapture(&localCapture);
      itkExceptionMacro(<< "Empty image got from camera " << this->m_CameraIndex);
      }

    // Set the frame total to 1
    this->m_FrameTotal = 1;
    }

  // Should never get here
  else
    {
    itkExceptionMacro(<< "Invalid Read Type... How did we get here?");
    }

  // Populate member variables
  this->m_FramesPerSecond = static_cast<double>
    (cvGetCaptureProperty( localCapture, CV_CAP_PROP_FPS ));

  // Set width, height
  this->m_Dimensions.clear();
  this->m_Dimensions.push_back( cvGetCaptureProperty( localCapture, CV_CAP_PROP_FRAME_WIDTH ) );
  this->m_Dimensions.push_back( cvGetCaptureProperty( localCapture, CV_CAP_PROP_FRAME_HEIGHT ) );

  this->m_NumberOfComponents = tempImage->nChannels;

  // Set the pixel type
  if (this->m_NumberOfComponents == 1)
    {
    this->m_PixelType = SCALAR;
    }
  else if (this->m_NumberOfComponents == 3)
    {
    this->m_PixelType = RGB;
    }
  else if (this->m_NumberOfComponents == 4)
    {
    this->m_PixelType = RGBA;
    }
  else
    {
    itkExceptionMacro("OpenCV IO only supports Mono, RGB, and RGBA input");
    }

  // Release the local capture and image
  cvReleaseCapture(&localCapture);

}

void OpenCVVideoIO::Read(void *buffer)
{
  // Make sure we've already called ReadImageInformation (dimensions are non-zero)
  if (this->m_Dimensions[0] == 0 || this->m_Dimensions[1] == 0)
    {
    itkExceptionMacro(<< "Cannot read frame with zero dimension. May need to call ReadImageInformation");
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
  IplImage* tempIm = cvQueryFrame(this->m_Capture);
  if (tempIm == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Error reading frame " << this->m_CurrentFrame << ". May be out of bounds");
    }

  // Convert to RGB rather than BGR
  if (this->m_CVImage == ITK_NULLPTR)
    {
    this->m_CVImage = cvCreateImage( cvSize(this->m_Dimensions[0],this->m_Dimensions[1]),
                                     IPL_DEPTH_8U, this->m_NumberOfComponents );
    }
  cvCvtColor(tempIm, this->m_CVImage, CV_BGR2RGB);

  // Update the frame-dependent properties
  this->UpdateReaderProperties();

  // Put the frame's buffer into the supplied output buffer
  void* tempBuffer = reinterpret_cast<void*>(this->m_CVImage->imageData);
  size_t bufferSize = this->m_CVImage->imageSize;
  memcpy(buffer, tempBuffer, bufferSize);
}

bool OpenCVVideoIO::SetNextFrameToRead(OpenCVVideoIO::FrameOffsetType frameNumber)
{
  // If the capture isn't open, open it
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

  if (this->m_Capture != ITK_NULLPTR)
    {
    cvSetCaptureProperty(this->m_Capture,CV_CAP_PROP_POS_FRAMES, frameNumber);
    this->UpdateReaderProperties();
    this->Modified();

    return true;
    }
  return false;
}

bool OpenCVVideoIO::CanWriteFile(const char* filename)
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
  //       support all formats that OpenCV does (which I believe is some subset
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

void OpenCVVideoIO::WriteImageInformation()
{
  // Don't do anything
}

void OpenCVVideoIO::SetWriterParameters( TemporalRatioType fps,
                                         const std::vector<SizeValueType>& dim,
                                         const char* fourCC,
                                         unsigned int nChannels,
                                         IOComponentType componentType )
{
  if (this->m_ReaderOpen || this->m_WriterOpen)
    {
    itkExceptionMacro("Can not set the writer's parameters when either reader or writer is already open");
    }

  // Make sure componentType is acceptable (right now we only support char)
  if (componentType != UCHAR)
    {
    itkExceptionMacro("OpenCV IO only supports writing video with pixels of UCHAR");
    }

  if (dim.size() != 2)
    {
    itkExceptionMacro("OpenCV IO only supports 2D video");
    }
  this->m_Dimensions.clear();
  this->m_Dimensions.push_back(dim[0]);
  this->m_Dimensions.push_back(dim[1]);

  this->m_FramesPerSecond = fps;
  this->m_FourCC = CV_FOURCC(fourCC[0], fourCC[1], fourCC[2], fourCC[3]);
  this->m_NumberOfComponents = nChannels;
  if (nChannels == 1)
    {
    this->m_PixelType = SCALAR;
    }
  else if (nChannels == 3)
    {
    this->m_PixelType = RGB;
    }
  else if (nChannels == 4)
    {
    this->m_PixelType = RGBA;
    }
  else
    {
    itkExceptionMacro("OpenCV IO only supports Mono, RGB, and RGBA output");
    }
}

void OpenCVVideoIO::Write(const void *buffer)
{
  // Make sure parameters are specified
  if (this->m_FramesPerSecond == 0 || this->m_Dimensions.size() == 0 || this->m_FourCC == 0)
    {
    itkExceptionMacro("Can not write with empty parameters. You probably need to call SetWriterParameters");
    }

  // Make sure the number of channels is 1 or 3
  if (this->m_NumberOfComponents != 1 && this->m_NumberOfComponents != 3)
    {
    itkExceptionMacro("OpenCV only supports 1 and 3 component images. NumberOfComponents = "
      << this->m_NumberOfComponents);
    }

  // If the writer isn't open yet, open it
  if (!this->m_WriterOpen)
    {
    this->OpenWriter();
    }

  // Place the contents of the buffer into an OpenCV image
  if (this->m_CVImage == ITK_NULLPTR)
    {
    // The output image always has to be 3 components for the OpenCV writer
    this->m_CVImage = cvCreateImage( cvSize(this->m_Dimensions[0],this->m_Dimensions[1]),
                                     IPL_DEPTH_8U, 3 );
    }
  if (this->m_TempImage == ITK_NULLPTR)
    {
    this->m_TempImage = cvCreateImage( cvSize(this->m_Dimensions[0],this->m_Dimensions[1]),
                                       IPL_DEPTH_8U, this->m_NumberOfComponents );
    }

  // Insert the buffer into TempImage
  cvSetData(this->m_TempImage,
    reinterpret_cast<char*>(const_cast<void*>(buffer)), this->m_TempImage->widthStep);

  // Handle grayscale
  if (this->m_NumberOfComponents == 1)
    {
    cvCvtColor(this->m_TempImage, this->m_CVImage, CV_GRAY2BGR);
    }
  // Guaranteed to be 3 channels
  else
    {
    cvCvtColor(this->m_TempImage, this->m_CVImage, CV_RGB2BGR);
    }

  // Write the frame
  cvWriteFrame(this->m_Writer, this->m_CVImage);

  // Update the current frame
  this->m_CurrentFrame++;

}

void OpenCVVideoIO::UpdateReaderProperties()
{
  // 0-based index of the frame tobe decoded/captured next
  this->m_CurrentFrame =
    cvGetCaptureProperty(this->m_Capture, CV_CAP_PROP_POS_FRAMES);
  this->m_PositionInMSec =
    cvGetCaptureProperty(this->m_Capture, CV_CAP_PROP_POS_MSEC);
  this->m_FramesPerSecond = static_cast<double>
    (cvGetCaptureProperty( this->m_Capture, CV_CAP_PROP_FPS ));
  this->m_Ratio =
    cvGetCaptureProperty(this->m_Capture, CV_CAP_PROP_POS_AVI_RATIO);
  this->m_FourCC =
    cvGetCaptureProperty(this->m_Capture, CV_CAP_PROP_FOURCC);
}

void OpenCVVideoIO::OpenReader()
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
    this->m_Capture = cvCaptureFromFile( this->GetFileName() );
    if (this->m_Capture != ITK_NULLPTR)
      {
      this->m_ReaderOpen = true;
      }
    else
      {
      itkExceptionMacro("Video failed to open");
      }
    }
  else if (this->m_ReadType == ReadFromCamera)
    {
    this->m_Capture = cvCaptureFromCAM( this->m_CameraIndex );
    if (this->m_Capture != ITK_NULLPTR)
      {
      this->m_ReaderOpen = true;
      }
    else
      {
      itkExceptionMacro("Video failed to open");
      }
    }
}

void OpenCVVideoIO::OpenWriter()
{
  if (this->m_WriterOpen)
    {
    itkExceptionMacro("Can not open writer while video is already open for writing");
    }

  if (this->m_ReaderOpen)
    {
    itkExceptionMacro("Can not open writer while video is already open for reading");
    }

  // If neither reader nor writer is currently open, open the writer
  this->m_Writer = cvCreateVideoWriter( this->GetFileName(), this->m_FourCC,
    this->m_FramesPerSecond, cvSize(this->m_Dimensions[0],this->m_Dimensions[1]) );
  this->m_WriterOpen = true;
}

void OpenCVVideoIO::ResetMembers()
{
  this->m_CVImage = 0;
  this->m_TempImage = 0;
  this->m_Capture = 0;
  this->m_Writer = 0;
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

void OpenCVVideoIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  if (this->m_CVImage != ITK_NULLPTR)
    {
    os << indent << "Image dimensions : ["<< this->m_CVImage->width << ","
      << this->m_CVImage->height << "]" << std::endl;
    os << indent << "Origin : "<<this->m_CVImage->origin << std::endl;
    os << indent << "Image spacing (in bits) : "<< this->m_CVImage->depth
      << std::endl;
    os << indent << "Image Size : " << this->m_CVImage->imageSize << std::endl;
    os << indent << "Color model : " << this->m_CVImage->colorModel
      << " (" << this->m_NumberOfComponents << " channels)" << std::endl;
    }
}

} // end namespace itk
