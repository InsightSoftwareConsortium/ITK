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
#ifndef itkOpenCVVideoCapture_hxx
#define itkOpenCVVideoCapture_hxx

#include "itkOpenCVVideoCapture.h"
#include "itkNumericTraits.h"
#include "itkImageRegionConstIteratorWithIndex.h"

#include "opencv2/core/version.hpp"
#if !defined(CV_VERSION_EPOCH)
// OpenCV 3.x
#include "opencv2/videoio.hpp"
#include "opencv2/imgproc/types_c.h" // CV_RGB2BGR, CV_BGR2GRAY, ...
#else
// OpenCV 2.4.x
#include "cv.h"
#endif

namespace itk
{

//-CONSTRUCTORS----------------------------------------------------------------

//
// OpenCVVideoCapture()
//
template<typename TVideoStream>
OpenCVVideoCapture<TVideoStream>::OpenCVVideoCapture()
{
  m_VideoStream = ITK_NULLPTR;

  // Default to reporting 24fps. This may change once RealTime is implemented
  m_FpS = 24;

  // Default to mp4.2
  m_FourCC = CV_FOURCC('M','P','4','2');
}

//
// OpenCVVideoCapture(videoStream)
//
template<typename TVideoStream>
OpenCVVideoCapture<TVideoStream>::OpenCVVideoCapture(TVideoStream* videoStream)
{
  m_VideoStream = videoStream;

  // Default to reporting 24fps. This may change once RealTime is implemented
  m_FpS = 24;

  // Default to mp4.2
  m_FourCC = CV_FOURCC('M','P','4','2');
}


//-OPEN CLOSE FUNCTIONALITY----------------------------------------------------

//
// open(videoStream)
//
template<typename TVideoStream>
bool OpenCVVideoCapture<TVideoStream>::open(TVideoStream* videoStream)
{
  // We only support 2D
  if (this->Dimensions != 2)
    {
    return false;
    }

  // Make sure pointer isn't null
  if (!videoStream)
    {
    return false;
    }
  m_VideoStream = videoStream;
  return true;
}

//
// release
//
template<typename TVideoStream>
void OpenCVVideoCapture<TVideoStream>::release()
{
  m_VideoStream = ITK_NULLPTR;
}


//-FRAME ACCESS----------------------------------------------------------------

//
// grab
//
template<typename TVideoStream>
bool OpenCVVideoCapture<TVideoStream>::grab()
{
  // We only support 2D
  if (this->Dimensions != 2)
    {
    return false;
    }

  // Move the current frame forward by 1
  SizeValueType currentFrame = m_VideoStream->GetRequestedTemporalRegion().GetFrameStart();
  return set(CV_CAP_PROP_POS_FRAMES, currentFrame + 1);
}

//
// retrieve
//
template<typename TVideoStream>
bool OpenCVVideoCapture<TVideoStream>::retrieve(cv::Mat& image, int itkNotUsed(channel))
{
  // We only support 2D
  if (this->Dimensions != 2)
    {
    image.release();
    return false;
    }

  // Make sure VideoStream isn't null
  if (!m_VideoStream)
    {
    image.release();
    return false;
    }

  // Get the requested frame
  SizeValueType frameNum = m_VideoStream->GetRequestedTemporalRegion().GetFrameStart();
  FrameType* frame = m_VideoStream->GetFrame(frameNum);

  // Make sure frame isn't null
  if (!frame)
    {
    image.release();
    return false;
    }

  // Instantiate the cv::Mat with the proper size and type
  typename FrameType::SizeType size = frame->GetLargestPossibleRegion().GetSize();
  unsigned int depth = cv::DataDepth< typename itk::NumericTraits< PixelType >::ValueType >::value;
  unsigned int channels = itk::NumericTraits< PixelType >::MeasurementVectorType::Dimension;
  int matrixType = CV_MAKETYPE(depth, channels);

  // Copy the pixels -- There is probably a faster way to do this
  typedef itk::ImageRegionConstIteratorWithIndex<FrameType> ITKIterType;
  ITKIterType itkIter(frame, frame->GetLargestPossibleRegion());

  // Currently only support mono and RGB (unsigned) char pixels
  IplImage* iplImg = cvCreateImage( cvSize(size[0], size[1]), IPL_DEPTH_8U, channels );
  if (matrixType == CV_8UC1 || matrixType == CV_8SC1)
    {
    // Insert the buffer into iplImg
    cvSetData(iplImg,
      reinterpret_cast<char*>(frame->GetBufferPointer()),
      iplImg->widthStep);
    }
  else if (matrixType == CV_8UC3 || matrixType == CV_8SC3)
    {
    // Place the contents of the buffer into an OpenCV image
    IplImage* tempImg = cvCreateImage( cvSize(size[0], size[1]), IPL_DEPTH_8U, channels );

    // Insert the buffer into tempImg
    cvSetData(tempImg,
      reinterpret_cast<char*>(frame->GetBufferPointer()),
      tempImg->widthStep);

    // Convert to BGR
    cvCvtColor(tempImg, iplImg, CV_RGB2BGR);
    }
  else
    {
    itkExceptionMacro("itk::OpenCVVideoCaptures -> Pixel type not supported");
    }

  // Pass off to the Mat
  image.create(size[0], size[1], matrixType);
  image = cv::cvarrToMat(iplImg, true);

  // Return success
  return true;
}

//
// operator >>
//
template<typename TVideoStream>
OpenCVVideoCapture<TVideoStream>& OpenCVVideoCapture<TVideoStream>::operator >>(cv::Mat& image)
{
  if (!read(image))
    {
    image.release();
    }
  return *this;
}

//
// read
//
template<typename TVideoStream>
bool OpenCVVideoCapture<TVideoStream>::read(cv::Mat& image)
{
  if (!grab())
    {
    image.release();
    return false;
    }
  else
    {
    return retrieve(image);
    }
}


//-PROPERTIES------------------------------------------------------------------

//
// set
//
template<typename TVideoStream>
bool OpenCVVideoCapture<TVideoStream>::set(int propId, double value)
{
  // Make sure the VideoSource isn't null
  if (!m_VideoStream)
    {
    return false;
    }

  // Variables for use in cases
  SizeValueType maxFrame;
  SizeValueType nextFrame;
  SizeValueType ratioFrameOffset;
  SizeValueType newFrame;
  TemporalRegion largestPossible;
  TemporalRegion newRequest;

  // Handle all of the different properties
  switch (propId)
    {
    // Figure out the frame numbers from the value -- This is not currently supported
    case CV_CAP_PROP_POS_MSEC:
      itkExceptionMacro("OpenCVVideoCapture: Video Pipeline does not currently support RealTime");
      break;

    // Set the frame start of the requested region and update the pipeline
    case CV_CAP_PROP_POS_FRAMES:

      // Get the largest temporal region and make sure it has some duration
      m_VideoStream->UpdateOutputInformation();
      largestPossible = m_VideoStream->GetLargestPossibleTemporalRegion();

      if (!largestPossible.GetFrameDuration())
        {
        return false;
        }

      // Verify that we aren't trying to skip past the end of the video
      maxFrame =
        largestPossible.GetFrameStart() + largestPossible.GetFrameDuration() - 1;
      nextFrame = (long)value;
      if (nextFrame > maxFrame)
        {
        return false;
        }

      // Move the requested temporal region
      newRequest.SetFrameStart(nextFrame);
      newRequest.SetFrameDuration(1);
      m_VideoStream->SetRequestedTemporalRegion(newRequest);

      // Make sure we're requesting the largest possible spatial region
      m_VideoStream->SetFrameRequestedSpatialRegion(nextFrame,
        m_VideoStream->GetFrameLargestPossibleSpatialRegion(nextFrame));

      // Update the pipeline
      m_VideoStream->Update();
      break;

    // Figure out the frame number from the AVI ratio and set accordingly
    case CV_CAP_PROP_POS_AVI_RATIO:

      // Compute the new frame
      m_VideoStream->UpdateOutputInformation();
      largestPossible = m_VideoStream->GetLargestPossibleTemporalRegion();
      ratioFrameOffset = (SizeValueType)((double)largestPossible.GetFrameDuration() * value);
      newFrame = largestPossible.GetFrameStart() + ratioFrameOffset;

      // Use the CV_CAP_PROP_POS_FRAMES property to update
      set(CV_CAP_PROP_POS_FRAMES, newFrame);
      break;

    // Set FourCC
    case CV_CAP_PROP_FOURCC:
      m_FourCC = (int)value;
      break;

    // Set FpS
    case CV_CAP_PROP_FPS:
      m_FpS = value;
      break;

    // We don't support setting anything else
    default:
      return false;
    }

  // Return success
  return true;
}

//
// get
//
template<typename TVideoStream>
double OpenCVVideoCapture<TVideoStream>::get(int propId)
{
  // Make sure the VideoSource isn't null
  if (!m_VideoStream)
    {
    return 0;
    }

  // Variables for use in cases
  SizeValueType frameNum = m_VideoStream->GetRequestedTemporalRegion().GetFrameStart();
  SizeValueType currentOffset;
  TemporalRegion largest;
  TemporalRegion requested;

  // Handle all of the different properties
  switch (propId)
    {
    // Figure out the frame numbers from the value -- This is not currently supported
    case CV_CAP_PROP_POS_MSEC:
      itkExceptionMacro("OpenCVVideoCapture: Video Pipeline does not currently support RealTime");
      break;

    // Get the frame start of the requested region and update the pipeline
    case CV_CAP_PROP_POS_FRAMES:
      return frameNum;

    // Figure out the frame number from the AVI ratio and set accordingly
    case CV_CAP_PROP_POS_AVI_RATIO:

      // Compute the frame number from the ratio
      m_VideoStream->UpdateOutputInformation();
      largest = m_VideoStream->GetLargestPossibleTemporalRegion();
      requested = m_VideoStream->GetRequestedTemporalRegion();
      currentOffset = requested.GetFrameStart() - largest.GetFrameStart();
      return (double)currentOffset / (double)largest.GetFrameDuration();

    // Get FourCC
    case CV_CAP_PROP_FOURCC:
      return m_FourCC;

    // Set FpS
    case CV_CAP_PROP_FPS:
      return m_FpS;

    // Get Frame Width
    case CV_CAP_PROP_FRAME_WIDTH:
      m_VideoStream->UpdateOutputInformation();
      return m_VideoStream->GetFrameLargestPossibleSpatialRegion(frameNum).GetSize()[0];

    // Get Frame Height
    case CV_CAP_PROP_FRAME_HEIGHT:
      m_VideoStream->UpdateOutputInformation();
      return m_VideoStream->GetFrameLargestPossibleSpatialRegion(frameNum).GetSize()[1];

    // We don't support getting anything else
    default:
      return false;
    }

  // Return success
  return true;
}

} // end namespace itk

#endif
