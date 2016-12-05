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
#ifndef itkOpenCVVideoCapture_h
#define itkOpenCVVideoCapture_h

#include <string>

// Include the required header with OpenCV > 2.X
#include "opencv2/core/version.hpp"
#if !defined( CV_VERSION_EPOCH )
// OpenCV 3.x
#include "opencv2/videoio.hpp"
#else
// OpenCV 2.4.x
#include "highgui.h"
#endif

#include "itkVideoStream.h"

namespace itk
{

/** \class OpenCVVideoCapture
 * \brief This class implements OpenCV's VideoCapture API and takes an itk
 * VideoStream as input
 *
 * This implementation of cv::VideoCapture provides the additional method
 * open( itk::VideoStream* ) to "open" video data from an ITK pipeline. The
 * traditional open methods will throw exceptions, so it can only be used to
 * access the output of an ITK video pipeline from an OpenCV capture context.
 *
 * \ingroup ITKVideoBridgeOpenCV
 */
template <typename TVideoStream>
class ITK_TEMPLATE_EXPORT OpenCVVideoCapture : public cv::VideoCapture
{
public:

  /**-CONSTRUCTORS AND DESTRUCTOR--------------------------------------------*/

  /** ITK stype typedefs */
  typedef TVideoStream                        VideoStreamType;
  typedef OpenCVVideoCapture<VideoStreamType> Self;
  typedef typename VideoStreamType::FrameType FrameType;
  typedef typename FrameType::PixelType       PixelType;
  static ITK_CONSTEXPR_VAR unsigned int Dimensions =        FrameType::ImageDimension;
  /** Constructor that initializes internal VideoStream to null */
  OpenCVVideoCapture();

  /** Constructor that takes a VideoStream as input */
  OpenCVVideoCapture(VideoStreamType* videoStream);

  /** Destructor that does nothing. The VideoStream will be freed by the source
   * that generated it. */
  virtual ~OpenCVVideoCapture()
  {
  }

  /** ITK's type info */
  itkTypeMacroNoParent(OpenCVVideoCapture);

  /**-OPEN CLOSE FUNCTIONALITY-----------------------------------------------*/

  /** overload reading from file and camera just to throw exceptions */
  virtual bool open(const std::string &)
  {
    itkExceptionMacro("itk::OpenCVVideoCapture::open(filename) -> If you just want "
                      "to read from a file, use cv::VideoCapture since there is nothing to be "
                      "gained using itk's version.");
  }

  virtual bool open(int)
  {
    itkExceptionMacro("itk::OpenCVVideoCapture::open(device) -> If you just want "
                      "to read from a device, use cv::VideoCapture since there is nothing to be "
                      "gained using itk's version.");
  }

  /** Add an open method that takes a TemporalDataObject. This checks to make
   * sure that it can be cast to a VideoStream */
  virtual bool open(VideoStreamType* videoStream);

  /** Check if the VideoStream is null */
  virtual bool isOpened() const
  {
    return m_VideoStream == 0;
  }

  /** Just set the internal pointer to null. Let the upstream filters take care
   * of actually freeing the memory */
  virtual void release();

  /**-FRAME ACCESS-----------------------------------------------------------*/

  /** Grab the next frame from the VideoStream */
  virtual bool grab();

  /** Access the current frame of the VideoStream */
  virtual bool retrieve(cv::Mat & image, int channel = 0);

  /** Stream the next frame into the provided image.
   * Equivalent to grab() + retrieve(image, 0) */
  virtual Self & operator >>(cv::Mat& image);

  /** non-operator version of >>'s functionality */
  virtual bool read(cv::Mat& image);

  /**-PROPERTIES-------------------------------------------------------------*/

  /** Set a property */
  virtual bool set(int propId, double value);

  /** Get a property */
  virtual double get(int propId);

protected:

  /** Internal VideoStream */
  VideoStreamType* m_VideoStream;

  /** Property members */
  double m_FpS;
  int    m_FourCC;

};  // end class VideoCapture

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOpenCVVideoCapture.hxx"
#endif

#endif
