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
#ifndef __itkOpenCVVideoCapture_h
#define __itkOpenCVVideoCapture_h

#include <string>

#include "highgui.h"
#include "itkVideoStream.h"

namespace itk
{

/** \class VideoCapture
 * \brief This class implements OpenCV's VideoCapture API and takes an itk
 * VideoStream as input
 */
class OpenCVVideoCapture : public cv::VideoCapture
{
public:

  /**-CONSTRUCTORS AND DESTRUCTOR--------------------------------------------*/

  /** Constructor that initializes internal VideoStream to null */
  OpenCVVideoCapture();

  /** Constructor that takes a TemporalDataObject as input. It checks to make
   * sure that the object can be cast to a VideoStream */
  OpenCVVideoCapture(TemporalDataObject* videoStream);

  /** Destructor that does nothing. The VideoStream will be freed by the source
   * that generated it. */
  virtual ~OpenCVVideoCapture() {}

  /** ITK's type info */
  typedef OpenCVVideoCapture Self;
  itkTypeMacro(OpenCVVideoCapture, cv::VideoCapture);


  /**-OPEN CLOSE FUNCTIONALITY-----------------------------------------------*/

  /** overload reading from file and camera just to throw exceptions */
  virtual bool open(const std::string&)
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
  virtual bool open(TemporalDataObject* videoStream);

  /** Check if the VideoStream is null */
  virtual bool isOpened() { return m_VideoStream == 0; }

  /** Force the VideoStream's source (if it exists) to detach it's output and
   * free VideoStream's data */
  virtual void release();


  /**-FRAME ACCESS-----------------------------------------------------------*/

  /** Grab the next frame from the VideoStream */
  virtual bool grab();

  /** Access the current frame of the VideoStream */
  virtual bool retrieve(cv::Mat& image, int channel=0);

  /** Stream the next frame into the provided image.
   * Equivalent to grab() + retrieve(image, 0) */
  virtual Self& operator >> (cv::Mat& image);

  /** non-operator version of >>'s functionality */
  virtual bool read(cv::Mat& image);


  /**-PROPERTIES-------------------------------------------------------------*/

  /** Set a property */
  virtual bool set(int propId, double value);

  /** Get a property */
  virtual double get(int propId);

protected:

  /** We store the pointer to the internal VideoStream as a TemporalDataObject
   * to avoid the need for templating. Internally, we always cast to
   * VideoStream using the apropriate primitive OpenCV type */
  TemporalDataObject* m_VideoStream;

};  // end class VideoCapture

} // end namespace itk

#endif
