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
#ifndef __itkOpenCVImageBridge_h
#define __itkOpenCVImageBridge_h

#include <string>

#include "itkImage.h"
#include "cv.h"
#include "highgui.h"

namespace itk
{

/** \class OpenCVImageBridge
 * \brief This class provides static methods to convert between OpenCV images
 * and itk::Image
 *
 * This class provides methods for the following conversions:
 *    IplImage -> itk::Image
 *    cv::Mat -> itk::Image
 *    itk::Image -> IplImage
 *    itk::Image -> cv::Mat
 *
 * Each method is templated over the type of itk::Image used.  The conversions
 * copy the data and convert between types if necessary.
 */
class OpenCVImageBridge
{
public:

  /** ITK stype typedefs */
  typedef OpenCVImageBridge Self;

  /** IplImage* -> itk::Image */
  template<class TOutputImageType>
  static typename TOutputImageType::Pointer IplImageToITKImage(const IplImage* in);

  /** cv::Mat -> itk::Image */
  template<class TOutputImageType>
  static typename TOutputImageType::Pointer CVMatToITKImage(const cv::Mat in);

  /** itk::Image -> IplImage* */
  template<class TInputImageType>
  static IplImage* ITKImageToIplImage(const TInputImageType* in);

  /** itk::Image -> cv::Mat */
  template<class TInputImageType>
  static cv::Mat ITKImageToCVMat(const TInputImageType* in);

private:
  OpenCVImageBridge(const Self &); //purposely not implemented
  void operator=(const Self &);    //purposely not implemented

};  // end class OpenCVImageBridge

} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkOpenCVImageBridge.txx"
#endif

#endif
