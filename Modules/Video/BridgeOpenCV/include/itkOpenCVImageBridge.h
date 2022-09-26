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
#ifndef itkOpenCVImageBridge_h
#define itkOpenCVImageBridge_h

#include <string>

#include "itkImage.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkConvertPixelBuffer.h"

#include "opencv2/core/version.hpp"
#if !defined(CV_VERSION_EPOCH)
// OpenCV 3+
#  include "opencv2/imgproc.hpp"
#  include "opencv2/imgproc/types_c.h"   // CV_RGB2BGR, CV_BGR2GRAY, ...
#  include "opencv2/imgproc/imgproc_c.h" // cvCvtColor
#else
// OpenCV 2.4.x
#  include "cv.h"
#  include "highgui.h"
#endif

namespace itk
{

/**
 * \class OpenCVImageBridge
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
 *
 * \ingroup ITKVideoBridgeOpenCV
 */
class OpenCVImageBridge
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(OpenCVImageBridge);

  /** ITK stype type alias */
  using Self = OpenCVImageBridge;

  /** IplImage* -> itk::Image */
  template <typename TOutputImageType>
  static typename TOutputImageType::Pointer
  IplImageToITKImage(const IplImage * in);

  /** cv::Mat -> itk::Image */
  template <typename TOutputImageType>
  static typename TOutputImageType::Pointer
  CVMatToITKImage(const cv::Mat & in);

  /** itk::Image -> IplImage* */
  template <typename TInputImageType>
  static IplImage *
  ITKImageToIplImage(const TInputImageType * in, bool force3Channels = false);

  /** itk::Image -> cv::Mat */
  template <typename TInputImageType>
  static cv::Mat
  ITKImageToCVMat(const TInputImageType * in, bool force3Channels = false);

private:
  /** Steps involved in this method are:
    1) Handle converting between colorspaces
    2) Allocate the output image
    3) Create a copy of the current IplImage's buffer without any padding
    (slow but necessary)
    4) Copy the buffer and convert the pixels if necessary */
  template <typename TOutputImageType, typename TPixel>
  static void
  ITKConvertIplImageBuffer(const IplImage * in, TOutputImageType * out, unsigned int iDepth)
  {
    // Typedefs
    using ImageType = TOutputImageType;
    using OutputPixelType = typename ImageType::PixelType;

    unsigned int inChannels = in->nChannels;
    unsigned int outChannels = itk::NumericTraits<OutputPixelType>::MeasurementVectorType::Dimension;

    // Manage unsupported conversions.
    if ((inChannels != outChannels || (inChannels == 3 && outChannels == 3)) &&
        (iDepth == IPL_DEPTH_8S || iDepth == IPL_DEPTH_16S || iDepth == IPL_DEPTH_32S || iDepth == IPL_DEPTH_64F))
    {
      itkGenericExceptionMacro("OpenCV IplImage to ITK Image conversion - the necessary color"
                               " conversion is not supported for the input OpenCV pixel type");
    }

    // Manage input/output types mismatch
    checkMatchingTypes<TPixel, OutputPixelType>(outChannels);

    // We only change current if it no longer points at in, so this is safe
    IplImage * current = const_cast<IplImage *>(in);

    bool freeCurrent = false;
    if (inChannels == 3 && outChannels == 1)
    {
      current = cvCreateImage(cvSize(in->width, in->height), iDepth, 1);
      cvCvtColor(in, current, CV_BGR2GRAY);
      freeCurrent = true;
    }
    else if (inChannels == 1 && outChannels == 3)
    {
      current = cvCreateImage(cvSize(in->width, in->height), iDepth, 3);
      cvCvtColor(in, current, CV_GRAY2RGB);
      freeCurrent = true;
    }
    else if (inChannels == 3 && outChannels == 3)
    {
      current = cvCreateImage(cvSize(in->width, in->height), iDepth, 3);
      cvCvtColor(in, current, CV_BGR2RGB);
      freeCurrent = true;
    }
    else if (inChannels != 1 || outChannels != 1)
    {
      itkGenericExceptionMacro("Conversion from " << inChannels << " channels to " << outChannels
                                                  << " channels is not supported");
    }

    ITKConvertImageBuffer<TOutputImageType, TPixel>(
      current->imageData, out, current->nChannels, current->width, current->height, current->widthStep);

    if (freeCurrent)
    {
      cvReleaseImage(&current);
    }
  }

  template <typename TOutputImageType, typename TPixel>
  static void
  ITKConvertMatImageBuffer(const cv::Mat & in, TOutputImageType * out)
  {
    using namespace cv;

    // Typedefs
    using ImageType = TOutputImageType;
    using OutputPixelType = typename ImageType::PixelType;

    unsigned int inChannels = in.channels();
    unsigned int iDepth = in.depth();
    unsigned int outChannels = itk::NumericTraits<OutputPixelType>::MeasurementVectorType::Dimension;

    // Manage pixel/component types mismatch and impossible conversions.
    if ((inChannels != outChannels || (inChannels == 3 && outChannels == 3)) &&
        (iDepth == CV_8S || iDepth == CV_16S || iDepth == CV_32S || iDepth == CV_64F))
    {
      itkGenericExceptionMacro("OpenCV Mat to ITK Image conversion - the necessary color"
                               " conversion is not supported for the input OpenCV pixel type");
    }

    // Manage input/output types mismatch
    checkMatchingTypes<TPixel, OutputPixelType>(outChannels);

    Mat current;

    if (inChannels == 3 && outChannels == 1)
    {
      cvtColor(in, current, COLOR_BGR2GRAY);
    }
    else if (inChannels == 1 && outChannels == 3)
    {
      cvtColor(in, current, COLOR_GRAY2RGB);
    }
    else if (inChannels == 3 && outChannels == 3)
    {
      cvtColor(in, current, COLOR_BGR2RGB);
    }
    else if (inChannels != 1 || outChannels != 1)
    {
      itkGenericExceptionMacro("Conversion from " << inChannels << " channels to " << outChannels
                                                  << " channels is not supported");
    }
    else
    {
      current = in;
    }

    ITKConvertImageBuffer<TOutputImageType, TPixel>(
      reinterpret_cast<char *>(current.ptr()), out, current.channels(), current.cols, current.rows, current.step);
  }

  template <typename InputPixelType, typename OutputPixelType>
  static void
  checkMatchingTypes(unsigned int outChannels)
  {
    if (outChannels == 3)
    {
      if (typeid(RGBPixel<InputPixelType>) != typeid(OutputPixelType))
      {
        itkGenericExceptionMacro("OpenCV to ITK conversion channel component type mismatch");
      }
    }
    else if (typeid(InputPixelType) != typeid(OutputPixelType))
    {
      itkGenericExceptionMacro("OpenCV to ITK conversion pixel type mismatch");
    }
  }

  template <typename TOutputImageType, typename TPixel>
  static void
  ITKConvertImageBuffer(const char *       in,
                        TOutputImageType * out,
                        unsigned int       inChannels,
                        int                imgWidth,
                        int                imgHeight,
                        int                widthStep)
  {
    using ImageType = TOutputImageType;
    using OutputPixelType = typename ImageType::PixelType;
    using ConvertPixelTraits = DefaultConvertPixelTraits<OutputPixelType>;

    bool isVectorImage(strcmp(out->GetNameOfClass(), "VectorImage") == 0);

    typename ImageType::RegionType            region;
    typename ImageType::RegionType::SizeType  size;
    typename ImageType::RegionType::IndexType start;
    typename ImageType::SpacingType           spacing;
    size.Fill(1);
    size[0] = imgWidth;
    size[1] = imgHeight;
    start.Fill(0);
    spacing.Fill(1);
    region.SetSize(size);
    region.SetIndex(start);
    out->SetRegions(region);
    out->SetSpacing(spacing);
    out->Allocate();
    size_t       lineLength = imgWidth * inChannels * sizeof(TPixel);
    void *       unpaddedBuffer = reinterpret_cast<void *>(new TPixel[imgHeight * lineLength]);
    unsigned int paddedBufPos = 0;
    unsigned int unpaddedBufPos = 0;

    for (int i = 0; i < imgHeight; ++i)
    {
      memcpy(&(reinterpret_cast<char *>(unpaddedBuffer)[unpaddedBufPos]), in + paddedBufPos, lineLength);
      paddedBufPos += widthStep;
      unpaddedBufPos += lineLength;
    }

    if (isVectorImage)
    {
      ConvertPixelBuffer<TPixel, OutputPixelType, ConvertPixelTraits>::ConvertVectorImage(
        static_cast<TPixel *>(unpaddedBuffer),
        inChannels,
        out->GetPixelContainer()->GetBufferPointer(),
        out->GetPixelContainer()->Size());
    }
    else
    {
      ConvertPixelBuffer<TPixel, OutputPixelType, ConvertPixelTraits>::Convert(
        static_cast<TPixel *>(unpaddedBuffer),
        inChannels,
        out->GetPixelContainer()->GetBufferPointer(),
        out->GetPixelContainer()->Size());
    }

    delete[] reinterpret_cast<TPixel *>(unpaddedBuffer);
  }

  template <typename TPixel, unsigned int VDimension>
  struct HandleRGBPixel
  {
    static void
    Padding(const Image<TPixel, VDimension> * itkNotUsed(in), IplImage * itkNotUsed(out))
    {}
  };

  template <typename TValue, unsigned int VDimension>
  struct HandleRGBPixel<RGBPixel<TValue>, VDimension>
  {
    using ValueType = TValue;
    using PixelType = RGBPixel<ValueType>;
    using ImageType = Image<PixelType, VDimension>;

    static void
    Padding(const ImageType * in, IplImage * out)
    {
      typename ImageType::IndexType pixelIndex = { { 0, 0 } };

      for (int r = 0; r < out->height; ++r)
      {
        ValueType * ptr = reinterpret_cast<ValueType *>(out->imageData + r * out->widthStep);
        for (int c = 0; c < out->width; ++c)
        {
          pixelIndex[0] = c;
          pixelIndex[1] = r;
          typename ImageType::PixelType pixel = in->GetPixel(pixelIndex);

          for (unsigned int i = 0; i < 3; ++i)
          {
            *ptr++ = pixel[i];
          }
        }
      }
    }
  };
}; // end class OpenCVImageBridge

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkOpenCVImageBridge.hxx"
#endif

#endif
