/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkOpenCVImageBridge_hxx
#define itkOpenCVImageBridge_hxx

#include "itkOpenCVImageBridge.h"
#include "itkNumericTraits.h"

#if !defined(CV_VERSION_EPOCH)
// OpenCV 3.x
#  include "opencv2/imgproc/imgproc_c.h" // cvCvtColor, CV_RGB2BGR, ...
#endif

namespace itk
{
// IplImageToITKImage
template <typename TOutputImageType>
typename TOutputImageType::Pointer
OpenCVImageBridge::IplImageToITKImage(const IplImage * in)
{
  // Typedefs
  using ImageType = TOutputImageType;

  // IplImage::depth is defined as a int, but it is treated as a unsigned
  // integer bit field which causes a compilation error in clang 6.0 when
  // trying to implicit cast some IPL_DEPTH_* values
  using DepthIDType = unsigned int;

  // Make sure input isn't null and output type is 2D or 1D
  if (!in)
  {
    itkGenericExceptionMacro("Input is nullptr");
  }

  // Do the conversion
  typename ImageType::Pointer out = ImageType::New();

#define CONVERSION_CASE(iplInputDepthID, itkOutputPixelType)                                                           \
  case (iplInputDepthID):                                                                                              \
  {                                                                                                                    \
    static_assert((iplInputDepthID) <= NumericTraits<DepthIDType>::max() &&                                            \
                    (iplInputDepthID) >= NumericTraits<DepthIDType>::min(),                                            \
                  "Invalid IPL depth ID: " #iplInputDepthID);                                                          \
    ITKConvertIplImageBuffer<ImageType, itkOutputPixelType>(in, out.GetPointer(), (iplInputDepthID));                  \
    break;                                                                                                             \
  }

  switch (static_cast<DepthIDType>(in->depth))
  {
    CONVERSION_CASE(IPL_DEPTH_8U, unsigned char)
    CONVERSION_CASE(IPL_DEPTH_8S, char)
    CONVERSION_CASE(IPL_DEPTH_16U, unsigned short)
    CONVERSION_CASE(IPL_DEPTH_16S, short)
    CONVERSION_CASE(IPL_DEPTH_32S, int)
    CONVERSION_CASE(IPL_DEPTH_32F, float)
    CONVERSION_CASE(IPL_DEPTH_64F, double)
    default:
    {
      itkGenericExceptionMacro("Unknown OpenCV type");
    }
  }

#undef CONVERSION_CASE

  return out;
}

// CVMatToITKImage
template <typename TOutputImageType>
typename TOutputImageType::Pointer
OpenCVImageBridge::CVMatToITKImage(const cv::Mat & in)
{
  using namespace cv;

  using ImageType = TOutputImageType;
  using DepthIDType = int;

  typename ImageType::Pointer out = ImageType::New();

#define CONVERSION_CASE(inputDepthID, itkOutputPixelType)                                                              \
  case (inputDepthID):                                                                                                 \
  {                                                                                                                    \
    static_assert((inputDepthID) <= NumericTraits<DepthIDType>::max() &&                                               \
                    (inputDepthID) >= NumericTraits<DepthIDType>::min(),                                               \
                  "Invalid Mat depth ID: " #inputDepthID);                                                             \
    ITKConvertMatImageBuffer<ImageType, itkOutputPixelType>(in, out.GetPointer());                                     \
    break;                                                                                                             \
  }

  switch (static_cast<DepthIDType>(in.depth()))
  {
    CONVERSION_CASE(CV_8U, unsigned char)
    CONVERSION_CASE(CV_8S, char)
    CONVERSION_CASE(CV_16U, unsigned short)
    CONVERSION_CASE(CV_16S, short)
    CONVERSION_CASE(CV_32S, int)
    CONVERSION_CASE(CV_32F, float)
    CONVERSION_CASE(CV_64F, double)
    default:
    {
      itkGenericExceptionMacro("Unknown OpenCV type");
    }
  }

#undef CONVERSION_CASE

  return out;
}


// ITKImageToIplImage
template <typename TInputImageType>
IplImage *
OpenCVImageBridge::ITKImageToIplImage(const TInputImageType * in, bool force3Channels)
{
  // Typedefs
  using ImageType = TInputImageType;
  using InputPixelType = typename ImageType::PixelType;
  using ValueType = typename itk::NumericTraits<InputPixelType>::ValueType;

  // Make sure input isn't null, is 2D or 1D, and is scalar or RGB
  if (!in)
  {
    itkGenericExceptionMacro("Input is nullptr");
  }

  typename ImageType::RegionType region = in->GetLargestPossibleRegion();
  typename ImageType::SizeType   size = region.GetSize();

  if (ImageType::ImageDimension > 2)
  {
    bool IsA2DImage = false;
    for (unsigned int dim = 2; (dim < ImageType::ImageDimension) && !IsA2DImage; dim++)
    {
      if (size[dim] != 1)
      {
        IsA2DImage = true;
      }
    }
    if (IsA2DImage)
    {
      itkGenericExceptionMacro("OpenCV only supports 2D and 1D images");
    }
  }
  unsigned int inChannels = itk::NumericTraits<InputPixelType>::MeasurementVectorType::Dimension;
  if (inChannels != 1 && inChannels != 3)
  {
    itkGenericExceptionMacro("OpenCV only supports scalar and 3-channel data");
  }

  unsigned int outChannels = inChannels;
  if (force3Channels)
  {
    outChannels = 3;
  }

  // Set up the output image
  IplImage *   out;
  unsigned int w = static_cast<unsigned int>(size[0]);
  unsigned int h = static_cast<unsigned int>(size[1]);

  // set the depth correctly based on input pixel type
  unsigned int typeSize = 1;
  if (typeid(ValueType) == typeid(unsigned char))
  {
    out = cvCreateImage(cvSize(w, h), IPL_DEPTH_8U, outChannels);
    typeSize = IPL_DEPTH_8U / 8;
  }
  else if (typeid(ValueType) == typeid(char))
  {
    if (outChannels != 1)
    {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type char");
    }
    out = cvCreateImage(cvSize(w, h), IPL_DEPTH_8S, outChannels);
    typeSize = IPL_DEPTH_8U / 8;
  }
  else if (typeid(ValueType) == typeid(unsigned short))
  {
    out = cvCreateImage(cvSize(w, h), IPL_DEPTH_16U, outChannels);
    typeSize = IPL_DEPTH_16U / 8;
  }
  else if (typeid(ValueType) == typeid(short))
  {
    if (outChannels != 1)
    {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type short");
    }
    out = cvCreateImage(cvSize(w, h), IPL_DEPTH_16S, outChannels);
    typeSize = IPL_DEPTH_16U / 8;
  }
  else if (typeid(ValueType) == typeid(float))
  {
    out = cvCreateImage(cvSize(w, h), IPL_DEPTH_32F, outChannels);
    typeSize = IPL_DEPTH_32F / 8;
  }
  else if (typeid(ValueType) == typeid(int))
  {
    if (outChannels != 1)
    {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type int");
    }
    out = cvCreateImage(cvSize(w, h), IPL_DEPTH_32S, outChannels);
    typeSize = IPL_DEPTH_32F / 8;
  }
  else if (typeid(ValueType) == typeid(double))
  {
    if (outChannels != 1)
    {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type double");
    }
    out = cvCreateImage(cvSize(w, h), IPL_DEPTH_64F, outChannels);
    typeSize = IPL_DEPTH_64F / 8;
  }
  else
  {
    itkGenericExceptionMacro("OpenCV does not support the input pixel type");
  }

  // Scalar output
  if (outChannels == 1)
  {
    size_t paddedRowBytes = typeSize * out->width;
    for (int i = 0; i < out->height; i++)
    {
      memcpy(out->imageData + i * out->widthStep, in->GetBufferPointer() + i * out->width, paddedRowBytes);
    }
  }
  // RGB output
  else
  {
    // Set up an IplImage ponting at the input's buffer. It's ok to do the
    // const cast because it will only get used to copy pixels
    if (inChannels == 3)
    {
      IplImage * temp = cvCreateImage(cvSize(w, h), out->depth, inChannels);
      HandleRGBPixel<InputPixelType, ImageType::ImageDimension>::Padding(in, temp);
      cvCvtColor(temp, out, CV_RGB2BGR);
      cvReleaseImage(&temp);
    }
    // input 1 channel, but forcing 3 channel output
    else
    {
      IplImage * temp = cvCreateImage(cvSize(w, h), out->depth, inChannels);
      temp->imageData = reinterpret_cast<char *>(const_cast<InputPixelType *>(in->GetBufferPointer()));
      cvCvtColor(temp, out, CV_GRAY2BGR);
      cvReleaseImage(&temp);
    }
  }

  return out;
}

// ITKImageToIplImage
template <typename TInputImageType>
cv::Mat
OpenCVImageBridge::ITKImageToCVMat(const TInputImageType * in, bool force3Channels)
{
  using namespace cv;

  // Typedefs
  using ImageType = TInputImageType;
  using InputPixelType = typename ImageType::PixelType;
  using ValueType = typename itk::NumericTraits<InputPixelType>::ValueType;

  // Make sure input isn't null, is 2D or 1D, and is scalar or RGB
  if (!in)
  {
    itkGenericExceptionMacro("Input is nullptr");
  }

  typename ImageType::RegionType region = in->GetLargestPossibleRegion();
  typename ImageType::SizeType   size = region.GetSize();

  if (ImageType::ImageDimension > 2)
  {
    bool IsA2DImage = false;
    for (unsigned int dim = 2; (dim < ImageType::ImageDimension) && !IsA2DImage; dim++)
    {
      if (size[dim] != 1)
      {
        IsA2DImage = true;
      }
    }
    if (IsA2DImage)
    {
      itkGenericExceptionMacro("OpenCV only supports 2D and 1D images");
    }
  }

  unsigned int inChannels = itk::NumericTraits<InputPixelType>::MeasurementVectorType::Dimension;
  if (inChannels != 1 && inChannels != 3)
  {
    itkGenericExceptionMacro("OpenCV only supports scalar and 3-channel data");
  }

  unsigned int outChannels = inChannels;

  // Set up the output image
  Mat          tmp;
  unsigned int w = static_cast<unsigned int>(size[0]);
  unsigned int h = static_cast<unsigned int>(size[1]);

  // set the depth correctly based on input pixel type
  if (typeid(ValueType) == typeid(unsigned char))
  {
    tmp = Mat(h,
              w,
              CV_8UC(outChannels),
              reinterpret_cast<unsigned char *>(const_cast<InputPixelType *>(in->GetBufferPointer())));
  }
  else if (typeid(ValueType) == typeid(char))
  {
    if (outChannels != 1 || force3Channels)
    {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type char");
    }
    tmp = Mat(h, w, CV_8SC1, reinterpret_cast<unsigned char *>(const_cast<InputPixelType *>(in->GetBufferPointer())));
  }
  else if (typeid(ValueType) == typeid(unsigned short))
  {
    tmp = Mat(h,
              w,
              CV_16UC(outChannels),
              reinterpret_cast<unsigned char *>(const_cast<InputPixelType *>(in->GetBufferPointer())));
  }
  else if (typeid(ValueType) == typeid(short))
  {
    if (outChannels != 1 || force3Channels)
    {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type short");
    }
    tmp = Mat(h, w, CV_16SC1, reinterpret_cast<unsigned char *>(const_cast<InputPixelType *>(in->GetBufferPointer())));
  }
  else if (typeid(ValueType) == typeid(float))
  {
    tmp = Mat(h,
              w,
              CV_32FC(outChannels),
              reinterpret_cast<unsigned char *>(const_cast<InputPixelType *>(in->GetBufferPointer())));
  }
  else if (typeid(ValueType) == typeid(int))
  {
    if (outChannels != 1 || force3Channels)
    {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type int");
    }
    tmp = Mat(h, w, CV_32SC1, reinterpret_cast<unsigned char *>(const_cast<InputPixelType *>(in->GetBufferPointer())));
  }
  else if (typeid(ValueType) == typeid(double))
  {
    if (outChannels != 1 || force3Channels)
    {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type double");
    }
    tmp = Mat(h, w, CV_64FC1, reinterpret_cast<unsigned char *>(const_cast<InputPixelType *>(in->GetBufferPointer())));
  }
  else
  {
    itkGenericExceptionMacro("OpenCV does not support the input pixel type");
  }

  Mat out;
  if (inChannels == 3)
  {
    cvtColor(tmp, out, COLOR_RGB2BGR);
  }
  else if (inChannels == 1 && force3Channels)
  {
    cvtColor(tmp, out, COLOR_GRAY2BGR);
  }
  else
  {
    tmp.copyTo(out);
  }

  return out;
}

} // end namespace itk

#endif
