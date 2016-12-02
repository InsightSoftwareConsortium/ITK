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
#ifndef itkOpenCVImageBridge_hxx
#define itkOpenCVImageBridge_hxx

#include "itkOpenCVImageBridge.h"
#include "itkNumericTraits.h"

#if !defined(CV_VERSION_EPOCH)
// OpenCV 3.x
#include "opencv2/imgproc/imgproc_c.h" // cvCvtColor, CV_RGB2BGR, ...
#endif

namespace itk
{
//
// IplImageToITKImage
//
template<typename TOutputImageType>
typename TOutputImageType::Pointer
OpenCVImageBridge::IplImageToITKImage(const IplImage* in)
{
  // Typedefs
  typedef TOutputImageType                           ImageType;

  //
  // Make sure input isn't null and output type is 2D or 1D
  //
  if (!in)
    {
    itkGenericExceptionMacro("Input is ITK_NULLPTR");
    }

  //
  // Do the conversion
  //
  typename ImageType::Pointer out = ImageType::New();

  switch (in->depth)
    {
    case (IPL_DEPTH_8U):
      {
      ITKConvertIplImageBuffer< ImageType, unsigned char >( in, out.GetPointer(), IPL_DEPTH_8U );
      break;
      }
    case (IPL_DEPTH_8S):
      {
      ITKConvertIplImageBuffer< ImageType, char >( in, out.GetPointer(), IPL_DEPTH_8S );
      break;
      }
    case (IPL_DEPTH_16U):
      {
      ITKConvertIplImageBuffer< ImageType, unsigned short >( in, out.GetPointer(), IPL_DEPTH_16U );
      break;
      }
    case (IPL_DEPTH_16S):
      {
      ITKConvertIplImageBuffer< ImageType, short >( in, out.GetPointer(), IPL_DEPTH_16S );
      break;
      }
    case (IPL_DEPTH_32F):
      {
      ITKConvertIplImageBuffer< ImageType, float >( in, out.GetPointer(), IPL_DEPTH_32F );
      break;
      }
    case (IPL_DEPTH_64F):
      {
      ITKConvertIplImageBuffer< ImageType, double >( in, out.GetPointer(), IPL_DEPTH_64F );
      break;
      }
    default:
      {
      itkGenericExceptionMacro("Unknown OpenCV type");
      }
    }

  //
  // Return the converted image
  //
  return out;
}

//
// CVMatToITKImage
//
template<typename TOutputImageType>
typename TOutputImageType::Pointer
OpenCVImageBridge::CVMatToITKImage(const cv::Mat & in)
{
  const IplImage converted = in;
  return IplImageToITKImage<TOutputImageType>(&converted);
}

//
// ITKImageToIplImage
//
template<typename TInputImageType>
IplImage*
OpenCVImageBridge::ITKImageToIplImage(const TInputImageType* in, bool force3Channels)
{
  // Typedefs
  typedef TInputImageType                                        ImageType;
  typedef typename ImageType::PixelType                          InputPixelType;
  typedef typename itk::NumericTraits<InputPixelType>::ValueType ValueType;

  //
  // Make sure input isn't null, is 2D or 1D, and is scalar or RGB
  //
  if (!in)
    {
    itkGenericExceptionMacro("Input is ITK_NULLPTR");
    }

  typename ImageType::RegionType  region = in->GetLargestPossibleRegion();
  typename ImageType::SizeType    size = region.GetSize();

  if (ImageType::ImageDimension > 2)
    {
    bool IsA2DImage = false;
    for( unsigned int dim = 2; ( dim < ImageType::ImageDimension) && !IsA2DImage;dim++ )
      {
      if( size[dim] != 1 )
        {
        IsA2DImage= true;
        }
      }
    if( IsA2DImage )
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

  //
  // Set up the output image
  //
  IplImage* out;
  unsigned int w = static_cast< unsigned int >( size[0] );
  unsigned int h = static_cast< unsigned int >( size[1] );

  //
  // set the depth correctly based on input pixel type
  //
  unsigned int typeSize = 1;
  if (typeid(ValueType) == typeid(unsigned char))
    {
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_8U, outChannels);
    typeSize = IPL_DEPTH_8U/8;
    }
  else if (typeid(ValueType) == typeid(char))
    {
    if (outChannels != 1)
      {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type char");
      }
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_8S, outChannels);
    typeSize = IPL_DEPTH_8U/8;
    }
  else if (typeid(ValueType) == typeid(unsigned short))
    {
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_16U, outChannels);
    typeSize = IPL_DEPTH_16U/8;
    }
  else if (typeid(ValueType) == typeid(short))
    {
    if (outChannels != 1)
      {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type short");
      }
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_16S, outChannels);
    typeSize = IPL_DEPTH_16U/8;
    }
  else if (typeid(ValueType) == typeid(float))
    {
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_32F, outChannels);
    typeSize = IPL_DEPTH_32F/8;
    }
  else if (typeid(ValueType) == typeid(double))
    {
    if (outChannels != 1)
      {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type double");
      }
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_64F, outChannels);
    typeSize = IPL_DEPTH_64F/8;
    }
  else
    {
    itkGenericExceptionMacro("OpenCV does not support the input pixel type");
    }

  // Scalar output
  if (outChannels == 1)
    {
    size_t paddedRowBytes = typeSize * out->width;
    for (int i=0;i<out->height;i++)
       {
        memcpy( out->imageData + i*out->widthStep,
             in->GetBufferPointer() + i*out->width,
             paddedRowBytes);
       }
    }
  // RGB output
  else
    {
    // Set up an IplImage ponting at the input's buffer. It's ok to do the
    // const cast because it will only get used to copy pixels
    if (inChannels == 3 )
      {
      IplImage * temp = cvCreateImage(cvSize(w,h),out->depth, inChannels);
      HandleRGBPixel< InputPixelType, ImageType::ImageDimension >::Padding( in, temp );
      cvCvtColor(temp, out, CV_RGB2BGR);
      cvReleaseImage(&temp);
      }
    // input 1 channel, but forcing 3 channel output
    else
      {
      IplImage* temp = cvCreateImage(cvSize(w,h), out->depth, inChannels);
      temp->imageData = reinterpret_cast<char*>(
       const_cast<InputPixelType*>(in->GetBufferPointer()));
      cvCvtColor(temp, out, CV_GRAY2BGR);
      cvReleaseImage(&temp);
      }
    }

  //
  // Return the result
  //
  return out;
}

//
// ITKImageToIplImage
//
template<typename TInputImageType>
cv::Mat
OpenCVImageBridge::ITKImageToCVMat(const TInputImageType* in, bool force3Channels)
{
  // Extra copy, but necessary to prevent memory leaks
  IplImage* temp = ITKImageToIplImage<TInputImageType>(in, force3Channels);
  cv::Mat out = cv::cvarrToMat( temp, true );
  cvReleaseImage(&temp);
  return out;
}


} // end namespace itk

#endif
