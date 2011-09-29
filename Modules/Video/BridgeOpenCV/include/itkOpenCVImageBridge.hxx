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
#ifndef __itkOpenCVImageBridge_txx
#define __itkOpenCVImageBridge_txx

#include "itkOpenCVImageBridge.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkConvertPixelBuffer.h"
#include "itkNumericTraits.h"

namespace itk
{

//
// IplImageToITKImage
//
template<class TOutputImageType>
typename TOutputImageType::Pointer
OpenCVImageBridge::IplImageToITKImage(const IplImage* in)
{

  // Typedefs
  typedef TOutputImageType ImageType;
  typedef typename ImageType::PixelType OutputPixelType;
  typedef DefaultConvertPixelTraits<OutputPixelType> ConvertPixelTraits;

  //
  // Make sure input isn't null and output type is 2D or 1D
  //
  if (!in)
    {
    itkGenericExceptionMacro("Input is NULL");
    }
  if (ImageType::ImageDimension > 2)
    {
    itkGenericExceptionMacro("OpenCV only supports 2D and 1D images");
    }

  //
  // Do the conversion
  //
  typename ImageType::Pointer out = ImageType::New();
  bool isVectorImage(strcmp(out->GetNameOfClass(), "VectorImage") == 0);
  unsigned int inChannels = in->nChannels;
  unsigned int outChannels = itk::NumericTraits<OutputPixelType>::MeasurementVectorType::Dimension;
  void* unpaddedBuffer;
  unsigned int rowPadding = 0;


  // We only change current if it no longer points at in, so this is safe
  IplImage* current = const_cast<IplImage*>(in);

  // Define a macro to do the conversion to avoid code duplication. The steps
  // are:
  //  1) Handle converting between colorspaces
  //  2) Allocate the output image
  //  3) Create a copy of the current IplImage's buffer without any padding
  //     (slow but necessary)
  //  4) Copy the buffer and convert the pixels if necessary
#define ITK_CONVERT_IPLIMAGE_BUFFER(_CVDepth, inType)\
  bool freeCurrent = false;\
  if (inChannels == 3 && outChannels == 1)\
    {\
    current = cvCreateImage(cvSize(in->width, in->height), _CVDepth, 1);\
    cvCvtColor(in, current, CV_BGR2GRAY);\
    freeCurrent = true;\
    }\
  else if (inChannels == 1 && outChannels == 3)\
    {\
    current = cvCreateImage(cvSize(in->width, in->height), _CVDepth, 3);\
    cvCvtColor(in, current, CV_GRAY2RGB);\
    freeCurrent = true;\
    }\
  else if (inChannels == 3 && outChannels == 3)\
    {\
    current = cvCreateImage(cvSize(in->width, in->height), _CVDepth, 3);\
    cvCvtColor(in, current, CV_BGR2RGB);\
    freeCurrent = true;\
    }\
  else if (inChannels != 1 || outChannels != 1)\
    {\
    itkGenericExceptionMacro("Conversion from " << inChannels << " channels to "\
                             << outChannels << " channels is not supported");\
    }\
  typename ImageType::RegionType region;\
  typename ImageType::RegionType::SizeType size;\
  typename ImageType::RegionType::IndexType start;\
  typename ImageType::SpacingType spacing;\
  size[0] = current->width;\
  size[1] = current->height;\
  start.Fill(0);\
  spacing.Fill(1);\
  region.SetSize(size);\
  region.SetIndex(start);\
  out->SetRegions(region);\
  out->SetSpacing(spacing);\
  out->Allocate();\
  rowPadding = current->widthStep % current->width*outChannels;\
  unpaddedBuffer = reinterpret_cast< void* >(\
    new inType[current->height*current->width*current->nChannels]);\
  unsigned int paddedBufPos = 0;\
  unsigned int unpaddedBufPos = 0;\
  for (int i = 0; i < current->height; ++i)\
    {\
    memcpy(&(reinterpret_cast<inType*>(unpaddedBuffer)[unpaddedBufPos]),\
           &(reinterpret_cast<inType*>(current->imageData)[paddedBufPos]),\
           current->width*current->nChannels);\
    paddedBufPos += current->widthStep;\
    unpaddedBufPos += current->width*current->nChannels;\
    }\
  if (isVectorImage)\
    {\
    ConvertPixelBuffer<inType, OutputPixelType, ConvertPixelTraits>\
      ::ConvertVectorImage(static_cast< inType* >(unpaddedBuffer),\
                           current->nChannels,\
                           out->GetPixelContainer()->GetBufferPointer(),\
                           out->GetPixelContainer()->Size());\
    }\
  else\
    {\
    ConvertPixelBuffer<inType, OutputPixelType, ConvertPixelTraits>\
      ::Convert(static_cast< inType* >(unpaddedBuffer),\
                current->nChannels,\
                out->GetPixelContainer()->GetBufferPointer(),\
                out->GetPixelContainer()->Size());\
    }\
  delete[] reinterpret_cast<inType*>(unpaddedBuffer);\
  if (freeCurrent)\
    {\
    cvReleaseImage(&current);\
    }


  switch (in->depth)
    {
    case (IPL_DEPTH_8U):
      {
      ITK_CONVERT_IPLIMAGE_BUFFER(IPL_DEPTH_8U, unsigned char);
      break;
      }
    case (IPL_DEPTH_8S):
      {
      ITK_CONVERT_IPLIMAGE_BUFFER(IPL_DEPTH_8S, char);
      break;
      }
    case (IPL_DEPTH_16U):
      {
      ITK_CONVERT_IPLIMAGE_BUFFER(IPL_DEPTH_16U, unsigned short);
      break;
      }
    case (IPL_DEPTH_16S):
      {
      ITK_CONVERT_IPLIMAGE_BUFFER(IPL_DEPTH_16S, short);
      break;
      }
    case (IPL_DEPTH_32F):
      {
      ITK_CONVERT_IPLIMAGE_BUFFER(IPL_DEPTH_32F, float);
      break;
      }
    case (IPL_DEPTH_64F):
      {
      ITK_CONVERT_IPLIMAGE_BUFFER(IPL_DEPTH_64F, double);
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
template<class TOutputImageType>
typename TOutputImageType::Pointer
OpenCVImageBridge::CVMatToITKImage(const cv::Mat & in)
{
  const IplImage converted = in;
  return IplImageToITKImage<TOutputImageType>(&converted);
}

//
// ITKImageToIplImage
//
template<class TInputImageType>
IplImage*
OpenCVImageBridge::ITKImageToIplImage(const TInputImageType* in, bool force3Channels)
{
  // Typedefs
  typedef TInputImageType ImageType;
  typedef typename ImageType::PixelType InputPixelType;
  typedef typename itk::NumericTraits<InputPixelType>::ValueType ValueType;

  //
  // Make sure input isn't null, is 2D or 1D, and is scalar or RGB
  //
  if (!in)
    {
    itkGenericExceptionMacro("Input is NULL");
    }
  if (ImageType::ImageDimension > 2)
    {
    itkGenericExceptionMacro("OpenCV only supports 2D and 1D images");
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
  unsigned int w = in->GetLargestPossibleRegion().GetSize()[0];
  unsigned int h = in->GetLargestPossibleRegion().GetSize()[1];

  //
  // set the depth correctly based on input pixel type
  //
  if (typeid(ValueType) == typeid(unsigned char))
    {
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_8U, outChannels);
    }
  else if (typeid(ValueType) == typeid(char))
    {
    if (outChannels != 1)
      {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type char");
      }
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_8S, outChannels);
    }
  else if (typeid(ValueType) == typeid(unsigned short))
    {
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_16U, outChannels);
    }
  else if (typeid(ValueType) == typeid(short))
    {
    if (outChannels != 1)
      {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type short");
      }
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_16S, outChannels);
    }
  else if (typeid(ValueType) == typeid(float))
    {
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_32F, outChannels);
    }
  else if (typeid(ValueType) == typeid(double))
    {
    if (outChannels != 1)
      {
      itkGenericExceptionMacro("OpenCV does not support color images with pixels of type double");
      }
    out = cvCreateImage(cvSize(w,h), IPL_DEPTH_64F, outChannels);
    }
  else
    {
    itkGenericExceptionMacro("OpenCV does not support the input pixel type");
    }

  // Scalar output
  if (outChannels == 1)
    {
    memcpy(out->imageData, in->GetBufferPointer(), out->imageSize);
    }

  // BGR output
  else
    {
    // Set up an IplImage pointing at the input's buffer. It's ok to do the
    // const cast because it will only get used to copy pixels
    IplImage* temp = cvCreateImageHeader(cvSize(w,h), out->depth, inChannels);
    temp->imageData = reinterpret_cast<char*>(
      const_cast<InputPixelType*>(in->GetBufferPointer()));

    // Already 3 channels
    if (inChannels == 3)
      {
      cvCvtColor(temp, out, CV_RGB2BGR);
      }

    // input 1 channel, but forcing 3 channel output
    else
      {
      cvCvtColor(temp, out, CV_GRAY2BGR);
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
template<class TInputImageType>
cv::Mat
OpenCVImageBridge::ITKImageToCVMat(const TInputImageType* in, bool force3Channels)
{
  // Extra copy, but necessary to prevent memory leaks
  IplImage* temp = ITKImageToIplImage<TInputImageType>(in, force3Channels);
  cv::Mat out(temp, true);
  cvReleaseImage(&temp);
  return out;
}


} // end namespace itk

#endif
