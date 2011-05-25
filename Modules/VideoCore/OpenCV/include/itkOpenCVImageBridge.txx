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
    itkGenericExceptionMacro("Cannot convert from a NULL IplImage to an itk::Image");
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
  if (inChannels == 3 && outChannels == 1)\
    {\
    current = cvCreateImage(cvSize(in->width, in->height), _CVDepth, 1);\
    cvCvtColor(in, current, CV_BGR2GRAY);\
    }\
  else if (inChannels == 1 && outChannels == 3)\
    {\
    current = cvCreateImage(cvSize(in->width, in->height), _CVDepth, 3);\
    cvCvtColor(in, current, CV_GRAY2RGB);\
    }\
  else if (inChannels == 3 && outChannels == 3)\
    {\
    current = cvCreateImage(cvSize(in->width, in->height), _CVDepth, 3);\
    cvCvtColor(in, current, CV_BGR2RGB);\
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
OpenCVImageBridge::CVMatToITKImage(const cv::Mat* in)
{
  std::cout << "STUB" << std::endl;
  return NULL;
}

//
// ITKImageToIplImage
//
template<class TInputImageType>
IplImage*
OpenCVImageBridge::ITKImageToIplImage(const TInputImageType* in)
{
  std::cout << "STUB" << std::endl;
  return NULL;
}

//
// ITKImageToIplImage
//
template<class TInputImageType>
cv::Mat*
OpenCVImageBridge::ITKImageToCVMat(const TInputImageType* in)
{
  std::cout << "STUB" << std::endl;
  return NULL;
}


} // end namespace itk

#endif
