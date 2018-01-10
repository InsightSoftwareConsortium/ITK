/*=========================================================================
 *
 *  Copyright Kitware Inc.
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

#ifndef __itkRegisterThresholdedImageFilter_txx
#define __itkRegisterThresholdedImageFilter_txx

#include "itkRegisterThresholdedImageFilter.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileWriter.h"

#include "vtkMicroscopyTileStitcherConfig.h"

#include <math.h>

namespace itk
{
//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
itk::RegisterThresholdedImageFilter< ImagePixelType, VImageDimension >
::RegisterThresholdedImageFilter()
{
  this->m_FixedImage  = NULL;
  this->m_MovingImage = NULL;
  this->m_Registration = RegistrationImageFilterType::New();
  this->m_PreprocessFlag = 0;
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
RegisterThresholdedImageFilter< ImagePixelType, VImageDimension >
::~RegisterThresholdedImageFilter()
{
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
void RegisterThresholdedImageFilter< ImagePixelType, VImageDimension >
::PrepareImages()
{
  ImagePointerType fixedImage = this->GetFixedImage();
  ImagePointerType movingImage = this->GetMovingImage();

  // normalize image
  if (vtkMicroscopyTileStitcherConfig::GetInstance()->GetNormalizeFlag())
    {
    fixedImage = this->NormalizeImage(fixedImage);
    movingImage = this->NormalizeImage(movingImage);

#if ENABLE_DEBUG
    typedef itk::ImageFileWriter<ImageType>        WriterType;
    typedef typename WriterType::Pointer           WriterPointerType;
    WriterPointerType writer = WriterType::New();
    writer->SetFileName("fixedNormalized.tif");
    writer->SetInput(fixedImage);
    writer->Update();
    writer->SetFileName("movingNormalized.tif");
    writer->SetInput(movingImage);
    writer->Update();
#endif
    }

  // blur image
  if (vtkMicroscopyTileStitcherConfig::GetInstance()->GetThresholdFlag())
    {
    fixedImage = this->BlurImage(fixedImage);
    movingImage = this->BlurImage(movingImage);

#if ENABLE_DEBUG
    typedef itk::ImageFileWriter<ImageType>        WriterType;
    typedef typename WriterType::Pointer           WriterPointerType;
    WriterPointerType writer = WriterType::New();
    writer->SetFileName("fixedBlurred.tif");
    writer->SetInput(fixedImage);
    writer->Update();
    writer->SetFileName("movingBlurred.tif");
    writer->SetInput(movingImage);
    writer->Update();
#endif
    }

  // threshold image
  if (vtkMicroscopyTileStitcherConfig::GetInstance()->GetThresholdFlag())
    {
    fixedImage = this->ThresholdImage(fixedImage);
    movingImage = this->ThresholdImage(movingImage);

#if ENABLE_DEBUG
    typedef itk::ImageFileWriter<ImageType>        WriterType;
    typedef typename WriterType::Pointer           WriterPointerType;
    WriterPointerType writer = WriterType::New();
    writer->SetFileName("fixedThresholded.tif");
    writer->SetInput(fixedImage);
    writer->Update();
    writer->SetFileName("movingThresholded.tif");
    writer->SetInput(movingImage);
    writer->Update();
#endif
    }

  this->SetFixedImage(fixedImage);
  this->SetMovingImage(movingImage);
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
void RegisterThresholdedImageFilter< ImagePixelType, VImageDimension >
::Update()
{
  if (this->GetPreprocessFlag())
    {
    try
      {
      this->PrepareImages();
      }
    catch( itk::ExceptionObject & excep )
      {
      itkExceptionMacro("Exception caught during preprocessing image." << excep);
      }
    }

  // register
  this->m_Registration->SetFixedImage(this->GetFixedImage());
  this->m_Registration->SetMovingImage(this->GetMovingImage());
  this->m_Registration->SetNumberOfIterations(200);
  //this->m_Registration->SetConvergenceThreshold(0.1);
  this->m_Registration->SetStdOutputFlag(false);
  try
    {
    this->m_Registration->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    itkExceptionMacro("Exception caught during registration." << excep);
    }
}

//----------------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
typename RegisterThresholdedImageFilter< ImagePixelType, VImageDimension >
::ImagePointerType
RegisterThresholdedImageFilter< ImagePixelType, VImageDimension >
::NormalizeImage(ImagePointerType image)
{
  typedef itk::NormalizeImageFilter<ImageType, FloatImageType> FilterType;
  typedef typename FilterType::Pointer                         FilterPointerType;
  FilterPointerType normalize = FilterType::New();
  normalize->SetInput(image);

  FloatImagePointerType normalizedImage = NULL;

  // directly normalize 2D image.
  if (VImageDimension < 3)
    {
    normalize->Update();
    normalizedImage = normalize->GetOutput();
    }
  // for m-D (m>=3) image, normalize its (m-1)-D slices along the last dimension.
  else
    {
    normalizedImage = FloatImageType::New();
    normalizedImage->SetRegions(image->GetLargestPossibleRegion());
    normalizedImage->SetSpacing(image->GetSpacing());
    normalizedImage->SetOrigin(image->GetOrigin());
    normalizedImage->Allocate();

    ImageRegionType region;
    ImageIndexType index = image->GetLargestPossibleRegion().GetIndex();
    ImageSizeType size = image->GetLargestPossibleRegion().GetSize();
    unsigned int numberOfSlices = size[VImageDimension-1];
    size[VImageDimension-1] = 1;
    region.SetSize(size);
    for (unsigned int i = 0; i < numberOfSlices; i++)
      {
      index[VImageDimension-1] = i;
      region.SetIndex(index);

      normalizedImage->SetRequestedRegion(region);
      normalize->GetOutput()->SetRequestedRegion(region);
      normalize->Update();

      itk::ImageRegionIteratorWithIndex<FloatImageType> it1(normalize->GetOutput(),
                                   normalize->GetOutput()->GetRequestedRegion());
      itk::ImageRegionIteratorWithIndex<FloatImageType> it2(normalizedImage,
                                   normalizedImage->GetRequestedRegion());
      while (!it1.IsAtEnd())
        {
        it2.Set(it1.Get());
        ++it1;
        ++it2;
        }
      }
    }

  // allocate new image of imagetype
  ImagePointerType newImage = ImageType::New();
  newImage->SetRegions(image->GetLargestPossibleRegion());
  newImage->SetSpacing(image->GetSpacing());
  newImage->SetOrigin(image->GetOrigin());
  newImage->Allocate();

  // converting float image to new image
  itk::ImageRegionIteratorWithIndex<FloatImageType> iter1(normalizedImage,
                               normalizedImage->GetLargestPossibleRegion());
  itk::ImageRegionIteratorWithIndex<ImageType> iter2(newImage,
                               newImage->GetLargestPossibleRegion());

  ImagePixelType min = std::numeric_limits<ImagePixelType>::min();
  ImagePixelType max = std::numeric_limits<ImagePixelType>::max();
  float middle = static_cast<float>(min + max) / 2.0;
  float halfRange = static_cast<float>(max - min) / 2.0;
  while (!iter1.IsAtEnd())
    {
    float dv = iter1.Get() * halfRange + middle;
    ImagePixelType v;
    if (dv < min)
      {
      v = min;
      }
    else if (dv > max)
      {
      v = max;
      }
    else
      {
      v = static_cast<ImagePixelType>(dv);
      }
    iter2.Set(v);
    ++iter1;
    ++iter2;
    }

  return newImage;
}

//-----------------------------------------------------------------------
template <typename ImagePixelType, unsigned int VImageDimension>
typename RegisterThresholdedImageFilter<ImagePixelType, VImageDimension>
::ImagePointerType
RegisterThresholdedImageFilter<ImagePixelType, VImageDimension>
::BlurImage(ImagePointerType image)
{
  typedef itk::RecursiveGaussianImageFilter<ImageType, ImageType> FilterType;
  typedef typename FilterType::Pointer                            FilterPointerType;
  std::vector<FilterPointerType> gaussians;
  for (unsigned int i = 0; i < VImageDimension; i++)
    {
    gaussians.push_back(FilterType::New());
    if (i == 0)
      {
      gaussians[i]->SetInput(image);
      }
    else
      {
      gaussians[i]->SetInput(gaussians[i-1]->GetOutput());
      }
    gaussians[i]->SetSigma(
      vtkMicroscopyTileStitcherConfig::GetInstance()->GetGaussianBlurSigma());
    gaussians[i]->SetDirection(i);
    gaussians[i]->SetOrder(FilterType::ZeroOrder);
    gaussians[i]->Update();
    }

  return gaussians[VImageDimension-1]->GetOutput();
}

//-----------------------------------------------------------------------
template <typename ImagePixelType, unsigned int VImageDimension>
typename RegisterThresholdedImageFilter<ImagePixelType, VImageDimension>
::ImagePointerType
RegisterThresholdedImageFilter<ImagePixelType, VImageDimension>
::ThresholdImage(ImagePointerType image)
{
  typedef itk::MinimumMaximumImageCalculator<ImageType>    MinMaxFilterType;
  typedef typename MinMaxFilterType::Pointer               MinMaxFilterPointerType;
  MinMaxFilterPointerType minmax = MinMaxFilterType::New();
  minmax->SetImage(image);
  minmax->SetRegion(image->GetLargestPossibleRegion());
  minmax->Compute();

  ImagePixelType min = minmax->GetMinimum();
  ImagePixelType max = minmax->GetMaximum();
  double lowerRatio = vtkMicroscopyTileStitcherConfig::GetInstance()->GetLowerThresholdRatio();
  double upperRatio = vtkMicroscopyTileStitcherConfig::GetInstance()->GetUpperThresholdRatio();
  ImagePixelType lowerThreshold = min * (1 - lowerRatio) + max * lowerRatio;
  ImagePixelType upperThreshold = min * (1 - upperRatio) + max * upperRatio;

  typedef itk::BinaryThresholdImageFilter<ImageType, ImageType>  FilterType;
  typedef typename FilterType::Pointer                           FilterPointerType;
  FilterPointerType threshold = FilterType::New();
  threshold->SetInput(image);
  threshold->SetLowerThreshold(lowerThreshold);
  threshold->SetUpperThreshold(upperThreshold);
  threshold->SetOutsideValue(0);
  threshold->SetInsideValue(1);
  threshold->Update();

  return threshold->GetOutput();
}

//-----------------------------------------------------------------------
template < typename ImagePixelType, unsigned int VImageDimension >
const typename RegisterThresholdedImageFilter< ImagePixelType, VImageDimension >
::ParametersType &
RegisterThresholdedImageFilter< ImagePixelType, VImageDimension >
::GetOutputParameters()
{
  return this->m_Registration->GetOutputParameters();
}

} //end namespace itk

#endif
