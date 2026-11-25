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

// First include the header file to be tested:
#include "itkNormalizedCorrelationImageFilter.h"

#include "itkImage.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

#include <cmath>

#include <gtest/gtest.h>

namespace
{
constexpr unsigned int Dimension = 2;
using PixelType = float;
using ImageType = itk::Image<PixelType, Dimension>;
using FilterType = itk::NormalizedCorrelationImageFilter<ImageType, ImageType, ImageType>;
using NeighborhoodType = FilterType::OutputNeighborhoodType;

using DoubleImageType = itk::Image<double, Dimension>;
using DoubleFilterType = itk::NormalizedCorrelationImageFilter<DoubleImageType, DoubleImageType, DoubleImageType>;
using DoubleNeighborhoodType = DoubleFilterType::OutputNeighborhoodType;

// Constant double whose 9-term sum-of-squares cancellation rounds negative, driving sqrt to NaN.
constexpr double cancellationProneValue = 536680.0128080135;

template <typename TImage = ImageType>
itk::SmartPointer<TImage>
MakeImage(typename TImage::PixelType fillValue)
{
  const typename TImage::SizeType size = { { 6, 6 } };
  typename TImage::RegionType     region;
  region.SetSize(size);

  auto image = TImage::New();
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(fillValue);
  return image;
}

template <typename TImage>
void
ExpectFiniteZeroOutput(const TImage & output)
{
  for (itk::ImageRegionConstIterator<TImage> it(&output, output.GetLargestPossibleRegion()); !it.IsAtEnd(); ++it)
  {
    EXPECT_FALSE(std::isnan(it.Get()));
    EXPECT_EQ(it.Get(), typename TImage::PixelType{});
  }
}
} // namespace

TEST(NormalizedCorrelationImageFilterGTest, ConstantTemplateProducesFiniteZeroOutput)
{
  const auto image = MakeImage(0.0f);
  float      value = 0.0f;
  for (itk::ImageRegionIterator<ImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd(); ++it, ++value)
  {
    it.Set(value);
  }

  NeighborhoodType constantTemplate;
  constantTemplate.SetRadius(1);
  for (unsigned int i = 0; i < constantTemplate.Size(); ++i)
  {
    constantTemplate[i] = 1.0f;
  }

  const auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetTemplate(constantTemplate);
  filter->Update();

  ExpectFiniteZeroOutput(*filter->GetOutput());
}

TEST(NormalizedCorrelationImageFilterGTest, ConstantNeighborhoodProducesFiniteZeroOutput)
{
  const auto image = MakeImage(5.0f);

  NeighborhoodType varyingTemplate;
  varyingTemplate.SetRadius(1);
  for (unsigned int i = 0; i < varyingTemplate.Size(); ++i)
  {
    varyingTemplate[i] = static_cast<PixelType>(i);
  }

  const auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetTemplate(varyingTemplate);
  filter->Update();

  ExpectFiniteZeroOutput(*filter->GetOutput());
}

TEST(NormalizedCorrelationImageFilterGTest, ConstantNeighborhoodUnderMaskProducesFiniteZeroOutput)
{
  const auto image = MakeImage(5.0f);
  const auto mask = MakeImage(1.0f);

  NeighborhoodType varyingTemplate;
  varyingTemplate.SetRadius(1);
  for (unsigned int i = 0; i < varyingTemplate.Size(); ++i)
  {
    varyingTemplate[i] = static_cast<PixelType>(i);
  }

  const auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetTemplate(varyingTemplate);
  filter->SetMaskImage(mask);
  filter->Update();

  ExpectFiniteZeroOutput(*filter->GetOutput());
}

TEST(NormalizedCorrelationImageFilterGTest, CancellationNaNTemplateProducesFiniteZeroOutput)
{
  const auto image = MakeImage<DoubleImageType>(0.0);
  double     value = 0.0;
  for (itk::ImageRegionIterator<DoubleImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd();
       ++it, ++value)
  {
    it.Set(value);
  }

  DoubleNeighborhoodType constantTemplate;
  constantTemplate.SetRadius(1);
  for (unsigned int i = 0; i < constantTemplate.Size(); ++i)
  {
    constantTemplate[i] = cancellationProneValue;
  }

  const auto filter = DoubleFilterType::New();
  filter->SetInput(image);
  filter->SetTemplate(constantTemplate);
  filter->Update();

  ExpectFiniteZeroOutput(*filter->GetOutput());
}

TEST(NormalizedCorrelationImageFilterGTest, CancellationNaNNeighborhoodProducesFiniteZeroOutput)
{
  const auto image = MakeImage<DoubleImageType>(cancellationProneValue);

  DoubleNeighborhoodType varyingTemplate;
  varyingTemplate.SetRadius(1);
  for (unsigned int i = 0; i < varyingTemplate.Size(); ++i)
  {
    varyingTemplate[i] = static_cast<double>(i);
  }

  const auto filter = DoubleFilterType::New();
  filter->SetInput(image);
  filter->SetTemplate(varyingTemplate);
  filter->Update();

  ExpectFiniteZeroOutput(*filter->GetOutput());
}

TEST(NormalizedCorrelationImageFilterGTest, CancellationNaNNeighborhoodUnderMaskProducesFiniteZeroOutput)
{
  const auto image = MakeImage<DoubleImageType>(cancellationProneValue);
  const auto mask = MakeImage<DoubleImageType>(1.0);

  DoubleNeighborhoodType varyingTemplate;
  varyingTemplate.SetRadius(1);
  for (unsigned int i = 0; i < varyingTemplate.Size(); ++i)
  {
    varyingTemplate[i] = static_cast<double>(i);
  }

  const auto filter = DoubleFilterType::New();
  filter->SetInput(image);
  filter->SetTemplate(varyingTemplate);
  filter->SetMaskImage(mask);
  filter->Update();

  ExpectFiniteZeroOutput(*filter->GetOutput());
}
