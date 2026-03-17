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

#include "itkRandomImageSource.h"
#include "itkChangeLabelImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkGTest.h"

TEST(ChangeLabelImageFilter, FilterLabelsAboveThreshold)
{
  constexpr unsigned int ImageDimension{ 3 };

  using InputImageType = itk::Image<unsigned short, ImageDimension>;
  using OutputImageType = itk::Image<unsigned char, ImageDimension>;
  using InputPixelType = InputImageType::PixelType;

  using SourceType = itk::RandomImageSource<InputImageType>;
  auto source = SourceType::New();

  InputImageType::SizeValueType sizeArray[ImageDimension] = { 3, 3, 3 };

  constexpr InputPixelType upper{ 10 };
  source->SetMin(InputPixelType{});
  source->SetMax(upper);
  source->SetSize(sizeArray);

  using FilterType = itk::ChangeLabelImageFilter<InputImageType, OutputImageType>;
  auto filter = FilterType::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(filter, ChangeLabelImageFilter, UnaryFunctorImageFilter);

  constexpr InputPixelType background{ 0 };
  constexpr InputPixelType maxRemainingLabel{ 2 };
  for (InputPixelType i = maxRemainingLabel; i <= upper; ++i)
  {
    filter->SetChange(i, background);
  }

  filter->SetInput(source->GetOutput());

  EXPECT_NO_THROW(filter->Update());
  filter->SetFunctor(filter->GetFunctor());

  const auto outputImage = filter->GetOutput();

  itk::ImageRegionIteratorWithIndex<OutputImageType> ot(outputImage, outputImage->GetRequestedRegion());
  for (ot.GoToBegin(); !ot.IsAtEnd(); ++ot)
  {
    EXPECT_LE(ot.Get(), maxRemainingLabel);
  }
}

TEST(ChangeLabelImageFilter, ClearChangeMapPassesThrough)
{
  constexpr unsigned int ImageDimension{ 3 };

  using InputImageType = itk::Image<unsigned short, ImageDimension>;
  using OutputImageType = itk::Image<unsigned char, ImageDimension>;
  using InputPixelType = InputImageType::PixelType;

  using SourceType = itk::RandomImageSource<InputImageType>;
  auto source = SourceType::New();

  InputImageType::SizeValueType sizeArray[ImageDimension] = { 3, 3, 3 };

  constexpr InputPixelType upper{ 10 };
  source->SetMin(InputPixelType{});
  source->SetMax(upper);
  source->SetSize(sizeArray);

  using FilterType = itk::ChangeLabelImageFilter<InputImageType, OutputImageType>;
  auto filter = FilterType::New();

  constexpr InputPixelType background{ 0 };
  constexpr InputPixelType maxRemainingLabel{ 2 };
  for (InputPixelType i = maxRemainingLabel; i <= upper; ++i)
  {
    filter->SetChange(i, background);
  }

  filter->SetInput(source->GetOutput());
  filter->Update();

  // Clear the change map so input passes through unchanged
  filter->ClearChangeMap();

  EXPECT_NO_THROW(filter->Update());

  const auto outputImage = filter->GetOutput();

  itk::ImageRegionIteratorWithIndex<InputImageType>  it(source->GetOutput(), source->GetOutput()->GetRequestedRegion());
  itk::ImageRegionIteratorWithIndex<OutputImageType> ot(outputImage, outputImage->GetRequestedRegion());
  for (it.GoToBegin(), ot.GoToBegin(); !ot.IsAtEnd(); ++it, ++ot)
  {
    EXPECT_EQ(it.Get(), ot.Get());
  }
}
