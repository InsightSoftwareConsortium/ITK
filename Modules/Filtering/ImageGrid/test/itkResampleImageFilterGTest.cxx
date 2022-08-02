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

// The header file to be tested:
#include "itkResampleImageFilter.h"

#include "itkImage.h"

// Google Test header file:
#include <gtest/gtest.h>

// Standard C++ header files:
#include <limits>
#include <random>


namespace
{

// Returns the first pixel value from the output image of a ResampleImageFilter
// whose input is a 1x1 image, having the specified input pixel value. The
// filter uses a default interpolator and a default (identity) transform.
template <typename TPixel>
TPixel
GetFirstPixelFromFilterOutput(const TPixel inputPixel)
{
  using ImageType = itk::Image<TPixel>;

  const auto                         image = ImageType::New();
  const typename ImageType::SizeType imageSize = { { 1, 1 } };
  image->SetRegions(imageSize);
  image->Allocate();
  image->SetPixel({ { 0, 0 } }, inputPixel);

  const auto filter = itk::ResampleImageFilter<ImageType, ImageType>::New();
  filter->SetInput(image);
  filter->SetSize(imageSize);
  filter->Update();

  return filter->GetOutput()->GetPixel({ { 0, 0 } });
}


// Test if the resample image filter has a valid output region defined
// A common mistake is to use SetReferenceImage without setting UseReferenceImageOn
// which results in the default output image size of 0,0,0.
template <typename TPixel>
TPixel
TestThrowErrorOnEmptyResampleSpace(const TPixel inputPixel, const bool setUseReferenceImageMemberFunction)
{
  using ImageType = itk::Image<TPixel>;

  const auto                         image = ImageType::New();
  const typename ImageType::SizeType imageSize = { { 1, 1 } };
  image->SetRegions(imageSize);
  image->Allocate();
  image->SetPixel({ { 0, 0 } }, inputPixel);

  const auto filter = itk::ResampleImageFilter<ImageType, ImageType>::New();
  filter->SetInput(image);
  // NOTE: One might incorrectly assume that SetReferenceImage(image) has the same as effect as SetSize(imageSize)
  //       BUT in addition to SetUseReferenceImage(image), we must also specify UseReferenceImageOn()
  filter->SetReferenceImage(image);
  if (setUseReferenceImageMemberFunction)
  {
    filter->UseReferenceImageOn();
  }
  filter->Update();
  typename ImageType::ConstPointer filterOutputImage = filter->GetOutput();
  if (filterOutputImage->GetLargestPossibleRegion().GetNumberOfPixels() == 0)
  {
    return itk::NumericTraits<TPixel>::max();
  }
  return filterOutputImage->GetPixel({ { 0, 0 } });
}


// Tests that the value of the pixel from the 1x1 ResampleImageFilter output
// image is equal to the specified input pixel value, when the filter just
// uses default settings.
template <typename TPixel>
void
Expect_ResampleImageFilter_preserves_pixel_value(const TPixel inputPixel)
{
  EXPECT_EQ(GetFirstPixelFromFilterOutput(inputPixel), inputPixel);
}


// Tests that a useful diagnostic is provided via a thrown exception if the
// resample image filter has used "SetReferenceImage()" but not "UseReferenceImageOn()".
template <typename TPixel>
void
Expect_ResampleImageFilter_thows_on_incomplete_configuration(const TPixel inputPixel)
{
  EXPECT_THROW(TestThrowErrorOnEmptyResampleSpace(inputPixel, false), itk::ExceptionObject);
  EXPECT_EQ(TestThrowErrorOnEmptyResampleSpace(inputPixel, true), inputPixel);
}

} // namespace

// Compile time check of mixing transform and precision types
template class itk::ResampleImageFilter<itk::Image<int>, itk::Image<int>, float, float>;
template class itk::ResampleImageFilter<itk::Image<int>, itk::Image<int>, double, float>;
template class itk::ResampleImageFilter<itk::Image<int>, itk::Image<int>, float, double>;
template class itk::ResampleImageFilter<itk::Image<int>, itk::Image<int>, double, double>;

TEST(ResampleImageFilter, FilterPreservesAnyDoublePixelValueByDefault)
{
  using NumericLimits = std::numeric_limits<double>;

  Expect_ResampleImageFilter_preserves_pixel_value(0.0);
  Expect_ResampleImageFilter_preserves_pixel_value(1.0);
  Expect_ResampleImageFilter_preserves_pixel_value(-1.0);
  Expect_ResampleImageFilter_preserves_pixel_value(NumericLimits::lowest());
  Expect_ResampleImageFilter_preserves_pixel_value(NumericLimits::denorm_min());
  Expect_ResampleImageFilter_preserves_pixel_value(NumericLimits::min());
  Expect_ResampleImageFilter_preserves_pixel_value(NumericLimits::epsilon());
  Expect_ResampleImageFilter_preserves_pixel_value(NumericLimits::max());

  // Note: The following two expectations would fail on ITK version <= 4.13.1:
  Expect_ResampleImageFilter_preserves_pixel_value(NumericLimits::infinity());
  Expect_ResampleImageFilter_preserves_pixel_value(-NumericLimits::infinity());

  // Not A Number as input should yield Not A Number as output.
  EXPECT_TRUE(std::isnan(GetFirstPixelFromFilterOutput(NumericLimits::quiet_NaN())));

  // Check that the filter preserves any "random" pixel value:

  std::default_random_engine randomEngine;

  // The number of iterations is arbitrarily chosen.
  const size_t numberOfIterations = 11;

  for (size_t i = 0; i < numberOfIterations; ++i)
  {
    const double randomNumber1 = std::uniform_real_distribution<>{}(randomEngine);
    const double randomNumber2 = std::uniform_real_distribution<>{ 0.0, NumericLimits::max() }(randomEngine);

    Expect_ResampleImageFilter_preserves_pixel_value(randomNumber1);
    Expect_ResampleImageFilter_preserves_pixel_value(-randomNumber1);
    Expect_ResampleImageFilter_preserves_pixel_value(randomNumber2);
    Expect_ResampleImageFilter_preserves_pixel_value(-randomNumber2);
  }
}


TEST(ResampleImageFilter, FilterPreservesMinAndMaxInt64PixelValuesByDefault)
{
  Expect_ResampleImageFilter_preserves_pixel_value(std::numeric_limits<int64_t>::min());

  // Note: The following expectation would fail on ITK version <= 4.13.1:
  Expect_ResampleImageFilter_preserves_pixel_value(std::numeric_limits<int64_t>::max());
}


TEST(ResampleImageFilter, Expect_ResampleImageFilter_thows_on_incomplete_configuration)
{
  Expect_ResampleImageFilter_thows_on_incomplete_configuration(128.0);
}
