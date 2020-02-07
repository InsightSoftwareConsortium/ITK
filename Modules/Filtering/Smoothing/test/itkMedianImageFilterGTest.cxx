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

// First include the header file to be tested:
#include "itkMedianImageFilter.h"

#include "itkImage.h"
#include "itkImageBufferRange.h"

#include <numeric> // For iota.
#include <vector>

#include <gtest/gtest.h>

namespace
{
template <typename TImage>
void
Expect_output_pixels_have_same_value_as_input_when_input_image_is_uniform(
  const typename TImage::RegionType & imageRegion,
  const typename TImage::PixelType &  inputPixelValue)
{
  using PixelType = typename TImage::PixelType;

  const auto image = TImage::New();
  image->SetRegions(imageRegion);
  image->Allocate();
  image->FillBuffer(inputPixelValue);

  const auto filter = itk::MedianImageFilter<TImage, TImage>::New();
  filter->SetInput(image);
  filter->Update();

  const TImage * const output = filter->GetOutput();

  ASSERT_NE(output, nullptr);

  for (const PixelType outputPixelValue : itk::Experimental::MakeImageBufferRange(output))
  {
    EXPECT_EQ(outputPixelValue, inputPixelValue);
  }
}


// Creates a test image, filled with a sequence of natural numbers, 1, 2, 3, ..., N.
template <typename TImage>
typename TImage::Pointer
CreateImageFilledWithSequenceOfNaturalNumbers(const typename TImage::RegionType & imageRegion)
{
  using PixelType = typename TImage::PixelType;
  const auto image = TImage::New();
  image->SetRegions(imageRegion);
  image->Allocate();
  const auto imageBufferRange = itk::Experimental::ImageBufferRange<TImage>{ *image };
  std::iota(imageBufferRange.begin(), imageBufferRange.end(), PixelType{ 1 });
  return image;
}


template <typename TImage>
void
Expect_output_has_specified_pixel_values_when_input_has_sequence_of_natural_numbers(
  const typename TImage::RegionType &             imageRegion,
  const std::vector<typename TImage::PixelType> & expectedPixelValues)
{
  using PixelType = typename TImage::PixelType;

  const auto inputImage = CreateImageFilledWithSequenceOfNaturalNumbers<TImage>(imageRegion);
  const auto filter = itk::MedianImageFilter<TImage, TImage>::New();
  filter->SetInput(inputImage);
  filter->Update();

  const TImage * const         outputImage = filter->GetOutput();
  const auto                   outputImageBufferRange = itk::Experimental::MakeImageBufferRange(outputImage);
  const std::vector<PixelType> outputPixelValues(outputImageBufferRange.cbegin(), outputImageBufferRange.cend());

  EXPECT_EQ(outputPixelValues, expectedPixelValues);
}

} // namespace


// Tests that for a uniform input image, the output pixels have the same value as the input pixels.
TEST(MedianImageFilter, OutputSameAsInputForUniformImage)
{
  Expect_output_pixels_have_same_value_as_input_when_input_image_is_uniform<itk::Image<int>>(itk::Size<>{ { 5, 6 } },
                                                                                             64);
  Expect_output_pixels_have_same_value_as_input_when_input_image_is_uniform<itk::Image<float, 3>>(
    itk::Size<3>{ { 3, 4, 5 } }, 0.5f);
}


// Tests that the output pixel values are as expected for an example input image that is filled with
// the sequence of natural numbers, 1, 2, 3, ...
TEST(MedianImageFilter, ExpectedOutputPixelValuesForSequenceOfNaturalNumbers)
{
  Expect_output_has_specified_pixel_values_when_input_has_sequence_of_natural_numbers<itk::Image<int>>(
    itk::Size<>{ { 1, 1 } }, { 1 });
  Expect_output_has_specified_pixel_values_when_input_has_sequence_of_natural_numbers<itk::Image<int>>(
    itk::Size<>{ { 1, 2 } }, { 1, 2 });
  Expect_output_has_specified_pixel_values_when_input_has_sequence_of_natural_numbers<itk::Image<int>>(
    itk::Size<>{ { 2, 1 } }, { 1, 2 });
  Expect_output_has_specified_pixel_values_when_input_has_sequence_of_natural_numbers<itk::Image<int>>(
    itk::Size<>{ { 2, 2 } }, { 2, 2, 3, 3 });
  Expect_output_has_specified_pixel_values_when_input_has_sequence_of_natural_numbers<itk::Image<int>>(
    itk::Size<>{ { 3, 3 } }, { 2, 3, 3, 4, 5, 6, 7, 7, 8 });
  Expect_output_has_specified_pixel_values_when_input_has_sequence_of_natural_numbers<itk::Image<int, 3>>(
    itk::Size<3>{ { 2, 2, 2 } }, { 3, 3, 3, 4, 5, 6, 6, 6 });
}
