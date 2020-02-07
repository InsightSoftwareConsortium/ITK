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
#include "itkSumOfSquaresImageFunction.h"

#include "itkImage.h"
#include "itkImageBufferRange.h"
#include "itkIndexRange.h"

#include <gtest/gtest.h>
#include <numeric>     // For std::accumulate.
#include <type_traits> // For std::is_reference.

// Test template instantiations for various template arguments:
template class itk::SumOfSquaresImageFunction<itk::Image<short, 1>>;
template class itk::SumOfSquaresImageFunction<itk::Image<short, 2>>;
template class itk::SumOfSquaresImageFunction<itk::Image<short, 3>>;
template class itk::SumOfSquaresImageFunction<itk::Image<short, 3>, double>;

namespace
{
// Creates a test image, filled with a sequence of natural numbers, 1, 2, 3, ..., N.
template <typename TImage>
typename TImage::Pointer
CreateImageFilledWithSequenceOfNaturalNumbers(const typename TImage::SizeType & imageSize)
{
  using PixelType = typename TImage::PixelType;
  const auto image = TImage::New();
  image->SetRegions(imageSize);
  image->Allocate();
  const auto imageBufferRange = itk::Experimental::ImageBufferRange<TImage>{ *image };
  std::iota(imageBufferRange.begin(), imageBufferRange.end(), PixelType{ 1 });
  return image;
}


template <typename TImage>
void
Expect_EvaluateAtIndex_returns_zero_when_all_pixels_are_zero(const typename TImage::SizeType & imageSize)
{
  const auto image = TImage::New();
  image->SetRegions(imageSize);
  image->Allocate(true);

  const auto imageFunction = itk::SumOfSquaresImageFunction<TImage>::New();

  imageFunction->SetInputImage(image);

  for (const auto index : itk::Experimental::ZeroBasedIndexRange<TImage::ImageDimension>{ imageSize })
  {
    EXPECT_EQ(imageFunction->EvaluateAtIndex(index), 0);
  }
}


template <typename TImage>
void
Expect_EvaluateAtIndex_returns_number_of_neigbors_when_all_pixels_are_one(const typename TImage::SizeType & imageSize,
                                                                          const unsigned int                radius)
{
  const auto image = TImage::New();
  image->SetRegions(imageSize);
  image->Allocate();
  image->FillBuffer(1);

  const auto imageFunction = itk::SumOfSquaresImageFunction<TImage>::New();

  imageFunction->SetInputImage(image);
  imageFunction->SetNeighborhoodRadius(radius);

  const auto numberOfNeighbors = std::pow(2.0 * radius + 1.0, TImage::ImageDimension);

  for (const auto index : itk::Experimental::ZeroBasedIndexRange<TImage::ImageDimension>{ imageSize })
  {
    EXPECT_EQ(imageFunction->EvaluateAtIndex(index), numberOfNeighbors);
  }
}

} // namespace


// Tests that EvaluateAtIndex returns zero when all pixels are zero.
TEST(SumOfSquaresImageFunction, EvaluateAtIndexReturnsZeroWhenAllPixelsAreZero)
{
  Expect_EvaluateAtIndex_returns_zero_when_all_pixels_are_zero<itk::Image<double, 2>>(itk::Size<2>{ { 2, 3 } });
  Expect_EvaluateAtIndex_returns_zero_when_all_pixels_are_zero<itk::Image<unsigned char, 3>>(
    itk::Size<3>{ { 2, 3, 4 } });
}


// Tests that EvaluateAtIndex returns the number of neighborhood pixels when all pixels are one.
TEST(SumOfSquaresImageFunction, EvaluateAtIndexReturnsNumberOfNeighborsWhenAllPixelsAreOne)
{
  for (unsigned radius{}; radius <= 2; ++radius)
  {
    Expect_EvaluateAtIndex_returns_number_of_neigbors_when_all_pixels_are_one<itk::Image<double, 2>>(
      itk::Size<2>{ { 2, 3 } }, radius);
    Expect_EvaluateAtIndex_returns_number_of_neigbors_when_all_pixels_are_one<itk::Image<unsigned char, 3>>(
      itk::Size<3>{ { 2, 3, 4 } }, radius);
  }
}


// Tests EvaluateAtIndex at the center pixel index (1, 1) of a 3x3 image.
TEST(SumOfSquaresImageFunction, EvaluateAtCenterPixelOfImageOfSize3x3)
{
  using ImageType = itk::Image<int>;

  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>({ { 3, 3 } });
  const auto imageFunction = itk::SumOfSquaresImageFunction<ImageType>::New();

  imageFunction->SetInputImage(image);

  const auto imageBufferRange = itk::Experimental::ImageBufferRange<const ImageType>{ *image };

  // Sum of squares of all pixels of the image:
  const auto expectedResult = std::accumulate(
    imageBufferRange.cbegin(), imageBufferRange.cend(), 0.0, [](const double sum, const int pixelValue) {
      return sum + (pixelValue * pixelValue);
    });

  // Note that in this particular case, the image and the neighborhood have the same size!

  EXPECT_EQ(imageFunction->EvaluateAtIndex(itk::Index<>{ { 1, 1 } }), expectedResult);
}
