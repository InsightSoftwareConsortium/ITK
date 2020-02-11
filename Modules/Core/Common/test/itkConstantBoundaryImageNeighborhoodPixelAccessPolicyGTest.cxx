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
#include "itkConstantBoundaryImageNeighborhoodPixelAccessPolicy.h"

#include "itkShapedImageNeighborhoodRange.h"
#include "itkConstantBoundaryCondition.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkImage.h"
#include "itkImageNeighborhoodOffsets.h"
#include "itkVectorImage.h"

#include <gtest/gtest.h>

// Test template instantiations for various template arguments:
template class itk::Experimental::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<itk::Image<short, 1>>;
template class itk::Experimental::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<itk::Image<short, 2>>;
template class itk::Experimental::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<itk::Image<short, 3>>;
template class itk::Experimental::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<itk::Image<short, 4>>;
template class itk::Experimental::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<const itk::Image<short>>;
template class itk::Experimental::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<itk::VectorImage<short>>;
template class itk::Experimental::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<const itk::VectorImage<short>>;

namespace
{
template <typename TImage>
typename TImage::Pointer
CreateImage(const unsigned sizeX, const unsigned sizeY)
{
  const auto                      image = TImage::New();
  const typename TImage::SizeType imageSize = { { sizeX, sizeY } };
  image->SetRegions(imageSize);
  image->Allocate();
  return image;
}


// Creates a test image, filled with a sequence of natural numbers, 1, 2, 3, ..., N.
template <typename TImage>
typename TImage::Pointer
CreateImageFilledWithSequenceOfNaturalNumbers(const unsigned sizeX, const unsigned sizeY)
{
  using PixelType = typename TImage::PixelType;
  const auto image = CreateImage<TImage>(sizeX, sizeY);

  const unsigned numberOfPixels = sizeX * sizeY;

  PixelType * const bufferPointer = image->GetBufferPointer();

  for (unsigned i = 0; i < numberOfPixels; ++i)
  {
    bufferPointer[i] = static_cast<typename TImage::PixelType>(i + 1);
  }
  return image;
}
} // namespace


// When no constant is specified (which is the default), an attempt to retrieve
// a pixel outside the image boundaries should yield zero, for a scalar pixel type.
TEST(ConstantBoundaryImageNeighborhoodPixelAccessPolicy, YieldsZeroOutsideImageByDefault)
{
  using PixelType = int;
  using ImageType = itk::Image<PixelType>;
  using RangeType = itk::Experimental::ShapedImageNeighborhoodRange<
    ImageType,
    itk::Experimental::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<ImageType>>;

  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImage<ImageType>(sizeX, sizeY);
  image->FillBuffer(42);

  const ImageType::IndexType                                locationOutsideImage{ { -1, -1 } };
  const itk::Size<ImageType::ImageDimension>                radius = { {} };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::Experimental::GenerateRectangularImageNeighborhoodOffsets(radius);
  const RangeType range{ *image, locationOutsideImage, offsets };

  for (const PixelType pixel : range)
  {
    EXPECT_EQ(pixel, 0);
  }
}


// When the constant is added as extra argument of a neighborhood range, an attempt to retrieve
// a pixel outside the image boundaries should yield this specific constant value.
TEST(ConstantBoundaryImageNeighborhoodPixelAccessPolicy, YieldsSpecifiedConstantOutsideImage)
{
  using PixelType = int;
  using ImageType = itk::Image<PixelType>;
  using RangeType = itk::Experimental::ShapedImageNeighborhoodRange<
    ImageType,
    itk::Experimental::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<ImageType>>;

  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImage<ImageType>(sizeX, sizeY);
  image->FillBuffer(42);

  const ImageType::IndexType                                locationOutsideImage{ { -1, -1 } };
  const itk::Size<ImageType::ImageDimension>                radius = { {} };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::Experimental::GenerateRectangularImageNeighborhoodOffsets(radius);
  const auto numberOfExpectedNeighbors = offsets.size();

  for (PixelType constantValue = -1; constantValue <= 2; ++constantValue)
  {
    const RangeType range{ *image, locationOutsideImage, offsets, constantValue };

    // Test by using a range-based for-loop:
    for (const PixelType pixel : range)
    {
      EXPECT_EQ(pixel, constantValue);
    }

    ASSERT_EQ(range.size(), numberOfExpectedNeighbors);

    // Test by using RangeType::operator[]:
    for (std::size_t i = 0; i < numberOfExpectedNeighbors; ++i)
    {
      const PixelType pixel = range[i];
      EXPECT_EQ(pixel, constantValue);
    }
  }
}

// With this policy, ShapedImageNeighborhoodRange should yield the same pixel
// values as itk::ConstNeighborhoodIterator with itk::ConstantBoundaryCondition.
TEST(ConstantBoundaryImageNeighborhoodPixelAccessPolicy, YieldsSameValuesAsConstantBoundaryCondition)
{
  using PixelType = int;
  using ImageType = itk::Image<PixelType>;
  using RangeType = itk::Experimental::ShapedImageNeighborhoodRange<
    ImageType,
    itk::Experimental::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<const ImageType>>;

  enum
  {
    sizeX = 3,
    sizeY = 4
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ {} };
  const itk::Size<ImageType::ImageDimension>                radius = { { 1, 2 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::Experimental::GenerateRectangularImageNeighborhoodOffsets(radius);

  for (PixelType constantValue = -1; constantValue <= 2; ++constantValue)
  {
    const RangeType range{ *image, location, offsets, constantValue };

    itk::ConstantBoundaryCondition<ImageType> boundaryCondition;
    boundaryCondition.SetConstant(constantValue);
    itk::ConstNeighborhoodIterator<ImageType, itk::ConstantBoundaryCondition<ImageType>> constNeighborhoodIterator(
      radius, image, image->GetRequestedRegion());
    constNeighborhoodIterator.SetLocation(location);
    constNeighborhoodIterator.SetBoundaryCondition(boundaryCondition);

    itk::SizeValueType i = 0;

    for (const PixelType pixel : range)
    {
      EXPECT_EQ(pixel, constNeighborhoodIterator.GetPixel(i));
      ++i;
    }
  }
}
