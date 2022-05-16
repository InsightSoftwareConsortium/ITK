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
#include "itkShapedImageNeighborhoodRange.h"

#include "itkConstantBoundaryCondition.h"
#include "itkConstantBoundaryImageNeighborhoodPixelAccessPolicy.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkImage.h"
#include "itkImageNeighborhoodOffsets.h"
#include "itkRangeGTestUtilities.h"
#include "itkVectorImage.h"

#include <gtest/gtest.h>

#include <algorithm> // For std::reverse_copy, std::equal, etc.
#include <array>
#include <numeric> // For std::inner_product

// Test template instantiations for various ImageDimenion values, and const Image:
template class itk::ShapedImageNeighborhoodRange<itk::Image<short, 1>>;
template class itk::ShapedImageNeighborhoodRange<itk::Image<short, 2>>;
template class itk::ShapedImageNeighborhoodRange<itk::Image<short, 3>>;
template class itk::ShapedImageNeighborhoodRange<itk::Image<short, 4>>;
template class itk::ShapedImageNeighborhoodRange<const itk::Image<short>>;
template class itk::ShapedImageNeighborhoodRange<itk::VectorImage<short>>;
template class itk::ShapedImageNeighborhoodRange<const itk::VectorImage<short>>;

using itk::ShapedImageNeighborhoodRange;
using itk::RangeGTestUtilities;

namespace
{
template <typename TImage>
typename TImage::Pointer
CreateImage(const unsigned int sizeX, const unsigned int sizeY)
{
  const auto                      image = TImage::New();
  const typename TImage::SizeType imageSize = { { sizeX, sizeY } };
  image->SetRegions(imageSize);
  image->Allocate();
  return image;
}


// Tells whether or not ShapedImageNeighborhoodRange<TImage>::iterator is the same type
// as ShapedImageNeighborhoodRange<TImage>::const_iterator.
template <typename TImage>
constexpr bool
IsIteratorTypeTheSameAsConstIteratorType()
{
  using RangeType = ShapedImageNeighborhoodRange<TImage>;

  return std::is_same<typename RangeType::iterator, typename RangeType::const_iterator>::value;
}


static_assert(!IsIteratorTypeTheSameAsConstIteratorType<itk::Image<int>>() &&
                !IsIteratorTypeTheSameAsConstIteratorType<itk::VectorImage<int>>(),
              "For a non-const image, non-const iterator and const_iterator should be different types!");
static_assert(IsIteratorTypeTheSameAsConstIteratorType<const itk::Image<int>>() &&
                IsIteratorTypeTheSameAsConstIteratorType<const itk::VectorImage<int>>(),
              "For a const image, non-const iterator and const_iterator should be the same type!");


// Creates a test image, filled with a sequence of natural numbers, 1, 2, 3, ..., N.
template <typename TImage>
typename TImage::Pointer
CreateImageFilledWithSequenceOfNaturalNumbers(const unsigned int sizeX, const unsigned int sizeY)
{
  using PixelType = typename TImage::PixelType;
  const auto image = CreateImage<TImage>(sizeX, sizeY);

  const unsigned int numberOfPixels = sizeX * sizeY;

  PixelType * const bufferPointer = image->GetBufferPointer();

  for (unsigned int i = 0; i < numberOfPixels; ++i)
  {
    bufferPointer[i] = static_cast<typename TImage::PixelType>(i + 1);
  }
  return image;
}


template <typename TPixel, unsigned int VImageDimension>
void
SetVectorLengthIfImageIsVectorImage(itk::VectorImage<TPixel, VImageDimension> & image, const unsigned int vectorLength)
{
  image.SetVectorLength(vectorLength);
}


template <typename TPixel, unsigned int VImageDimension>
void
SetVectorLengthIfImageIsVectorImage(itk::Image<TPixel, VImageDimension> & itkNotUsed(image),
                                    const unsigned int                    itkNotUsed(vectorLength))
{
  // Do not set the VectorLength. The specified image is not a VectorImage.
}


template <typename TImage>
typename TImage::Pointer
CreateSmallImage()
{
  const auto                image = TImage::New();
  typename TImage::SizeType imageSize;
  imageSize.Fill(0);
  image->SetRegions(imageSize);
  SetVectorLengthIfImageIsVectorImage(*image, 1);
  image->Allocate(true);
  return image;
}


template <typename TImage>
void
ExpectRangeIsEmptyForAnEmptyContainerOfShapeOffsets()
{
  const auto                image = TImage::New();
  typename TImage::SizeType imageSize;
  imageSize.Fill(1);
  image->SetRegions(imageSize);
  SetVectorLengthIfImageIsVectorImage(*image, 1);
  image->Allocate();

  const typename TImage::IndexType location{ {} };

  using OffsetType = typename TImage::OffsetType;

  const std::array<OffsetType, 0> emptyArrayOfOffsets{ {} };
  const std::vector<OffsetType>   emptyVectorOfOffsets{};

  EXPECT_TRUE((ShapedImageNeighborhoodRange<TImage>{ *image, location, emptyArrayOfOffsets }.empty()));
  EXPECT_TRUE((ShapedImageNeighborhoodRange<TImage>{ *image, location, emptyVectorOfOffsets }.empty()));
}


template <typename TImage>
void
ExpectRangeIsNotEmptyForNonEmptyImageAndShapeOffsetContainer()
{
  // First create a non-empty image:
  const auto                image = TImage::New();
  typename TImage::SizeType imageSize;
  imageSize.Fill(1);
  image->SetRegions(imageSize);
  SetVectorLengthIfImageIsVectorImage(*image, 1);
  image->Allocate();

  // Then create a non-empty vector of shape offsets:
  const itk::Size<TImage::ImageDimension>                radius{ { 1 } };
  const std::vector<itk::Offset<TImage::ImageDimension>> shapeOffsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);

  // Sanity check: the container of shape offset is expected to be non-empty.
  EXPECT_FALSE(shapeOffsets.empty());

  const typename TImage::IndexType location{ {} };

  EXPECT_FALSE((ShapedImageNeighborhoodRange<TImage>{ *image, location, shapeOffsets }.empty()));
}


template <typename TImage>
void
ExpectCopyConstructedRangeHasSameIteratorsAsOriginal()
{
  const auto                                             image = CreateSmallImage<TImage>();
  const itk::Size<TImage::ImageDimension>                radius{ { 1 } };
  const std::vector<itk::Offset<TImage::ImageDimension>> shapeOffsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  const typename TImage::IndexType           location{ {} };
  const ShapedImageNeighborhoodRange<TImage> originalRange{ *image, location, shapeOffsets };

  RangeGTestUtilities::ExpectCopyConstructedRangeHasSameIteratorsAsOriginal(originalRange);
}


template <typename TImage>
void
ExpectCopyAssignedRangeHasSameIteratorsAsOriginal()
{
  const auto                                             image = CreateSmallImage<TImage>();
  const itk::Size<TImage::ImageDimension>                radius{ { 1 } };
  const std::vector<itk::Offset<TImage::ImageDimension>> shapeOffsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  const typename TImage::IndexType           location{ {} };
  const ShapedImageNeighborhoodRange<TImage> originalRange{ *image, location, shapeOffsets };

  RangeGTestUtilities::ExpectCopyAssignedRangeHasSameIteratorsAsOriginal(originalRange);
}


template <typename TImage>
void
ExpectMoveConstructedRangeHasSameIteratorsAsOriginalBeforeMove()
{
  const auto                                             image = CreateSmallImage<TImage>();
  const itk::Size<TImage::ImageDimension>                radius{ { 1 } };
  const std::vector<itk::Offset<TImage::ImageDimension>> shapeOffsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  const typename TImage::IndexType location{ {} };

  RangeGTestUtilities::ExpectMoveConstructedRangeHasSameIteratorsAsOriginalBeforeMove(
    ShapedImageNeighborhoodRange<TImage>{ *image, location, shapeOffsets });
}


template <typename TImage>
void
ExpectMoveAssignedRangeHasSameIteratorsAsOriginalBeforeMove()
{
  const auto                                             image = CreateSmallImage<TImage>();
  const itk::Size<TImage::ImageDimension>                radius{ { 1 } };
  const std::vector<itk::Offset<TImage::ImageDimension>> shapeOffsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  const typename TImage::IndexType location{ {} };

  RangeGTestUtilities::ExpectMoveConstructedRangeHasSameIteratorsAsOriginalBeforeMove(
    ShapedImageNeighborhoodRange<TImage>{ *image, location, shapeOffsets });
}

} // namespace


// Tests that a begin iterator compares equal to another begin iterator of the
// same range. Also does this test for end iterators.
TEST(ShapedImageNeighborhoodRange, EquivalentBeginOrEndIteratorsCompareEqual)
{
  using ImageType = itk::Image<int>;

  const auto image = CreateImage<ImageType>(2, 3);

  const itk::Size<ImageType::ImageDimension>                radius{ {} };
  const ImageType::IndexType                                location{ {} };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<ImageType> range{ *image, location, offsets };

  const itk::ShapedImageNeighborhoodRange<ImageType>::iterator       begin = range.begin();
  const itk::ShapedImageNeighborhoodRange<ImageType>::iterator       end = range.end();
  const itk::ShapedImageNeighborhoodRange<ImageType>::const_iterator cbegin = range.cbegin();
  const itk::ShapedImageNeighborhoodRange<ImageType>::const_iterator cend = range.cend();

  // An iterator object compares equal to itself:
  EXPECT_EQ(begin, begin);
  EXPECT_EQ(end, end);
  EXPECT_EQ(cbegin, cbegin);
  EXPECT_EQ(cend, cend);

  // Multiple calls of the same function yield equivalent objects:
  EXPECT_EQ(range.begin(), range.begin());
  EXPECT_EQ(range.end(), range.end());
  EXPECT_EQ(range.cbegin(), range.cbegin());
  EXPECT_EQ(range.cend(), range.cend());

  // Corresponding const_iterator and non-const iterator compare equal:
  EXPECT_EQ(begin, cbegin);
  EXPECT_EQ(end, cend);
  EXPECT_EQ(cbegin, begin);
  EXPECT_EQ(cend, end);
}


TEST(ShapedImageNeighborhoodRange, BeginAndEndDoNotCompareEqual)
{
  using ImageType = itk::Image<int>;

  const auto image = CreateImage<ImageType>(2, 3);

  // Note that even with a radius of zero, the neighborhood still has 1 pixel.
  const itk::Size<ImageType::ImageDimension>                radius{ {} };
  const ImageType::IndexType                                location{ {} };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<ImageType> range{ *image, location, offsets };

  EXPECT_FALSE(range.begin() == range.end());
  EXPECT_NE(range.begin(), range.end());
}


// Tests that an iterator converts (implicitly) to a const_iterator.
TEST(ShapedImageNeighborhoodRange, IteratorConvertsToConstIterator)
{
  using ImageType = itk::Image<int>;

  const auto image = CreateImage<ImageType>(2, 3);

  const itk::Size<ImageType::ImageDimension>                radius{ {} };
  const ImageType::IndexType                                location{ {} };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<ImageType> range{ *image, location, offsets };

  const itk::ShapedImageNeighborhoodRange<ImageType>::iterator       begin = range.begin();
  const itk::ShapedImageNeighborhoodRange<ImageType>::const_iterator const_begin_from_begin = begin;
  EXPECT_EQ(const_begin_from_begin, begin);

  const itk::ShapedImageNeighborhoodRange<ImageType>::const_iterator const_begin_from_range_begin = range.begin();
  EXPECT_EQ(const_begin_from_range_begin, range.begin());
}


// Tests that the iterators of a NeigborhoodRange can be used as first and
// second argument of an std::vector constructor.
TEST(ShapedImageNeighborhoodRange, IteratorsCanBePassedToStdVectorConstructor)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ {} };
  const itk::Size<ImageType::ImageDimension>                radius = { { 0, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<ImageType> range{ *image, location, offsets };

  // Easily store all pixels of the ShapedImageNeighborhoodRange in an std::vector:
  const std::vector<PixelType> stdVector(range.begin(), range.end());
  EXPECT_EQ(stdVector, std::vector<PixelType>(range.cbegin(), range.cend()));
  EXPECT_TRUE(std::equal(stdVector.begin(), stdVector.end(), range.cbegin()));
}


// Tests that the iterators of a NeigborhoodRange can be used as first and
// second argument of std::reverse (which requires bidirectional iterators).
TEST(ShapedImageNeighborhoodRange, IteratorsCanBePassedToStdReverseCopy)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ {} };
  const itk::Size<ImageType::ImageDimension>                radius = { { 0, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<ImageType> range{ *image, location, offsets };

  const unsigned int numberOfNeighborhoodPixels = 3;

  const std::vector<PixelType> stdVector(range.begin(), range.end());
  std::vector<PixelType>       reversedStdVector1(numberOfNeighborhoodPixels);
  std::vector<PixelType>       reversedStdVector2(numberOfNeighborhoodPixels);
  std::vector<PixelType>       reversedStdVector3(numberOfNeighborhoodPixels);

  // Checks bidirectionality of the ShapedImageNeighborhoodRange iterators!
  std::reverse_copy(stdVector.cbegin(), stdVector.cend(), reversedStdVector1.begin());
  std::reverse_copy(range.begin(), range.end(), reversedStdVector2.begin());
  std::reverse_copy(range.cbegin(), range.cend(), reversedStdVector3.begin());

  // Sanity check
  EXPECT_NE(reversedStdVector1, stdVector);
  EXPECT_NE(reversedStdVector2, stdVector);
  EXPECT_NE(reversedStdVector3, stdVector);

  // The real tests:
  EXPECT_EQ(reversedStdVector1, reversedStdVector2);
  EXPECT_EQ(reversedStdVector1, reversedStdVector3);
}


// Tests that the iterators of a NeigborhoodRange can be used as first and
// second argument of std::inner_product.
TEST(ShapedImageNeighborhoodRange, IteratorsCanBePassedToStdInnerProduct)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ {} };
  const itk::Size<ImageType::ImageDimension>                radius = { { 0, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<ImageType> range{ *image, location, offsets };

  const double innerProduct = std::inner_product(range.begin(), range.end(), range.begin(), 0.0);

  EXPECT_EQ(innerProduct, 102);
}


// Tests that the iterators of a NeigborhoodRange can be used as first and
// second argument of std::for_each.
TEST(ShapedImageNeighborhoodRange, IteratorsCanBePassedToStdForEach)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ {} };
  const itk::Size<ImageType::ImageDimension>                radius = { { 0, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<ImageType> range{ *image, location, offsets };

  std::for_each(range.begin(), range.end(), [](const PixelType pixel) { EXPECT_TRUE(pixel > 0); });
}


// Tests that a NeigborhoodRange can be used as the "range expression" of a
// C++11 range-based for loop.
TEST(ShapedImageNeighborhoodRange, CanBeUsedAsExpressionOfRangeBasedForLoop)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  using RangeType = itk::ShapedImageNeighborhoodRange<ImageType>;

  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ {} };
  const itk::Size<ImageType::ImageDimension>                radius = { { 0, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  RangeType range{ *image, location, offsets };

  for (const PixelType pixel : range)
  {
    EXPECT_NE(pixel, 42);
  }

  // Note: instead of 'iterator::reference', you may also type 'auto&&', but
  // that might trigger a warning, for example from Mac10.13-AppleClang
  // (AppleClang 9.1.0.9020039): "loop variable 'pixel' is always a copy because
  // the range of type 'ShapedImageNeighborhoodRange' does not return a reference"
  // This issue was reported to LLVM by Sean McBride, 2018-05-09, Bug 37392,
  // "Undesirable -Wrange-loop-analysis warning with auto and vector<bool>",
  // https://bugs.llvm.org/show_bug.cgi?id=37392
  for (RangeType::iterator::reference pixel : range)
  {
    pixel = 42;
  }

  for (const PixelType pixel : range)
  {
    EXPECT_EQ(pixel, 42);
  }
}


// Tests that the distance between two iterators, it1 and it2, can be obtained by
// subtraction (it2 - it1).
TEST(ShapedImageNeighborhoodRange, DistanceBetweenIteratorsCanBeObtainedBySubtraction)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImage<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ {} };
  const itk::Size<ImageType::ImageDimension>                radius = { { 2, 3 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<ImageType> range{ *image, location, offsets };

  itk::ShapedImageNeighborhoodRange<ImageType>::iterator it1 = range.begin();

  const size_t numberOfNeighborhoodPixels = range.size();

  for (size_t i1 = 0; i1 < numberOfNeighborhoodPixels; ++i1, ++it1)
  {
    itk::ShapedImageNeighborhoodRange<ImageType>::iterator it2 = it1;

    for (size_t i2 = 0; i2 < numberOfNeighborhoodPixels; ++i2, ++it2)
    {
      EXPECT_EQ(it2 - it1, std::distance(it1, it2));
    }
  }
}


// Tests that ShapedImageNeighborhoodRange::iterator retrieves the same pixel values, in the same order,
// as ConstNeighborhoodIterator.
TEST(ShapedImageNeighborhoodRange, IteratorRetrievesSamePixelValuesAsConstNeighborhoodIterator)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                initialLocation{ {} };
  const ImageType::IndexType                                testLocations[] = { { {} }, { { 0, 1 } }, { { 1, 0 } } };
  const itk::Size<ImageType::ImageDimension>                radius = { { 2, 3 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<const ImageType> range{ *image, initialLocation, offsets };
  itk::ConstNeighborhoodIterator<ImageType> constNeighborhoodIterator(radius, image, image->GetRequestedRegion());

  for (const auto & location : testLocations)
  {
    constNeighborhoodIterator.SetLocation(location);
    range.SetLocation(location);

    itk::SizeValueType i = 0;

    for (const PixelType pixel : range)
    {
      EXPECT_EQ(pixel, constNeighborhoodIterator.GetPixel(i));
      ++i;
    }
  }
}


// Tests that iterator::reference and const_iterator::reference act like a real
// (built-in) C++ reference to the pixel type.
TEST(ShapedImageNeighborhoodRange, IteratorReferenceActsLikeARealReference)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);
  using RangeType = itk::ShapedImageNeighborhoodRange<ImageType>;

  const ImageType::IndexType                                location = { { 1, 0 } };
  const itk::Size<ImageType::ImageDimension>                radius = { { 1, 0 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  RangeType           range{ *image, location, offsets };
  RangeType::iterator it = range.begin();

  RangeType::iterator::reference       reference1 = *it;
  RangeType::iterator::reference       reference2 = *(++it);
  RangeType::const_iterator::reference reference3 = *(++it);
  EXPECT_EQ(reference1, 1);
  EXPECT_EQ(reference2, 2);
  EXPECT_EQ(reference3, 3);

  RangeType::const_iterator::reference reference4 = reference1;
  RangeType::const_iterator::reference reference5 = reference2;
  RangeType::const_iterator::reference reference6 = reference3;
  EXPECT_EQ(reference4, 1);
  EXPECT_EQ(reference5, 2);
  EXPECT_EQ(reference6, 3);

  PixelType pixelValue1 = reference1;
  EXPECT_EQ(pixelValue1, reference1);

  reference1 = 42;
  EXPECT_EQ(reference1, 42);

  pixelValue1 = reference1;
  EXPECT_EQ(pixelValue1, 42);

  reference2 = reference1;
  EXPECT_EQ(reference1, 42);
  EXPECT_EQ(reference2, 42);

  reference2 = 0;
  EXPECT_EQ(reference1, 42);
  EXPECT_EQ(reference2, 0);
}


// Tests that ShapedImageNeighborhoodRange<VectorImage<T>> is supported well.
TEST(ShapedImageNeighborhoodRange, SupportsVectorImage)
{
  using ImageType = itk::VectorImage<unsigned char>;
  using PixelType = ImageType::PixelType;
  enum
  {
    vectorLength = 2,
    sizeX = 9,
    sizeY = 11,
    sizeZ = 3
  };
  const auto                         image = ImageType::New();
  const typename ImageType::SizeType imageSize = { { sizeX, sizeY, sizeZ } };
  image->SetRegions(imageSize);
  image->SetVectorLength(vectorLength);
  image->Allocate(true);
  PixelType fillPixelValue(vectorLength);
  fillPixelValue.Fill(42);
  image->FillBuffer(fillPixelValue);

  const ImageType::IndexType                                location = { { 0, 1 } };
  const itk::Size<ImageType::ImageDimension>                radius = { { 0, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);

  using RangeType = itk::ShapedImageNeighborhoodRange<ImageType>;
  RangeType range{ *image, location, offsets };

  for (PixelType pixelValue : range)
  {
    EXPECT_EQ(pixelValue, fillPixelValue);
  }

  PixelType otherPixelValue(vectorLength);
  otherPixelValue.Fill(1);
  image->SetPixel({ {} }, otherPixelValue);

  RangeType::const_iterator it = range.begin();
  const PixelType           firstPixelValueFromRange = *it;
  EXPECT_EQ(firstPixelValueFromRange, otherPixelValue);
  ++it;
  const PixelType secondPixelValueFromRange = *it;
  EXPECT_EQ(secondPixelValueFromRange, fillPixelValue);
}


TEST(ShapedImageNeighborhoodRange, IteratorsCanBePassedToStdSort)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 3,
    sizeY = 3
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ { 1, 1 } };
  const itk::Size<ImageType::ImageDimension>                radius = { { 1, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<ImageType> range{ *image, location, offsets };

  // Initial order: (1, 2, 3, ..., 9).
  const std::vector<PixelType> initiallyOrderedPixels(range.cbegin(), range.cend());
  const std::vector<PixelType> reverseOrderedPixels(initiallyOrderedPixels.rbegin(), initiallyOrderedPixels.rend());

  // Sanity checks before doing the "real" tests:
  EXPECT_EQ(std::vector<PixelType>(range.cbegin(), range.cend()), initiallyOrderedPixels);
  EXPECT_NE(std::vector<PixelType>(range.cbegin(), range.cend()), reverseOrderedPixels);

  // Test std::sort with predicate (lambda expression), to revert the order:
  std::sort(range.begin(), range.end(), [](PixelType lhs, PixelType rhs) { return rhs < lhs; });
  EXPECT_EQ(std::vector<PixelType>(range.cbegin(), range.cend()), reverseOrderedPixels);

  // Test std::sort without predicate, to go back to the initial order (1, 2, 3, ..., 9):
  std::sort(range.begin(), range.end());
  EXPECT_EQ(std::vector<PixelType>(range.cbegin(), range.cend()), initiallyOrderedPixels);
}


TEST(ShapedImageNeighborhoodRange, IteratorsCanBePassedToStdNthElement)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 3,
    sizeY = 3
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ { 1, 1 } };
  const itk::Size<ImageType::ImageDimension>                radius = { { 1, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  itk::ShapedImageNeighborhoodRange<ImageType> range{ *image, location, offsets };

  std::reverse(range.begin(), range.end());

  std::vector<PixelType> pixels(range.cbegin(), range.cend());

  // The 'n' to be used with 'nth_element':
  const size_t n = pixels.size() / 2;

  std::nth_element(pixels.begin(), pixels.begin() + n, pixels.end());

  // Sanity check, before the "real" test:
  EXPECT_NE(std::vector<PixelType>(range.cbegin(), range.cend()), pixels);

  // nth_element on the range should rearrange the pixels in the same way as
  // it did on the std::vector of pixels.
  std::nth_element(range.begin(), range.begin() + n, range.end());
  EXPECT_EQ(std::vector<PixelType>(range.cbegin(), range.cend()), pixels);
}


TEST(ShapedImageNeighborhoodRange, IteratorIsDefaultConstructible)
{
  RangeGTestUtilities::ExpectIteratorIsDefaultConstructible<itk::ShapedImageNeighborhoodRange<itk::Image<int>>>();
}


TEST(ShapedImageNeighborhoodRange, IteratorsSupportRandomAccess)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  using RangeType = itk::ShapedImageNeighborhoodRange<ImageType>;
  enum
  {
    sizeX = 3,
    sizeY = 3
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ { 1, 1 } };
  const itk::Size<ImageType::ImageDimension>                radius = { { 1, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  RangeType range{ *image, location, offsets };

  // Testing expressions from Table 111 "Random access iterator requirements
  // (in addition to bidirectional iterator)", C++11 Standard, section 24.2.7
  // "Random access iterator" [random.access.iterators].

  // Note: The 1-letter identifiers (X, a, b, n, r) and the operational semantics
  // are directly from the C++11 Standard.
  using X = RangeType::iterator;
  X a = range.begin();
  X b = range.end();

  const X initialIterator = range.begin();
  X       mutableIterator = initialIterator;
  X &     r = mutableIterator;

  using difference_type = X::difference_type;
  using reference = X::reference;

  {
    // Expression to be tested: 'r += n'
    difference_type n = 3;

    static_assert(std::is_same<decltype(r += n), X &>::value, "Return type tested");

    r = initialIterator;
    const auto expectedResult = [&r, n] {
      // Operational semantics, as specified by the C++11 Standard:
      difference_type m = n;
      if (m >= 0)
        while (m--)
          ++r;
      else
        while (m++)
          --r;
      return r;
    }();
    r = initialIterator;
    const auto actualResult = r += n;
    EXPECT_EQ(actualResult, expectedResult);
  }
  {
    // Expressions to be tested: 'a + n' and 'n + a'
    difference_type n = 3;

    static_assert(std::is_same<decltype(a + n), X>::value, "Return type tested");
    static_assert(std::is_same<decltype(n + a), X>::value, "Return type tested");

    const auto expectedResult = [a, n] {
      // Operational semantics, as specified by the C++11 Standard:
      X tmp = a;
      return tmp += n;
    }();

    EXPECT_EQ(a + n, expectedResult);
    EXPECT_TRUE(a + n == n + a);
  }
  {
    // Expression to be tested: 'r -= n'
    difference_type n = 3;

    static_assert(std::is_same<decltype(r -= n), X &>::value, "Return type tested");

    r = initialIterator;
    const auto expectedResult = [&r, n] {
      // Operational semantics, as specified by the C++11 Standard:
      return r += -n;
    }();
    r = initialIterator;
    const auto actualResult = r -= n;
    EXPECT_EQ(actualResult, expectedResult);
  }
  {
    // Expression to be tested: 'a - n'
    difference_type n = -3;

    static_assert(std::is_same<decltype(a - n), X>::value, "Return type tested");

    const auto expectedResult = [a, n] {
      // Operational semantics, as specified by the C++11 Standard:
      X tmp = a;
      return tmp -= n;
    }();

    EXPECT_EQ(a - n, expectedResult);
  }
  {
    // Expression to be tested: 'b - a'
    static_assert(std::is_same<decltype(b - a), difference_type>::value, "Return type tested");

    difference_type n = b - a;
    EXPECT_TRUE(a + n == b);
    EXPECT_TRUE(b == a + (b - a));
  }
  {
    // Expression to be tested: 'a[n]'
    difference_type n = 3;
    static_assert(std::is_convertible<decltype(a[n]), reference>::value, "Return type tested");
    EXPECT_EQ(a[n], *(a + n));
  }
  {
    // Expressions to be tested: 'a < b', 'a > b', 'a >= b', and 'a <= b':
    static_assert(std::is_convertible<decltype(a < b), bool>::value, "Return type tested");
    static_assert(std::is_convertible<decltype(a > b), bool>::value, "Return type tested");
    static_assert(std::is_convertible<decltype(a >= b), bool>::value, "Return type tested");
    static_assert(std::is_convertible<decltype(a <= b), bool>::value, "Return type tested");
    EXPECT_EQ(a<b, b - a> 0);
    EXPECT_EQ(a > b, b < a);
    EXPECT_EQ(a >= b, !(a < b));
    EXPECT_EQ(a <= b, !(b < a));
  }
}


TEST(ShapedImageNeighborhoodRange, SupportsSubscript)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  using RangeType = itk::ShapedImageNeighborhoodRange<ImageType>;

  enum
  {
    sizeX = 3,
    sizeY = 3
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ { 1, 1 } };
  const itk::Size<ImageType::ImageDimension>                radius = { { 1, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  RangeType range{ *image, location, offsets };

  const size_t numberOfNeighbors = range.size();

  RangeType::iterator it = range.begin();

  for (size_t i = 0; i < numberOfNeighbors; ++i)
  {
    RangeType::iterator::reference neighbor = range[i];
    EXPECT_EQ(neighbor, *it);
    ++it;
  }
}


TEST(ShapedImageNeighborhoodRange, ProvidesReverseIterators)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  using RangeType = itk::ShapedImageNeighborhoodRange<ImageType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  const ImageType::IndexType                                location{ {} };
  const itk::Size<ImageType::ImageDimension>                radius = { { 0, 1 } };
  const std::vector<itk::Offset<ImageType::ImageDimension>> offsets =
    itk::GenerateRectangularImageNeighborhoodOffsets(radius);
  RangeType range{ *image, location, offsets };

  const unsigned int numberOfNeighborhoodPixels = 3;

  const std::vector<PixelType> stdVector(range.begin(), range.end());
  std::vector<PixelType>       reversedStdVector1(numberOfNeighborhoodPixels);
  std::vector<PixelType>       reversedStdVector2(numberOfNeighborhoodPixels);
  std::vector<PixelType>       reversedStdVector3(numberOfNeighborhoodPixels);

  std::reverse_copy(stdVector.cbegin(), stdVector.cend(), reversedStdVector1.begin());

  const RangeType::const_reverse_iterator crbegin = range.crbegin();
  const RangeType::const_reverse_iterator crend = range.crend();
  const RangeType::reverse_iterator       rbegin = range.rbegin();
  const RangeType::reverse_iterator       rend = range.rend();

  EXPECT_EQ(crbegin, rbegin);
  EXPECT_EQ(crend, rend);

  std::copy(crbegin, crend, reversedStdVector2.begin());
  std::copy(rbegin, rend, reversedStdVector3.begin());

  // Sanity check
  EXPECT_NE(reversedStdVector1, stdVector);
  EXPECT_NE(reversedStdVector2, stdVector);
  EXPECT_NE(reversedStdVector3, stdVector);

  // The real tests:
  EXPECT_EQ(reversedStdVector1, reversedStdVector2);
  EXPECT_EQ(reversedStdVector1, reversedStdVector3);
}


TEST(ShapedImageNeighborhoodRange, ConstructorSupportsRValueShapeOffsets)
{
  using ImageType = itk::Image<unsigned char>;
  using RangeType = itk::ShapedImageNeighborhoodRange<ImageType>;
  using OffsetType = ImageType::OffsetType;

  const auto                 image = CreateImage<ImageType>(1, 2);
  const ImageType::IndexType location{ { 1, 1 } };

  // Note that the expression 'std::vector<OffsetType>{1}' is an rvalue.
  // The code is carefully written so that this rvalue remains alive while
  // the range 'RangeType{...}' is being used.
  ASSERT_EQ((RangeType{ *image, location, std::vector<OffsetType>{ 1 } }).size(), 1);
}


// Tests that an arbitrary (possibly non-zero) index of the buffered region is supported.
TEST(ShapedImageNeighborhoodRange, SupportsArbitraryBufferedRegionIndex)
{
  using ImageType = itk::Image<int>;

  // Do zero-padding for neighborhood pixels outside the image:
  using PolicyType = itk::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<ImageType>;
  using RangeType = itk::ShapedImageNeighborhoodRange<ImageType, PolicyType>;

  // A minimal radius, and trivial shape offsets (n = 1), just for the test.
  const itk::Size<ImageType::ImageDimension> radius = { { 0 } };
  const auto                                 offsets = itk::GenerateRectangularImageNeighborhoodOffsets(radius);

  const ImageType::IndexType zeroIndex{ { 0 } };
  const ImageType::IndexType arbitraryIndex{ { 10, -1 } };
  const ImageType::SizeType  imageSize{ { 2, 3 } };

  const ImageType::RegionType bufferedRegion{ arbitraryIndex, imageSize };

  const auto image = ImageType::New();
  image->SetRegions(bufferedRegion);
  image->Allocate(true);

  // Set a 'magic value' at the begin of the buffered region.
  const ImageType::PixelType   magicPixelValue = 42;
  ImageType::PixelType * const bufferPointer = image->GetBufferPointer();
  *bufferPointer = magicPixelValue;

  RangeType range{ *image, zeroIndex, offsets };

  // Expect zero, because zeroIndex is outside the buffered region.
  EXPECT_EQ(range[0], 0);

  range.SetLocation(arbitraryIndex);

  // Expect the magic value as the first range value, because the range is now
  // located at the arbitrary index, which corresponds to the begin of the
  // image buffer.
  EXPECT_EQ(range[0], magicPixelValue);

  // Expect the magic value also when a range is constructed with
  // the arbitrary index as its location.
  EXPECT_EQ((RangeType{ *image, arbitraryIndex, offsets })[0], magicPixelValue);

  // Check to see that this range yields the same pixel value as an
  // itk::ConstNeighborhoodIterator with this arbitrary buffered region index.
  using ConstNeighborhoodIteratorType =
    itk::ConstNeighborhoodIterator<ImageType, itk::ConstantBoundaryCondition<ImageType>>;
  ConstNeighborhoodIteratorType neighborhoodIterator(radius, image, image->GetRequestedRegion());

  // Note: Set NeedToUseBoundaryCondition, to ensure that 'neighborhoodIterator'
  // also supports any arbitrary buffered region index at any location!
  neighborhoodIterator.NeedToUseBoundaryConditionOn();

  range.SetLocation(zeroIndex);
  neighborhoodIterator.SetLocation(zeroIndex);
  EXPECT_EQ(range[0], neighborhoodIterator.GetPixel(0));

  range.SetLocation(arbitraryIndex);
  neighborhoodIterator.SetLocation(arbitraryIndex);
  EXPECT_EQ(range[0], neighborhoodIterator.GetPixel(0));
}


// Tests that begin() == end() for a default-constructed range.
TEST(ShapedImageNeighborhoodRange, BeginIsEndWhenDefaultConstructed)
{
  RangeGTestUtilities::ExpectBeginIsEndWhenRangeIsDefaultConstructed<ShapedImageNeighborhoodRange<itk::Image<int>>>();
  RangeGTestUtilities::ExpectBeginIsEndWhenRangeIsDefaultConstructed<
    ShapedImageNeighborhoodRange<itk::VectorImage<int>>>();
}


// Tests that size() returns 0 for a default-constructed range.
TEST(ShapedImageNeighborhoodRange, SizeIsZeroWhenDefaultConstructed)
{
  RangeGTestUtilities::ExpectZeroSizeWhenRangeIsDefaultConstructed<ShapedImageNeighborhoodRange<itk::Image<int>>>();
  RangeGTestUtilities::ExpectZeroSizeWhenRangeIsDefaultConstructed<
    ShapedImageNeighborhoodRange<itk::VectorImage<int>>>();
}


// Tests empty() for a default-constructed range.
TEST(ShapedImageNeighborhoodRange, IsEmptyWhenDefaultConstructed)
{
  RangeGTestUtilities::ExpectRangeIsEmptyWhenDefaultConstructed<ShapedImageNeighborhoodRange<itk::Image<int>>>();
  RangeGTestUtilities::ExpectRangeIsEmptyWhenDefaultConstructed<ShapedImageNeighborhoodRange<itk::VectorImage<int>>>();
}


// Tests range.empty() for an empty container of shape offsets.
TEST(ShapedImageNeighborhoodRange, IsEmptyForAnEmptyContainerOfShapeOffsets)
{
  ExpectRangeIsEmptyForAnEmptyContainerOfShapeOffsets<itk::Image<int>>();
  ExpectRangeIsEmptyForAnEmptyContainerOfShapeOffsets<itk::VectorImage<int>>();
}


// Tests that range.empty() returns false for a non-empty image, with a
// non-empty container of shape offsets.
TEST(ShapedImageNeighborhoodRange, IsNotEmptyWhenImageAndShapeOffsetContainerAreNonEmpty)
{
  ExpectRangeIsNotEmptyForNonEmptyImageAndShapeOffsetContainer<itk::Image<int>>();
  ExpectRangeIsNotEmptyForNonEmptyImageAndShapeOffsetContainer<itk::VectorImage<int>>();
}


// Tests that a copy-constructed range has the same iterators (begin and end) as the original.
TEST(ShapedImageNeighborhoodRange, CopyConstructedRangeHasSameIterators)
{
  ExpectCopyConstructedRangeHasSameIteratorsAsOriginal<itk::Image<int>>();
  ExpectCopyConstructedRangeHasSameIteratorsAsOriginal<itk::VectorImage<int>>();
}


// Tests that a copy-assigned range has the same iterators (begin and end) as the original.
TEST(ShapedImageNeighborhoodRange, CopyAssignedRangeHasSameIterators)
{
  ExpectCopyAssignedRangeHasSameIteratorsAsOriginal<itk::Image<int>>();
  ExpectCopyAssignedRangeHasSameIteratorsAsOriginal<itk::VectorImage<int>>();
}


// Tests that a move-constructed range has the same iterators as the original, before the move.
TEST(ShapedImageNeighborhoodRange, MoveConstructedRangeHasSameIterators)
{
  ExpectMoveConstructedRangeHasSameIteratorsAsOriginalBeforeMove<itk::Image<int>>();
  ExpectMoveConstructedRangeHasSameIteratorsAsOriginalBeforeMove<itk::VectorImage<int>>();
}


// Tests that a move-assigned range has the same iterators as the original, before the move.
TEST(ShapedImageNeighborhoodRange, MoveAssignedRangeHasSameIterators)
{
  ExpectMoveAssignedRangeHasSameIteratorsAsOriginalBeforeMove<itk::Image<int>>();
  ExpectMoveAssignedRangeHasSameIteratorsAsOriginalBeforeMove<itk::VectorImage<int>>();
}
