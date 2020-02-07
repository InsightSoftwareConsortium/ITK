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
#include "itkImageRegionRange.h"

#include "itkImage.h"
#include "itkImageBufferRange.h"
#include "itkImageRegionIterator.h"
#include "itkMacro.h" // For itkNotUsed.
#include "itkRGBPixel.h"
#include "itkRangeGTestUtilities.h"
#include "itkVectorImage.h"

#include <gtest/gtest.h>
#include <algorithm>   // For std::reverse_copy, std::equal, etc.
#include <numeric>     // For std::inner_product
#include <type_traits> // For std::is_reference.

// Test template instantiations for various ImageDimension values, and const Image:
template class itk::Experimental::ImageRegionRange<itk::Image<short, 1>>;
template class itk::Experimental::ImageRegionRange<itk::Image<short, 2>>;
template class itk::Experimental::ImageRegionRange<itk::Image<short, 3>>;
template class itk::Experimental::ImageRegionRange<itk::Image<short, 4>>;
template class itk::Experimental::ImageRegionRange<const itk::Image<short>>;
template class itk::Experimental::ImageRegionRange<itk::VectorImage<short>>;
template class itk::Experimental::ImageRegionRange<const itk::VectorImage<short>>;
template class itk::Experimental::ImageRegionRange<itk::Image<itk::RGBPixel<std::uint8_t>>>;
template class itk::Experimental::ImageRegionRange<itk::Image<itk::Vector<long, 11>>>;

using itk::Experimental::ImageRegionRange;


namespace
{
// Tells whether or not ImageRegionRange<TImage>::iterator::operator*() returns a reference.
// (If it does not return a reference, it actually returns a proxy to the pixel.)
template <typename TImage>
constexpr bool
DoesImageRegionRangeIteratorDereferenceOperatorReturnReference()
{
  using IteratorType = typename ImageRegionRange<TImage>::iterator;

  return std::is_reference<decltype(*std::declval<IteratorType>())>::value;
}


static_assert(DoesImageRegionRangeIteratorDereferenceOperatorReturnReference<itk::Image<int>>(),
              "ImageRegionRange::iterator::operator*() should return a reference for an itk::Image.");
static_assert(DoesImageRegionRangeIteratorDereferenceOperatorReturnReference<const itk::Image<int>>(),
              "ImageRegionRange::iterator::operator*() should return a reference for a 'const' itk::Image.");
static_assert(!DoesImageRegionRangeIteratorDereferenceOperatorReturnReference<itk::VectorImage<int>>(),
              "ImageRegionRange::iterator::operator*() should not return a reference for an itk::VectorImage.");
static_assert(!DoesImageRegionRangeIteratorDereferenceOperatorReturnReference<const itk::VectorImage<int>>(),
              "ImageRegionRange::iterator::operator*() should not return a reference for a 'const' itk::VectorImage.");


// Tells whether or not ImageRegionRange<TImage>::iterator is the same type
// as ImageRegionRange<TImage>::const_iterator.
template <typename TImage>
constexpr bool
IsIteratorTypeTheSameAsConstIteratorType()
{
  using RangeType = ImageRegionRange<TImage>;

  return std::is_same<typename RangeType::iterator, typename RangeType::const_iterator>::value;
}


static_assert(!IsIteratorTypeTheSameAsConstIteratorType<itk::Image<int>>() &&
                !IsIteratorTypeTheSameAsConstIteratorType<itk::VectorImage<int>>(),
              "For a non-const image, non-const iterator and const_iterator should be different types!");
static_assert(IsIteratorTypeTheSameAsConstIteratorType<const itk::Image<int>>() &&
                IsIteratorTypeTheSameAsConstIteratorType<const itk::VectorImage<int>>(),
              "For a const image, non-const iterator and const_iterator should be the same type!");


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
  const auto                                        image = CreateImage<TImage>(sizeX, sizeY);
  const itk::Experimental::ImageBufferRange<TImage> imageBufferRange{ *image };
  std::iota(imageBufferRange.begin(), imageBufferRange.end(), PixelType{ 1 });
  return image;
}


template <typename TPixel, unsigned VImageDimension>
void
SetVectorLengthIfImageIsVectorImage(itk::VectorImage<TPixel, VImageDimension> & image, const unsigned vectorLength)
{
  image.SetVectorLength(vectorLength);
}


template <typename TPixel, unsigned VImageDimension>
void
SetVectorLengthIfImageIsVectorImage(itk::Image<TPixel, VImageDimension> & itkNotUsed(image),
                                    const unsigned                        itkNotUsed(vectorLength))
{
  // Do not set the VectorLength. The specified image is not a VectorImage.
}


template <typename TRange>
void
ExpectBeginIsEndWhenRangeIsDefaultConstructed()
{
  TRange defaultConstructedRange;
  EXPECT_EQ(defaultConstructedRange.begin(), defaultConstructedRange.end());
}


template <typename TRange>
void
ExpectRangeIsEmptyWhenDefaultConstructed()
{
  TRange defaultConstructedRange;
  EXPECT_TRUE(defaultConstructedRange.empty());
}


template <typename TImage>
void
ExpectRangeIsNotEmptyForNonEmptyImage()
{
  // First create a non-empty image:
  const auto                image = TImage::New();
  typename TImage::SizeType imageSize;
  imageSize.Fill(1);
  image->SetRegions(imageSize);
  SetVectorLengthIfImageIsVectorImage(*image, 1);
  image->Allocate();

  EXPECT_FALSE((ImageRegionRange<TImage>{ *image }.empty()));
}


template <typename TImage>
void
Expect_ImageRegionRange_iterates_forward_over_same_pixels_as_ImageRegionIterator(
  TImage &                                         image,
  const itk::ImageRegion<TImage::ImageDimension> & iterationRegion)
{
  using PixelType = typename TImage::PixelType;

  itk::ImageRegionIterator<TImage> imageRegionIterator{ &image, iterationRegion };
  ImageRegionRange<TImage>         range{ image, iterationRegion };

  ASSERT_TRUE(imageRegionIterator.IsAtBegin());

  for (const PixelType pixel : range)
  {
    ASSERT_FALSE(imageRegionIterator.IsAtEnd());
    EXPECT_EQ(pixel, imageRegionIterator.Get());
    ++imageRegionIterator;
  }
  EXPECT_TRUE(imageRegionIterator.IsAtEnd());
}

template <typename TImage>
void
Expect_ImageRegionRange_iterates_backward_over_same_pixels_as_ImageRegionIterator(
  TImage &                                         image,
  const itk::ImageRegion<TImage::ImageDimension> & iterationRegion)
{
  itk::ImageRegionIterator<TImage> imageRegionIterator{ &image, iterationRegion };
  ImageRegionRange<TImage>         range{ image, iterationRegion };

  auto rangeIterator = range.cend();
  imageRegionIterator.GoToEnd();

  while (!imageRegionIterator.IsAtBegin())
  {
    ASSERT_NE(rangeIterator, range.begin());
    --imageRegionIterator;
    --rangeIterator;
    EXPECT_EQ(*rangeIterator, imageRegionIterator.Get());
  };

  EXPECT_EQ(rangeIterator, range.begin());
}


#ifdef NDEBUG
template <typename TImage>
void
Check_Range_constructor_throws_ExceptionObject_when_iteration_region_is_outside_of_buffered_region(
  TImage &                            image,
  const typename TImage::RegionType & region)
{
  try
  {
    const ImageRegionRange<TImage> range{ image, region };

    // The test fails when the construction of this range does not trigger an exception.
    GTEST_FAIL();
  }
  catch (const itk::ExceptionObject & exceptionObject)
  {
    const char * const description = exceptionObject.GetDescription();
    ASSERT_NE(description, nullptr);
    EXPECT_TRUE(std::strstr(description, "outside of buffered region") != nullptr);
  }
}
#endif

} // namespace


// Tests that a begin iterator compares equal to another begin iterator of the
// same range. Also does this test for end iterators.
TEST(ImageRegionRange, EquivalentBeginOrEndIteratorsCompareEqual)
{
  using ImageType = itk::Image<int>;
  using RangeType = ImageRegionRange<ImageType>;

  const auto image = CreateImage<ImageType>(2, 3);

  const RangeType range{ *image };

  const RangeType::iterator       begin = range.begin();
  const RangeType::iterator       end = range.end();
  const RangeType::const_iterator cbegin = range.cbegin();
  const RangeType::const_iterator cend = range.cend();

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


TEST(ImageRegionRange, BeginAndEndOfNonEmptyImageRegionAreNotEqual)
{
  using ImageType = itk::Image<int>;

  constexpr auto                         ImageDimension = ImageType::ImageDimension;
  const auto                             image = CreateImage<ImageType>(2, 3);
  const itk::ImageRegion<ImageDimension> region{ itk::Size<ImageDimension>::Filled(2) };

  ImageRegionRange<ImageType> range{ *image, region };

  EXPECT_FALSE(range.begin() == range.end());
  EXPECT_NE(range.begin(), range.end());
}


// Tests that an iterator converts (implicitly) to a const_iterator.
TEST(ImageRegionRange, IteratorConvertsToConstIterator)
{
  using ImageType = itk::Image<int>;
  using RangeType = ImageRegionRange<ImageType>;

  constexpr auto                         ImageDimension = ImageType::ImageDimension;
  const auto                             image = CreateImage<ImageType>(2, 3);
  const itk::ImageRegion<ImageDimension> region{ itk::Size<ImageDimension>::Filled(2) };

  RangeType range{ *image, region };

  const RangeType::iterator       begin = range.begin();
  const RangeType::const_iterator const_begin_from_begin = begin;
  EXPECT_EQ(const_begin_from_begin, begin);

  const RangeType::const_iterator const_begin_from_range_begin = range.begin();
  EXPECT_EQ(const_begin_from_range_begin, range.begin());
}


// Tests that the iterators of an ImageRegionRange can be used as first and
// second argument of an std::vector constructor.
TEST(ImageRegionRange, IteratorsCanBePassedToStdVectorConstructor)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  constexpr auto ImageDimension = ImageType::ImageDimension;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto                             image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);
  const itk::ImageRegion<ImageDimension> region{ itk::Size<ImageDimension>::Filled(2) };

  ImageRegionRange<ImageType> range{ *image, region };

  // Easily store all pixels of the ImageRegionRange in an std::vector:
  const std::vector<PixelType> stdVector(range.begin(), range.end());
  EXPECT_EQ(stdVector, std::vector<PixelType>(range.cbegin(), range.cend()));
  EXPECT_TRUE(std::equal(stdVector.begin(), stdVector.end(), range.cbegin()));
}


// Tests that the iterators can be used as first and
// second argument of std::reverse (which requires bidirectional iterators).
TEST(ImageRegionRange, IteratorsCanBePassedToStdReverseCopy)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  constexpr auto ImageDimension = ImageType::ImageDimension;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto                             image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);
  const itk::ImageRegion<ImageDimension> region{ itk::Size<ImageDimension>::Filled(2) };

  ImageRegionRange<ImageType> range{ *image, region };

  const unsigned numberOfPixels = sizeX * sizeY;

  const std::vector<PixelType> stdVector(range.begin(), range.end());
  std::vector<PixelType>       reversedStdVector1(numberOfPixels);
  std::vector<PixelType>       reversedStdVector2(numberOfPixels);
  std::vector<PixelType>       reversedStdVector3(numberOfPixels);

  // Checks bidirectionality of the ImageRegionRange iterators!
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


// Tests that the iterators can be used as first and
// second argument of std::inner_product.
TEST(ImageRegionRange, IteratorsCanBePassedToStdInnerProduct)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  constexpr auto ImageDimension = ImageType::ImageDimension;
  enum
  {
    sizeX = 2,
    sizeY = 2
  };
  const auto                             image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);
  const itk::ImageRegion<ImageDimension> region{ itk::Size<ImageDimension>::Filled(2) };

  ImageRegionRange<ImageType> range{ *image, region };

  const double innerProduct = std::inner_product(range.begin(), range.end(), range.begin(), 0.0);

  EXPECT_EQ(innerProduct, 30);
}


// Tests that the iterators can be used as first and
// second argument of std::for_each.
TEST(ImageRegionRange, IteratorsCanBePassedToStdForEach)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  constexpr auto ImageDimension = ImageType::ImageDimension;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto                             image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);
  const itk::ImageRegion<ImageDimension> region{ itk::Size<ImageDimension>::Filled(2) };

  ImageRegionRange<ImageType> range{ *image, region };

  std::for_each(range.begin(), range.end(), [](const PixelType pixel) { EXPECT_TRUE(pixel > 0); });
}


// Tests that an ImageRegionRange can be used as the "range expression" of a
// C++11 range-based for loop.
TEST(ImageRegionRange, CanBeUsedAsExpressionOfRangeBasedForLoop)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  constexpr auto ImageDimension = ImageType::ImageDimension;
  using RangeType = ImageRegionRange<ImageType>;

  enum
  {
    sizeX = 2,
    sizeY = 3
  };
  const auto                             image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);
  const itk::ImageRegion<ImageDimension> region{ itk::Size<ImageDimension>::Filled(2) };

  RangeType range{ *image, region };

  for (const PixelType pixel : range)
  {
    EXPECT_NE(pixel, 42);
  }

  for (auto && pixel : range)
  {
    pixel = 42;
  }

  for (const PixelType pixel : range)
  {
    EXPECT_EQ(pixel, 42);
  }
}


// Tests that ImageRegionRange<VectorImage<T>> is supported well.
TEST(ImageRegionRange, SupportsVectorImage)
{
  using ImageType = itk::VectorImage<unsigned char>;
  constexpr auto ImageDimension = ImageType::ImageDimension;
  using PixelType = ImageType::PixelType;
  enum
  {
    vectorLength = 2,
    sizeX = 2,
    sizeY = 2,
    sizeZ = 2
  };
  const auto                         image = ImageType::New();
  const typename ImageType::SizeType imageSize = { { sizeX, sizeY, sizeZ } };
  image->SetRegions(imageSize);
  image->SetVectorLength(vectorLength);
  image->Allocate(true);
  PixelType fillPixelValue(vectorLength);
  fillPixelValue.Fill(42);
  image->FillBuffer(fillPixelValue);
  const itk::ImageRegion<ImageDimension> region{ itk::Size<ImageDimension>::Filled(2) };

  using RangeType = ImageRegionRange<ImageType>;
  RangeType range{ *image, region };

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


TEST(ImageRegionRange, IteratorIsDefaultConstructible)
{
  using RangeType = ImageRegionRange<itk::Image<int>>;

  RangeType::iterator defaultConstructedIterator{};

  // Test that a default-constructed iterator behaves according to C++ proposal
  // N3644, "Null Forward Iterators" by Alan Talbot, which is accepted with
  // C++14: "value-initialized iterators may be compared and shall compare
  // equal to other value-initialized iterators of the same type."
  // http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3644.pdf

  EXPECT_TRUE(defaultConstructedIterator == defaultConstructedIterator);
  EXPECT_FALSE(defaultConstructedIterator != defaultConstructedIterator);
  EXPECT_EQ(defaultConstructedIterator, RangeType::iterator{});
}


TEST(ImageRegionRange, ProvidesReverseIterators)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  constexpr auto ImageDimension = ImageType::ImageDimension;
  using RangeType = ImageRegionRange<ImageType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto                             image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);
  const itk::ImageRegion<ImageDimension> region{ itk::Size<ImageDimension>::Filled(2) };

  RangeType range{ *image, region };

  const unsigned numberOfPixels = sizeX * sizeY;

  const std::vector<PixelType> stdVector(range.begin(), range.end());
  std::vector<PixelType>       reversedStdVector1(numberOfPixels);
  std::vector<PixelType>       reversedStdVector2(numberOfPixels);
  std::vector<PixelType>       reversedStdVector3(numberOfPixels);

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


// Tests that begin() == end() for a default-constructed range.
TEST(ImageRegionRange, BeginIsEndWhenDefaultConstructed)
{
  ExpectBeginIsEndWhenRangeIsDefaultConstructed<ImageRegionRange<itk::Image<int>>>();
  ExpectBeginIsEndWhenRangeIsDefaultConstructed<ImageRegionRange<itk::VectorImage<int>>>();
}


// Tests empty() for a default-constructed range.
TEST(ImageRegionRange, IsEmptyWhenDefaultConstructed)
{
  ExpectRangeIsEmptyWhenDefaultConstructed<ImageRegionRange<itk::Image<int>>>();
  ExpectRangeIsEmptyWhenDefaultConstructed<ImageRegionRange<itk::VectorImage<int>>>();
}


// Tests that range.empty() returns false for a non-empty image.
TEST(ImageRegionRange, IsNotEmptyWhenImageIsNonEmpty)
{
  ExpectRangeIsNotEmptyForNonEmptyImage<itk::Image<int>>();
  ExpectRangeIsNotEmptyForNonEmptyImage<itk::VectorImage<int>>();
}


TEST(ImageRegionRange, IteratesForwardOverSamePixelsAsImageRegionIterator)
{
  using ImageType = itk::Image<unsigned char>;

  using PixelType = ImageType::PixelType;
  using IndexType = ImageType::IndexType;
  using SizeType = ImageType::SizeType;
  using RegionType = ImageType::RegionType;

  const auto image = ImageType::New();
  image->SetRegions(RegionType{ IndexType{ { -1, -2 } }, SizeType{ { 9, 11 } } });
  image->Allocate();
  const itk::Experimental::ImageBufferRange<ImageType> imageBufferRange{ *image };
  std::iota(imageBufferRange.begin(), imageBufferRange.end(), PixelType{ 1 });

  Expect_ImageRegionRange_iterates_forward_over_same_pixels_as_ImageRegionIterator(
    *image, RegionType{ IndexType{ { 1, 2 } }, SizeType{ { 3, 4 } } });
}


TEST(ImageRegionRange, IteratesBackwardOverSamePixelsAsImageRegionIterator)
{
  using ImageType = itk::Image<unsigned char>;

  using PixelType = ImageType::PixelType;
  using IndexType = ImageType::IndexType;
  using SizeType = ImageType::SizeType;
  using RegionType = ImageType::RegionType;

  const auto image = ImageType::New();
  image->SetRegions(RegionType{ IndexType{ { -1, -2 } }, SizeType{ { 9, 11 } } });
  image->Allocate();
  const itk::Experimental::ImageBufferRange<ImageType> imageBufferRange{ *image };
  std::iota(imageBufferRange.begin(), imageBufferRange.end(), PixelType{ 1 });

  Expect_ImageRegionRange_iterates_backward_over_same_pixels_as_ImageRegionIterator(
    *image, RegionType{ IndexType{ { 1, 2 } }, SizeType{ { 3, 4 } } });
}


// Tests that a range constructor throws an itk::ExceptionObject when the
// iteration region is not entirely within the buffered region.
TEST(ImageRegionRange, ThrowsInReleaseWhenIterationRegionIsOutsideBufferedRegion)
{
#ifdef NDEBUG
  // NDEBUG suggests that we are in Release mode.

  using ImageType = itk::Image<unsigned char>;

  using IndexType = ImageType::IndexType;
  using SizeType = ImageType::SizeType;
  using RegionType = ImageType::RegionType;

  const auto image = ImageType::New();

  const IndexType imageIndex{ { -1, -2 } };
  const SizeType  imageSize{ { 3, 4 } };

  image->SetRegions(RegionType{ imageIndex, imageSize });
  image->Allocate(true);

  Check_Range_constructor_throws_ExceptionObject_when_iteration_region_is_outside_of_buffered_region(
    *image, RegionType{ imageIndex, imageSize + SizeType::Filled(1) });

  Check_Range_constructor_throws_ExceptionObject_when_iteration_region_is_outside_of_buffered_region(
    *image, RegionType{ imageIndex - SizeType::Filled(1), imageSize });

#endif
}
