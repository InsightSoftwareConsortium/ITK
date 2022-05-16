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
#include "itkImageBufferRange.h"

#include "itkImage.h"
#include "itkMacro.h" // For itkNotUsed.
#include "itkRangeGTestUtilities.h"
#include "itkVectorImage.h"

#include <gtest/gtest.h>
#include <algorithm>   // For std::reverse_copy, std::equal, etc.
#include <numeric>     // For std::inner_product
#include <type_traits> // For std::is_reference.

// Test template instantiations for various ImageDimension values, and const Image:
template class itk::ImageBufferRange<itk::Image<short, 1>>;
template class itk::ImageBufferRange<itk::Image<short, 2>>;
template class itk::ImageBufferRange<itk::Image<short, 3>>;
template class itk::ImageBufferRange<itk::Image<short, 4>>;
template class itk::ImageBufferRange<const itk::Image<short>>;
template class itk::ImageBufferRange<itk::VectorImage<short>>;
template class itk::ImageBufferRange<const itk::VectorImage<short, 4>>;

using itk::ImageBufferRange;
using itk::MakeImageBufferRange;
using itk::RangeGTestUtilities;


namespace
{
// Tells whether or not ImageBufferRange<TImage>::iterator::operator*() returns a reference.
// (If it does not return a reference, it actually returns a proxy to the pixel.)
template <typename TImage>
constexpr bool
DoesImageBufferRangeIteratorDereferenceOperatorReturnReference()
{
  using IteratorType = typename ImageBufferRange<TImage>::iterator;

  return std::is_reference<decltype(*std::declval<IteratorType>())>::value;
}


static_assert(DoesImageBufferRangeIteratorDereferenceOperatorReturnReference<itk::Image<int>>(),
              "ImageBufferRange::iterator::operator*() should return a reference for an itk::Image.");
static_assert(DoesImageBufferRangeIteratorDereferenceOperatorReturnReference<const itk::Image<int>>(),
              "ImageBufferRange::iterator::operator*() should return a reference for a 'const' itk::Image.");
static_assert(!DoesImageBufferRangeIteratorDereferenceOperatorReturnReference<itk::VectorImage<int>>(),
              "ImageBufferRange::iterator::operator*() should not return a reference for an itk::VectorImage.");
static_assert(!DoesImageBufferRangeIteratorDereferenceOperatorReturnReference<const itk::VectorImage<int>>(),
              "ImageBufferRange::iterator::operator*() should not return a reference for a 'const' itk::VectorImage.");


// Tells whether or not ImageBufferRange<TImage>::iterator is the same type
// as ImageBufferRange<TImage>::const_iterator.
template <typename TImage>
constexpr bool
IsIteratorTypeTheSameAsConstIteratorType()
{
  using RangeType = ImageBufferRange<TImage>;

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
CreateImage(const unsigned int sizeX, const unsigned int sizeY)
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
ExpectRangeIsNotEmptyForNonEmptyImage()
{
  // First create a non-empty image:
  const auto                image = TImage::New();
  typename TImage::SizeType imageSize;
  imageSize.Fill(1);
  image->SetRegions(imageSize);
  SetVectorLengthIfImageIsVectorImage(*image, 1);
  image->Allocate();

  EXPECT_FALSE(ImageBufferRange<TImage>{ *image }.empty());
}


template <typename TImage>
void
ExpectMakeImageBufferRangeReturnsEmptyRangeForNullptr()
{
  TImage * const imageNullptr = nullptr;
  EXPECT_TRUE(MakeImageBufferRange(imageNullptr).empty());
}


template <typename TImage>
void
ExpectMakeImageBufferRangeReturnsCorrectRangeForNonEmptyImage()
{
  // First create a non-empty image:
  const auto                image = TImage::New();
  typename TImage::SizeType imageSize;
  imageSize.Fill(1);
  image->SetRegions(imageSize);
  SetVectorLengthIfImageIsVectorImage(*image, 1);
  image->Allocate();

  auto && imageRef = *image;

  const auto expectedRange = ImageBufferRange<TImage>{ imageRef };
  const auto actualRange = MakeImageBufferRange(&imageRef);

  EXPECT_EQ(actualRange.begin(), expectedRange.begin());
  EXPECT_EQ(actualRange.end(), expectedRange.end());
}


template <typename TImage>
void
ExpectCopyConstructedRangeHasSameIteratorsAsOriginal()
{
  const auto                     image = CreateSmallImage<TImage>();
  const ImageBufferRange<TImage> originalRange{ *image };

  RangeGTestUtilities::ExpectCopyConstructedRangeHasSameIteratorsAsOriginal(originalRange);
}


template <typename TImage>
void
ExpectCopyAssignedRangeHasSameIteratorsAsOriginal()
{
  const auto                     image = CreateSmallImage<TImage>();
  const ImageBufferRange<TImage> originalRange{ *image };

  RangeGTestUtilities::ExpectCopyAssignedRangeHasSameIteratorsAsOriginal(originalRange);
}


template <typename TImage>
void
ExpectMoveConstructedRangeHasSameIteratorsAsOriginalBeforeMove()
{
  const auto image = CreateSmallImage<TImage>();

  RangeGTestUtilities::ExpectMoveConstructedRangeHasSameIteratorsAsOriginalBeforeMove(
    ImageBufferRange<TImage>{ *image });
}


template <typename TImage>
void
ExpectMoveAssignedRangeHasSameIteratorsAsOriginalBeforeMove()
{
  const auto image = CreateSmallImage<TImage>();

  RangeGTestUtilities::ExpectMoveAssignedRangeHasSameIteratorsAsOriginalBeforeMove(ImageBufferRange<TImage>{ *image });
}

} // namespace


// Tests that a begin iterator compares equal to another begin iterator of the
// same range. Also does this test for end iterators.
TEST(ImageBufferRange, EquivalentBeginOrEndIteratorsCompareEqual)
{
  using ImageType = itk::Image<int>;

  const auto image = CreateImage<ImageType>(2, 3);

  ImageBufferRange<ImageType> range{ *image };

  const ImageBufferRange<ImageType>::iterator       begin = range.begin();
  const ImageBufferRange<ImageType>::iterator       end = range.end();
  const ImageBufferRange<ImageType>::const_iterator cbegin = range.cbegin();
  const ImageBufferRange<ImageType>::const_iterator cend = range.cend();

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


TEST(ImageBufferRange, BeginAndEndDoNotCompareEqual)
{
  using ImageType = itk::Image<int>;

  const auto image = CreateImage<ImageType>(2, 3);

  ImageBufferRange<ImageType> range{ *image };

  EXPECT_FALSE(range.begin() == range.end());
  EXPECT_NE(range.begin(), range.end());
}


// Tests that an iterator converts (implicitly) to a const_iterator.
TEST(ImageBufferRange, IteratorConvertsToConstIterator)
{
  using ImageType = itk::Image<int>;

  const auto image = CreateImage<ImageType>(2, 3);

  ImageBufferRange<ImageType> range{ *image };

  const ImageBufferRange<ImageType>::iterator       begin = range.begin();
  const ImageBufferRange<ImageType>::const_iterator const_begin_from_begin = begin;
  EXPECT_EQ(const_begin_from_begin, begin);

  const ImageBufferRange<ImageType>::const_iterator const_begin_from_range_begin = range.begin();
  EXPECT_EQ(const_begin_from_range_begin, range.begin());
}


// Tests that the iterators of an ImageBufferRange can be used as first and
// second argument of an std::vector constructor.
TEST(ImageBufferRange, IteratorsCanBePassedToStdVectorConstructor)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  ImageBufferRange<ImageType> range{ *image };

  // Easily store all pixels of the ImageBufferRange in an std::vector:
  const std::vector<PixelType> stdVector(range.begin(), range.end());
  EXPECT_EQ(stdVector, std::vector<PixelType>(range.cbegin(), range.cend()));
  EXPECT_TRUE(std::equal(stdVector.begin(), stdVector.end(), range.cbegin()));
}


// Tests that the iterators can be used as first and
// second argument of std::reverse (which requires bidirectional iterators).
TEST(ImageBufferRange, IteratorsCanBePassedToStdReverseCopy)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  ImageBufferRange<ImageType> range{ *image };

  const unsigned int numberOfPixels = sizeX * sizeY;

  const std::vector<PixelType> stdVector(range.begin(), range.end());
  std::vector<PixelType>       reversedStdVector1(numberOfPixels);
  std::vector<PixelType>       reversedStdVector2(numberOfPixels);
  std::vector<PixelType>       reversedStdVector3(numberOfPixels);

  // Checks bidirectionality of the ImageBufferRange iterators!
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
TEST(ImageBufferRange, IteratorsCanBePassedToStdInnerProduct)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 2,
    sizeY = 2
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  ImageBufferRange<ImageType> range{ *image };

  const double innerProduct = std::inner_product(range.begin(), range.end(), range.begin(), 0.0);

  EXPECT_EQ(innerProduct, 30);
}


// Tests that the iterators can be used as first and
// second argument of std::for_each.
TEST(ImageBufferRange, IteratorsCanBePassedToStdForEach)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  ImageBufferRange<ImageType> range{ *image };

  std::for_each(range.begin(), range.end(), [](const PixelType pixel) { EXPECT_TRUE(pixel > 0); });
}


// Tests that an ImageBufferRange can be used as the "range expression" of a
// C++11 range-based for loop.
TEST(ImageBufferRange, CanBeUsedAsExpressionOfRangeBasedForLoop)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  using RangeType = ImageBufferRange<ImageType>;

  enum
  {
    sizeX = 2,
    sizeY = 3
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  RangeType range{ *image };

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


// Tests that the distance between two iterators, it1 and it2, can be obtained by
// subtraction (it2 - it1).
TEST(ImageBufferRange, DistanceBetweenIteratorsCanBeObtainedBySubtraction)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImage<ImageType>(sizeX, sizeY);

  ImageBufferRange<ImageType> range{ *image };

  ImageBufferRange<ImageType>::iterator it1 = range.begin();

  const size_t numberOfPixels = range.size();

  for (size_t i1 = 0; i1 < numberOfPixels; ++i1, ++it1)
  {
    ImageBufferRange<ImageType>::iterator it2 = it1;

    for (size_t i2 = 0; i2 < numberOfPixels; ++i2, ++it2)
    {
      EXPECT_EQ(it2 - it1, std::distance(it1, it2));
    }
  }
}


// Tests that iterator::reference and const_iterator::reference act like a real
// (built-in) C++ reference to the pixel type.
TEST(ImageBufferRange, IteratorReferenceActsLikeARealReference)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);
  using RangeType = ImageBufferRange<ImageType>;

  RangeType           range{ *image };
  RangeType::iterator it = range.begin();

  std::iterator_traits<RangeType::iterator>::reference       reference1 = *it;
  std::iterator_traits<RangeType::iterator>::reference       reference2 = *(++it);
  std::iterator_traits<RangeType::const_iterator>::reference reference3 = *(++it);
  EXPECT_EQ(reference1, 1);
  EXPECT_EQ(reference2, 2);
  EXPECT_EQ(reference3, 3);

  std::iterator_traits<RangeType::const_iterator>::reference reference4 = reference1;
  std::iterator_traits<RangeType::const_iterator>::reference reference5 = reference2;
  std::iterator_traits<RangeType::const_iterator>::reference reference6 = reference3;
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


// Tests that ImageBufferRange<VectorImage<T>> is supported well.
TEST(ImageBufferRange, SupportsVectorImage)
{
  using ImageType = itk::VectorImage<unsigned char>;
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

  using RangeType = ImageBufferRange<ImageType>;
  RangeType range{ *image };

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


TEST(ImageBufferRange, IteratorsCanBePassedToStdSort)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 3,
    sizeY = 3
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  ImageBufferRange<ImageType> range{ *image };

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


TEST(ImageBufferRange, IteratorsCanBePassedToStdNthElement)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  enum
  {
    sizeX = 3,
    sizeY = 3
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  ImageBufferRange<ImageType> range{ *image };

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


TEST(ImageBufferRange, IteratorIsDefaultConstructible)
{
  RangeGTestUtilities::ExpectIteratorIsDefaultConstructible<ImageBufferRange<itk::Image<int>>>();
  RangeGTestUtilities::ExpectIteratorIsDefaultConstructible<ImageBufferRange<itk::VectorImage<int>>>();
}


TEST(ImageBufferRange, IteratorsSupportRandomAccess)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  using RangeType = ImageBufferRange<ImageType>;
  enum
  {
    sizeX = 3,
    sizeY = 3
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  RangeType range{ *image };

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

  using difference_type = std::iterator_traits<X>::difference_type;
  using reference = std::iterator_traits<X>::reference;

  {
    // Expression to be tested: 'r += n'
    difference_type n = 3;

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
    auto && actualResult = r += n;
    EXPECT_EQ(actualResult, expectedResult);
    static_assert(std::is_same<decltype(actualResult), X &>::value, "Type of result 'r += n' tested");
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

    r = initialIterator;
    const auto expectedResult = [&r, n] {
      // Operational semantics, as specified by the C++11 Standard:
      return r += -n;
    }();
    r = initialIterator;
    auto && actualResult = r -= n;
    EXPECT_EQ(actualResult, expectedResult);
    static_assert(std::is_same<decltype(actualResult), X &>::value, "Type of result 'r -= n' tested");
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


TEST(ImageBufferRange, SupportsSubscript)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  using RangeType = ImageBufferRange<ImageType>;

  enum
  {
    sizeX = 3,
    sizeY = 3
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  RangeType range{ *image };

  const size_t numberOfNeighbors = range.size();

  RangeType::iterator it = range.begin();

  for (size_t i = 0; i < numberOfNeighbors; ++i)
  {
    std::iterator_traits<RangeType::iterator>::reference neighbor = range[i];
    EXPECT_EQ(neighbor, *it);
    ++it;
  }
}


TEST(ImageBufferRange, ProvidesReverseIterators)
{
  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType>;
  using RangeType = ImageBufferRange<ImageType>;
  enum
  {
    sizeX = 9,
    sizeY = 11
  };
  const auto image = CreateImageFilledWithSequenceOfNaturalNumbers<ImageType>(sizeX, sizeY);

  RangeType range{ *image };

  const unsigned int numberOfPixels = sizeX * sizeY;

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
TEST(ImageBufferRange, BeginIsEndWhenDefaultConstructed)
{
  RangeGTestUtilities::ExpectBeginIsEndWhenRangeIsDefaultConstructed<ImageBufferRange<itk::Image<int>>>();
  RangeGTestUtilities::ExpectBeginIsEndWhenRangeIsDefaultConstructed<ImageBufferRange<itk::VectorImage<int>>>();
}


// Tests that size() returns 0 for a default-constructed range.
TEST(ImageBufferRange, SizeIsZeroWhenDefaultConstructed)
{
  RangeGTestUtilities::ExpectZeroSizeWhenRangeIsDefaultConstructed<ImageBufferRange<itk::Image<int>>>();
  RangeGTestUtilities::ExpectZeroSizeWhenRangeIsDefaultConstructed<ImageBufferRange<itk::VectorImage<int>>>();
}


// Tests empty() for a default-constructed range.
TEST(ImageBufferRange, IsEmptyWhenDefaultConstructed)
{
  RangeGTestUtilities::ExpectRangeIsEmptyWhenDefaultConstructed<ImageBufferRange<itk::Image<int>>>();
  RangeGTestUtilities::ExpectRangeIsEmptyWhenDefaultConstructed<ImageBufferRange<itk::VectorImage<int>>>();
}


// Tests that range.empty() returns false for a non-empty image.
TEST(ImageBufferRange, IsNotEmptyWhenImageIsNonEmpty)
{
  ExpectRangeIsNotEmptyForNonEmptyImage<itk::Image<int>>();
  ExpectRangeIsNotEmptyForNonEmptyImage<itk::VectorImage<int>>();
}


// Tests that MakeImageBufferRange(imagePtr) returns an empty range when the argument (imagePtr) is a nullptr.
TEST(ImageBufferRange, MakeImageBufferRangeReturnsEmptyRangeForNullptr)
{
  ExpectMakeImageBufferRangeReturnsEmptyRangeForNullptr<itk::Image<int>>();
  ExpectMakeImageBufferRangeReturnsEmptyRangeForNullptr<itk::VectorImage<int>>();
}


// Tests that MakeImageBufferRange(image) returns the correct ImageBufferRange for a non-empty image:
// For a non-empty image, MakeImageBufferRange(&image) should be equivalent to ImageBufferRange{image}.
TEST(ImageBufferRange, MakeImageBufferRangeReturnsCorrectRangeForNonEmptyImage)
{
  ExpectMakeImageBufferRangeReturnsCorrectRangeForNonEmptyImage<itk::Image<int>>();
  ExpectMakeImageBufferRangeReturnsCorrectRangeForNonEmptyImage<itk::VectorImage<int>>();
}


// Tests that a copy-constructed range has the same iterators (begin and end) as the original.
TEST(ImageBufferRange, CopyConstructedRangeHasSameIterators)
{
  ExpectCopyConstructedRangeHasSameIteratorsAsOriginal<itk::Image<int>>();
  ExpectCopyConstructedRangeHasSameIteratorsAsOriginal<itk::VectorImage<int>>();
}


// Tests that a copy-assigned range has the same iterators (begin and end) as the original.
TEST(ImageBufferRange, CopyAssignedRangeHasSameIterators)
{
  ExpectCopyAssignedRangeHasSameIteratorsAsOriginal<itk::Image<int>>();
  ExpectCopyAssignedRangeHasSameIteratorsAsOriginal<itk::VectorImage<int>>();
}


// Tests that a move-constructed range has the same iterators as the original, before the move.
TEST(ImageBufferRange, MoveConstructedRangeHasSameIterators)
{
  ExpectMoveConstructedRangeHasSameIteratorsAsOriginalBeforeMove<itk::Image<int>>();
  ExpectMoveConstructedRangeHasSameIteratorsAsOriginalBeforeMove<itk::VectorImage<int>>();
}


// Tests that a move-assigned range has the same iterators as the original, before the move.
TEST(ImageBufferRange, MoveAssignedRangeHasSameIterators)
{
  ExpectMoveAssignedRangeHasSameIteratorsAsOriginalBeforeMove<itk::Image<int>>();
  ExpectMoveAssignedRangeHasSameIteratorsAsOriginalBeforeMove<itk::VectorImage<int>>();
}
