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

#include <iostream>
#include "itkReflectiveImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkGTest.h"
#include "itkTestingGetBufferPointerCountingImage.h"
#include <type_traits>

namespace
{
// HasSet tells whether `Set(pixel)` can be called on the specified iterator type.
template <typename TIterator, typename = void>
struct HasSet : std::false_type
{};

template <typename TIterator>
struct HasSet<TIterator,
              std::void_t<decltype(std::declval<TIterator &>().Set(std::declval<typename TIterator::PixelType>()))>>
  : std::true_type
{};

// HasAssignableValue tells whether a pixel can be assigned through the reference returned by `Value()`.
template <typename TIterator, typename = void>
struct HasAssignableValue : std::false_type
{};

template <typename TIterator>
struct HasAssignableValue<
  TIterator,
  std::void_t<decltype(std::declval<TIterator &>().Value() = std::declval<typename TIterator::PixelType>())>>
  : std::true_type
{};
} // namespace


TEST(ReflectiveImageRegionIterator, Test)
{
  std::cout << "Creating an image" << std::endl;
  constexpr unsigned int Dimension{ 4 };
  using PixelType = itk::Index<Dimension>;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ImageVisitsType = itk::Image<int, Dimension>;

  using IteratorType = itk::ImageRegionIteratorWithIndex<ImageType>;
  using IteratorVisitsType = itk::ImageRegionIteratorWithIndex<ImageVisitsType>;

  auto myImage = ImageType::New();

  constexpr ImageType::SizeType size{ 4, 4, 4, 4 };

  const ImageType::RegionType region{ size };

  myImage->SetRegions(region);
  myImage->Allocate();

  auto visitImage = ImageVisitsType::New();

  visitImage->SetRegions(region);
  visitImage->Allocate();

  IteratorType       nit(myImage, region);
  IteratorVisitsType vit(visitImage, region);

  // Store information on the Image
  std::cout << "Storing data in the image ... " << std::endl;
  nit.GoToBegin();
  vit.GoToBegin();
  while (!nit.IsAtEnd())
  {
    // set the pixel index as value
    nit.Set(nit.GetIndex());
    // Set the number of visits to zero
    vit.Set(ImageVisitsType::PixelType{});
    ++nit;
    ++vit;
  }


  using ReflectiveIteratorType = itk::ReflectiveImageRegionConstIterator<ImageType>;
  ReflectiveIteratorType rit(myImage, region);

  using ReflectiveVisitsIteratorType = itk::ReflectiveImageRegionIterator<ImageVisitsType>;

  ReflectiveVisitsIteratorType rvt(visitImage, region);

  // Verification
  std::cout << "Verifying the reflective iterator... " << std::endl;

  rit.GoToBegin();
  rvt.GoToBegin();
  while (!rit.IsAtEnd())
  {
    const PixelType            value = rit.Get();
    const ImageType::IndexType index = rit.GetIndex();
    rvt.Set(rvt.Get() + 1);
    EXPECT_EQ(value, index);
    ++rit;
    ++rvt;
  }


  // Each element should be visited 2 ^ # of dimensions
  // each left shift = multiply by 2
  constexpr int visits{ (1 << (ImageType::ImageDimension)) };

  // Verify the number of visits
  vit.GoToBegin();
  while (!vit.IsAtEnd())
  {
    EXPECT_EQ(vit.Get(), visits);
    ++vit;
  }

  std::cout << std::endl;

  // Test the value at a pixel using both the iterator and the image given the index pointed
  // to by the iterator.  These should obviously always be the same.
  // But this test exposes a bug in the code that has been fixed.
  ImageType::OffsetType voffset;
  for (unsigned int dim = 0; dim < Dimension; ++dim)
  {
    if (region.GetSize()[dim] > 1)
    {
      voffset[dim] = 1;
    }
    else
    {
      voffset[dim] = 0;
    }
  }
  rit.SetBeginOffset(voffset);
  rit.SetEndOffset(voffset);

  for (rit.GoToBegin(); !rit.IsAtEnd(); ++rit)
  {
    EXPECT_EQ(rit.Get(), myImage->GetPixel(rit.GetIndex()));
  }
}


// Checks that the iterator calls the non-const GetBufferPointer() overload of the image on construction.
TEST(ReflectiveImageRegionIterator, ConstructorCallsNonConstGetBufferPointer)
{
  using ImageType = itk::Testing::GetBufferPointerCountingImage<int>;
  EXPECT_TRUE(
    itk::Testing::IteratorConstructorCallsNonConstGetBufferPointer<itk::ReflectiveImageRegionIterator<ImageType>>());
}

// Checks that the read-only iterator deduces its image type from a raw image pointer.
TEST(ReflectiveImageRegionIterator, DeduceImageTypeFromRawPointer)
{
  using ImageType = itk::Image<int, 3>;
  ImageType * const       image = nullptr;
  const ImageType * const constImage = nullptr;
  static_assert(std::is_same_v<decltype(itk::ReflectiveImageRegionConstIterator(image, ImageType::RegionType{})),
                               itk::ReflectiveImageRegionConstIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ReflectiveImageRegionConstIterator(constImage, ImageType::RegionType{})),
                               itk::ReflectiveImageRegionConstIterator<ImageType>>);
}

// Checks that both iterator names deduce their image type from a smart pointer.
TEST(ReflectiveImageRegionIterator, DeduceImageTypeFromSmartPointer)
{
  using ImageType = itk::Image<int, 3>;
  const ImageType::Pointer      image{};
  const ImageType::ConstPointer constImage{};
  static_assert(std::is_same_v<decltype(itk::ReflectiveImageRegionIterator(image, ImageType::RegionType{})),
                               itk::ReflectiveImageRegionIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ReflectiveImageRegionConstIterator(image, ImageType::RegionType{})),
                               itk::ReflectiveImageRegionConstIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ReflectiveImageRegionConstIterator(constImage, ImageType::RegionType{})),
                               itk::ReflectiveImageRegionConstIterator<ImageType>>);
}


// Checks that the read-only name converts to and from the iterator over a const image, and never to a writable one.
TEST(ReflectiveImageRegionIterator, ReadOnlyIteratorDoesNotConvertToWritable)
{
  using ImageType = itk::Image<int, 2>;
  using WritableType = itk::ReflectiveImageRegionIterator<ImageType>;
  using ReadOnlyType = itk::ReflectiveImageRegionConstIterator<ImageType>;
  using ConstImageType = itk::ReflectiveImageRegionIterator<const ImageType>;
  static_assert(std::is_convertible_v<WritableType, ReadOnlyType>);
  static_assert(std::is_convertible_v<ReadOnlyType, ConstImageType>);
  static_assert(std::is_convertible_v<ConstImageType, ReadOnlyType>);
  static_assert(!std::is_constructible_v<WritableType, ReadOnlyType>);
  static_assert(!std::is_constructible_v<WritableType, itk::ReflectiveImageRegionIterator<const ImageType>>);
}


// Checks that converting a writable iterator to the read-only one keeps its position and reflection state.
TEST(ReflectiveImageRegionIterator, ConversionToConstIteratorKeepsPosition)
{
  using ImageType = itk::Image<int, 2>;
  const auto image = ImageType::New();
  const auto region = ImageType::RegionType(ImageType::SizeType::Filled(3));
  image->SetRegions(region);
  image->AllocateInitialized();

  itk::ReflectiveImageRegionIterator<ImageType> it(image, region);
  it.GoToBegin();
  for (int step = 0; step < 4; ++step)
  {
    ++it;
  }

  const auto expectSameState = [&it](const itk::ReflectiveImageRegionConstIterator<ImageType> & converted) {
    EXPECT_EQ(converted.GetIndex(), it.GetIndex());
    for (unsigned int dim = 0; dim < ImageType::ImageDimension; ++dim)
    {
      EXPECT_EQ(converted.IsReflected(dim), it.IsReflected(dim));
    }
  };
  const itk::ReflectiveImageRegionConstIterator<ImageType> copied(it);
  expectSameState(copied);

  itk::ReflectiveImageRegionConstIterator<ImageType> assigned(image, region);
  assigned = it;
  expectSameState(assigned);

  const itk::ReflectiveImageRegionIterator<const ImageType> readOnly(it);
  expectSameState(readOnly);
  itk::ReflectiveImageRegionIterator<const ImageType> readOnlyAssigned(image, region);
  readOnlyAssigned = readOnly;
  expectSameState(readOnlyAssigned);

  const auto writableCopy = it;
  expectSameState(writableCopy);
  const auto readOnlyCopy = readOnly;
  expectSameState(readOnlyCopy);
}


// Checks that only the writable iterator provides Set().
TEST(ReflectiveImageRegionIterator, OnlyWritableIteratorSets)
{
  using ImageType = itk::Image<int, 2>;
  static_assert(HasSet<itk::ReflectiveImageRegionIterator<ImageType>>::value);
  static_assert(!HasSet<itk::ReflectiveImageRegionConstIterator<ImageType>>::value);
  static_assert(!HasSet<itk::ReflectiveImageRegionIterator<const ImageType>>::value);
  static_assert(HasAssignableValue<itk::ReflectiveImageRegionIterator<ImageType>>::value);
  static_assert(!HasAssignableValue<itk::ReflectiveImageRegionConstIterator<ImageType>>::value);
  static_assert(!HasAssignableValue<itk::ReflectiveImageRegionIterator<const ImageType>>::value);
}


// Checks that both iterator names deduce their image type from an indexed base iterator.
TEST(ReflectiveImageRegionIterator, DeduceImageTypeFromBaseIterator)
{
  using ImageType = itk::Image<int, 2>;
  const itk::ImageIteratorWithIndex<ImageType>      base{};
  const itk::ImageConstIteratorWithIndex<ImageType> constBase{};
  static_assert(
    std::is_same_v<decltype(itk::ReflectiveImageRegionIterator(base)), itk::ReflectiveImageRegionIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ReflectiveImageRegionConstIterator(constBase)),
                               itk::ReflectiveImageRegionConstIterator<ImageType>>);
}


// Checks that default construction, which has no image, starts the first pass in every dimension.
TEST(ReflectiveImageRegionIterator, DefaultConstructsWithoutImage)
{
  using ImageType = itk::Image<int, 2>;
  const itk::ReflectiveImageRegionIterator<ImageType>      writable{};
  const itk::ReflectiveImageRegionConstIterator<ImageType> readOnly{};
  for (unsigned int dim = 0; dim < ImageType::ImageDimension; ++dim)
  {
    EXPECT_FALSE(writable.IsReflected(dim));
    EXPECT_FALSE(readOnly.IsReflected(dim));
  }
}


// Checks that an iterator built from an indexed iterator at the begin walks the region like one built from the image.
TEST(ReflectiveImageRegionIterator, ConstructedFromIndexedIteratorWalksBothPasses)
{
  using ImageType = itk::Image<int, 2>;
  const auto image = ImageType::New();
  const auto region = ImageType::RegionType(ImageType::SizeType::Filled(4));
  image->SetRegions(region);
  image->AllocateInitialized();

  const auto expectSameWalk = [&image, &region](auto converted) {
    itk::ReflectiveImageRegionConstIterator<ImageType> expected(image, region);
    for (; !expected.IsAtEnd(); ++expected)
    {
      ASSERT_FALSE(converted.IsAtEnd());
      EXPECT_EQ(converted.GetIndex(), expected.GetIndex());
      ++converted;
    }
    EXPECT_TRUE(converted.IsAtEnd());
  };
  expectSameWalk(
    itk::ReflectiveImageRegionIterator<ImageType>(itk::ImageRegionIteratorWithIndex<ImageType>(image, region)));
  expectSameWalk(itk::ReflectiveImageRegionConstIterator<ImageType>(
    itk::ImageRegionConstIteratorWithIndex<ImageType>(image, region)));
}
