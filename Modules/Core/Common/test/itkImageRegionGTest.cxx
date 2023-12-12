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
#include "itkImageRegion.h"
#include "itkIndexRange.h"
#include <gtest/gtest.h>
#include <limits>
#include <type_traits> // For remove_const_t and remove_reference_t.


namespace
{
template <unsigned int VDimension>
constexpr bool
CheckTrivialCopyabilityOfImageRegion()
{
  constexpr bool isImageRegionTriviallyCopyable{ std::is_trivially_copyable_v<itk::ImageRegion<VDimension>> };

#ifdef ITK_FUTURE_LEGACY_REMOVE
  static_assert(isImageRegionTriviallyCopyable, "In the future, ImageRegion<VDimension> should be trivially copyable.");
  return isImageRegionTriviallyCopyable;
#else
  static_assert(!isImageRegionTriviallyCopyable, "ImageRegion<VDimension> should *not* be trivially copyable.");
  return !isImageRegionTriviallyCopyable;
#endif
}
} // namespace

static_assert(CheckTrivialCopyabilityOfImageRegion<2>() && CheckTrivialCopyabilityOfImageRegion<3>(),
              "ImageRegion<VDimension> should be trivially copyable when legacy support is removed.");


// Tests that a zero-sized region is not considered to be inside of another region.
TEST(ImageRegion, ZeroSizedRegionIsNotInside)
{
  using RegionType = itk::ImageRegion<2>;
  using IndexType = RegionType::IndexType;
  using SizeType = RegionType::SizeType;

  const RegionType region(SizeType::Filled(2));

  for (const auto indexValue : { -1, 0, 1 })
  {
    const RegionType zeroSizedRegion{ IndexType::Filled(indexValue), SizeType{ { 0 } } };

    EXPECT_FALSE(region.IsInside(zeroSizedRegion));
  }
}


// Tests that regions of size 1 are considered to be inside of a region, if and only if ("iff") their index is inside of
// this region.
TEST(ImageRegion, OneSizedRegionIsInsideIffItsIndexIsInside)
{
  const auto check = [](const auto & region) {
    using RegionType = std::remove_const_t<std::remove_reference_t<decltype(region)>>;
    using SizeType = typename RegionType::SizeType;

    auto paddedRegion = region;
    paddedRegion.PadByRadius(1);

    for (const auto & index : itk::ImageRegionIndexRange<RegionType::ImageDimension>(paddedRegion))
    {
      const RegionType oneSizedRegion{ index, SizeType::Filled(1) };

      // The one-sized region is inside this region if and only if its index is inside this region.
      EXPECT_EQ(region.IsInside(oneSizedRegion), region.IsInside(index));
    }
  };

  // Check for a 2D and a 3D image region.
  check(itk::ImageRegion<2>(itk::Size<2>::Filled(3)));
  check(itk::ImageRegion<3>(itk::MakeIndex(-1, 0, 1), itk::MakeSize(2, 3, 4)));
}


// Tests that Crop returns false and does not change anything, when it cannot crop. That is when one of the regions is
// zero-sized, or when the region to be cropped and the crop region don't overlap.
TEST(ImageRegion, CropReturnsFalseWithoutChangingAnythingWhenItCannotCrop)
{
  const auto check = [](const auto & originalRegion, const auto & cropRegion) {
    auto regionToBeCropped = originalRegion;

    EXPECT_FALSE(regionToBeCropped.Crop(cropRegion));
    EXPECT_EQ(regionToBeCropped, originalRegion);
  };

  using RegionType = itk::ImageRegion<2>;
  using IndexType = RegionType::IndexType;
  using SizeType = RegionType::SizeType;

  for (const auto indexValue : { std::numeric_limits<itk::IndexValueType>::min(),
                                 itk::IndexValueType{ -1 },
                                 itk::IndexValueType{},
                                 itk::IndexValueType{ 1 } })
  {
    const RegionType zeroSizedRegion{ IndexType::Filled(indexValue), SizeType{} };
    const RegionType nonZeroSizedRegion{ IndexType::Filled(indexValue), SizeType::Filled(1) };

    // Cannot crop when the region to be cropped and/or the crop region is zero-sized.
    check(zeroSizedRegion, zeroSizedRegion);
    check(zeroSizedRegion, nonZeroSizedRegion);
    check(nonZeroSizedRegion, zeroSizedRegion);
  }

  for (const auto indexValue : { std::numeric_limits<itk::IndexValueType>::min(),
                                 itk::IndexValueType{ -1 },
                                 itk::IndexValueType{},
                                 itk::IndexValueType{ 1 } })
  {
    for (const itk::SizeValueType sizeValue : { 1, 2 })
    {
      const RegionType region1{ IndexType::Filled(indexValue), SizeType::Filled(sizeValue) };
      const RegionType region2{ IndexType::Filled(indexValue + static_cast<itk::IndexValueType>(sizeValue)),
                                SizeType::Filled(sizeValue) };

      // Cannot crop when the region to be cropped and the crop region do not overlap.
      check(region1, region2);
      check(region2, region1);
    }
  }
}


// Tests that Crop returns true, and does not change the region to be cropped, when it is equal to the crop region
// (assuming that both regions are not zero-sized).
TEST(ImageRegion, CropDoesNotChangeRegionToBeCroppedWhenCropRegionIsEqual)
{
  using RegionType = itk::ImageRegion<2>;
  using IndexType = RegionType::IndexType;
  using SizeType = RegionType::SizeType;

  for (const auto indexValue : { std::numeric_limits<itk::IndexValueType>::min(),
                                 itk::IndexValueType{ -1 },
                                 itk::IndexValueType{},
                                 itk::IndexValueType{ 1 } })
  {
    for (const itk::SizeValueType sizeValue : { 1, 2 })
    {
      const RegionType region{ IndexType::Filled(indexValue), SizeType::Filled(sizeValue) };
      auto             regionToBeCropped = region;
      EXPECT_TRUE(regionToBeCropped.Crop(region));
      EXPECT_EQ(regionToBeCropped, region);
    }
  }
}


// Tests cropping a larger to a smaller region, and vice versa.
TEST(ImageRegion, CropLargerToSmallerRegionAndViceVersa)
{
  using RegionType = itk::ImageRegion<2>;
  using IndexType = RegionType::IndexType;
  using SizeType = RegionType::SizeType;

  for (const auto indexValue : { std::numeric_limits<itk::IndexValueType>::min(),
                                 itk::IndexValueType{ -1 },
                                 itk::IndexValueType{},
                                 itk::IndexValueType{ 1 } })
  {
    for (const itk::SizeValueType sizeValue : { 1, 2 })
    {
      const RegionType smallerRegion{ IndexType::Filled(indexValue), SizeType::Filled(sizeValue) };
      const RegionType largerRegion{ smallerRegion.GetIndex(), SizeType::Filled(sizeValue + 1) };

      {
        auto regionToBeCropped = largerRegion;

        EXPECT_TRUE(regionToBeCropped.Crop(smallerRegion));
        EXPECT_EQ(regionToBeCropped, smallerRegion);
      }
      {
        auto regionToBeCropped = smallerRegion;

        EXPECT_TRUE(regionToBeCropped.Crop(largerRegion));
        EXPECT_EQ(regionToBeCropped, smallerRegion);
      }
    }
  }
}


// Tests C++ structured binding of an ImageRegion.
TEST(ImageRegion, SupportsStructuredBinding)
{
  using RegionType = itk::ImageRegion<2>;
  using IndexType = RegionType::IndexType;
  using SizeType = RegionType::SizeType;

  RegionType region{};
  auto && [index, size] = region;

  static_assert(std::is_same_v<decltype(index), IndexType>);
  static_assert(std::is_same_v<decltype(size), SizeType>);
  EXPECT_EQ(&index, &(region.GetIndex()));
  EXPECT_EQ(&size, &(region.GetSize()));

  const RegionType constRegion{};
  auto && [constIndex, constSize] = constRegion;

  static_assert(std::is_same_v<decltype(constIndex), const IndexType>);
  static_assert(std::is_same_v<decltype(constSize), const SizeType>);
  EXPECT_EQ(&constIndex, &(constRegion.GetIndex()));
  EXPECT_EQ(&constSize, &(constRegion.GetSize()));
}
