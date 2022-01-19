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
#include "itkImageRegion.h"
#include "itkIndexRange.h"
#include <gtest/gtest.h>
#include <type_traits> // For remove_const_t and remove_reference_t.


// Tests when zero-sized regions are considered to be inside of a region.
TEST(ImageRegion, ZeroSizedRegionIsInside)
{
  const auto check = [](const auto & region) {
    using RegionType = std::remove_const_t<std::remove_reference_t<decltype(region)>>;
    using SizeType = typename RegionType::SizeType;

    auto paddedRegion = region;
    paddedRegion.PadByRadius(2);

    // Extend the specified region (in 2D, by an extra column).
    const RegionType extendedRegion(region.GetIndex(), region.GetSize() + SizeType::Filled(1));

    for (const auto & index : itk::ImageRegionIndexRange<RegionType::ImageDimension>(paddedRegion))
    {
      const RegionType zeroSizedRegion(index, SizeType{ { 0 } });

      // The zero-sized region is considered inside this region if and only if its index is inside the extended region.
      EXPECT_EQ(region.IsInside(zeroSizedRegion), extendedRegion.IsInside(index));
    }
  };

  // Check for a 2D and a 3D image region.
  check(itk::ImageRegion<2>(itk::Size<2>::Filled(3)));
  check(itk::ImageRegion<3>(itk::MakeIndex(-1, 0, 1), itk::MakeSize(2, 3, 4)));
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
