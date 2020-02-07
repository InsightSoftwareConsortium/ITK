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

// Enable testing legacy member function GetAxisAlignedBoundingBoxRegion()
#define ITK_LEGACY_TEST

// First include the header file to be tested:
#include "itkImageMaskSpatialObject.h"

#include "itkImageBufferRange.h"
#include "itkIndexRange.h"
#include "itkNumericTraits.h"

#include <gtest/gtest.h>

#if !defined(ITK_LEGACY_REMOVE)

namespace
{
// Convenience function that calls GetAxisAlignedBoundingBoxRegion() for the specified image.
template <typename TImage>
typename TImage::RegionType
ComputeAxisAlignedBoundingBoxRegionInImageGridSpace(const TImage & image)
{
  using PixelType = typename TImage::PixelType;
  const auto spatialObject = itk::ImageMaskSpatialObject<TImage::ImageDimension, PixelType>::New();
  spatialObject->SetImage(&image);
  return spatialObject->GetAxisAlignedBoundingBoxRegion();
}


template <typename TPixel, unsigned VImageDimension>
void
Expect_AxisAlignedBoundingBoxRegion_is_empty_when_all_pixel_values_are_zero(
  const itk::ImageRegion<VImageDimension> & imageRegion)
{
  const auto image = itk::Image<TPixel, VImageDimension>::New();

  image->SetRegions(imageRegion);
  image->Allocate(true);

  EXPECT_EQ(ComputeAxisAlignedBoundingBoxRegionInImageGridSpace(*image).GetSize(), itk::Size<VImageDimension>{});
}


template <typename TPixel, unsigned VImageDimension>
void
Expect_AxisAlignedBoundingBoxRegion_equals_image_region_when_all_pixel_values_are_non_zero(
  const itk::ImageRegion<VImageDimension> & imageRegion)
{
  const auto image = itk::Image<TPixel, VImageDimension>::New();
  image->SetRegions(imageRegion);
  image->Allocate();

  // Set all pixels to non-zero (1).
  image->FillBuffer(1);

  EXPECT_EQ(ComputeAxisAlignedBoundingBoxRegionInImageGridSpace(*image), imageRegion);

  // Same test, but now using max() as non-zero value.
  image->FillBuffer(itk::NumericTraits<TPixel>::max());

  EXPECT_EQ(ComputeAxisAlignedBoundingBoxRegionInImageGridSpace(*image), imageRegion);
}


template <typename TPixel, unsigned VImageDimension>
void
Expect_AxisAlignedBoundingBoxRegion_equals_region_of_single_pixel_when_it_is_the_only_non_zero_pixel(
  const itk::ImageRegion<VImageDimension> & imageRegion)
{
  const auto image = itk::Image<TPixel, VImageDimension>::New();

  image->SetRegions(imageRegion);

  // Initialize all pixels to zero.
  image->Allocate(true);

  const itk::Experimental::ImageRegionIndexRange<VImageDimension> indexRange{ imageRegion };

  // Expected size: the "region size" of a single pixel (1x1, in 2D, 1x1x1 in 3D).
  const itk::Size<VImageDimension> expectedSize = [] {
    itk::Size<VImageDimension> size;
    size.Fill(1);
    return size;
  }();

  for (const auto & index : indexRange)
  {
    // Set only one pixel value non-zero.
    image->SetPixel(index, 1);
    const itk::ImageRegion<VImageDimension> expectedRegion{ index, expectedSize };
    EXPECT_EQ(ComputeAxisAlignedBoundingBoxRegionInImageGridSpace(*image), expectedRegion);
    // Restore pixel value to zero for the next iteration.
    image->SetPixel(index, 0);
  }
}


template <typename TPixel, unsigned VImageDimension>
void
Expect_AxisAlignedBoundingBoxRegion_equals_image_region_when_only_a_single_pixel_has_value_zero(
  const itk::ImageRegion<VImageDimension> & imageRegion)
{
  using ImageType = itk::Image<TPixel, VImageDimension>;
  const auto image = ImageType::New();
  image->SetRegions(imageRegion);
  image->Allocate();

  // Set all pixels to non-zero.
  image->FillBuffer(1);

  const itk::Experimental::ImageBufferRange<ImageType> imageRange{ *image };

  for (auto && pixel : imageRange)
  {
    // Set only one pixel value zero.
    pixel = 0;

    EXPECT_EQ(ComputeAxisAlignedBoundingBoxRegionInImageGridSpace(*image), imageRegion);

    // Restore pixel value to non-zero for the next iteration.
    pixel = 1;
  }
}

} // End of namespace


// Tests that the AABB region (as returned by GetAxisAlignedBoundingBoxRegion())
// is empty, when all pixel values are 0.
TEST(ImageMaskSpatialObject, AxisAlignedBoundingBoxIsEmptyWhenAllPixelsAreZero)
{
  // Test 2D images:
  Expect_AxisAlignedBoundingBoxRegion_is_empty_when_all_pixel_values_are_zero<double>(
    itk::ImageRegion<2>{ itk::Index<2>(), itk::Size<2>::Filled(1) });
  Expect_AxisAlignedBoundingBoxRegion_is_empty_when_all_pixel_values_are_zero<double>(
    itk::ImageRegion<2>{ itk::Index<2>{ { -1, -2 } }, itk::Size<2>{ { 3, 4 } } });

  // Test 3D images:
  Expect_AxisAlignedBoundingBoxRegion_is_empty_when_all_pixel_values_are_zero<unsigned char>(
    itk::ImageRegion<3>{ itk::Index<3>(), itk::Size<3>::Filled(1) });
  Expect_AxisAlignedBoundingBoxRegion_is_empty_when_all_pixel_values_are_zero<unsigned char>(
    itk::ImageRegion<3>{ itk::Index<3>{ { -1, -2, -3 } }, itk::Size<3>{ { 3, 4, 5 } } });
}


// Tests that the AABB region is equal to the image region, when all pixel values are non-zero.
TEST(ImageMaskSpatialObject, AxisAlignedBoundingBoxRegionIsImageRegionWhenAllPixelsAreNonZero)
{
  // Test 2D images:
  Expect_AxisAlignedBoundingBoxRegion_equals_image_region_when_all_pixel_values_are_non_zero<double>(
    itk::ImageRegion<2>{ itk::Index<2>(), itk::Size<2>::Filled(1) });
  Expect_AxisAlignedBoundingBoxRegion_equals_image_region_when_all_pixel_values_are_non_zero<double>(
    itk::ImageRegion<2>{ itk::Index<2>{ { -1, -2 } }, itk::Size<2>{ { 3, 4 } } });

  // Test 3D images:
  Expect_AxisAlignedBoundingBoxRegion_equals_image_region_when_all_pixel_values_are_non_zero<unsigned char>(
    itk::ImageRegion<3>{ itk::Index<3>(), itk::Size<3>::Filled(1) });
  Expect_AxisAlignedBoundingBoxRegion_equals_image_region_when_all_pixel_values_are_non_zero<unsigned char>(
    itk::ImageRegion<3>{ itk::Index<3>{ { -1, -2, -3 } }, itk::Size<3>{ { 3, 4, 5 } } });
}


// Tests that the AABB region is equal to the region of a single pixel, when it is the only non-zero pixel.
TEST(ImageMaskSpatialObject, AxisAlignedBoundingBoxRegionIsRegionOfSinglePixelWhenItIsOnlyNonZeroPixel)
{
  // Test 2D images:
  Expect_AxisAlignedBoundingBoxRegion_equals_region_of_single_pixel_when_it_is_the_only_non_zero_pixel<double>(
    itk::ImageRegion<2>{ itk::Index<2>(), itk::Size<2>::Filled(1) });
  Expect_AxisAlignedBoundingBoxRegion_equals_region_of_single_pixel_when_it_is_the_only_non_zero_pixel<double>(
    itk::ImageRegion<2>{ itk::Index<2>{ { -1, -2 } }, itk::Size<2>{ { 3, 4 } } });

  // Test 3D images:
  Expect_AxisAlignedBoundingBoxRegion_equals_region_of_single_pixel_when_it_is_the_only_non_zero_pixel<unsigned char>(
    itk::ImageRegion<3>{ itk::Index<3>(), itk::Size<3>::Filled(1) });
  Expect_AxisAlignedBoundingBoxRegion_equals_region_of_single_pixel_when_it_is_the_only_non_zero_pixel<unsigned char>(
    itk::ImageRegion<3>{ itk::Index<3>{ { -1, -2, -3 } }, itk::Size<3>{ { 3, 4, 5 } } });
}


// Tests that the AABB region is equal to the image region when only a single pixel has value 0.
// (This condition should hold for N-dimensional image regions, with N >= 2, and region size >= 2^N.)
TEST(ImageMaskSpatialObject, AxisAlignedBoundingBoxRegionIsImageRegionWhenOnlyOnePixelIsZero)
{
  // Test 2D images:
  Expect_AxisAlignedBoundingBoxRegion_equals_image_region_when_only_a_single_pixel_has_value_zero<double>(
    itk::ImageRegion<2>{ itk::Index<2>(), itk::Size<2>::Filled(2) });
  Expect_AxisAlignedBoundingBoxRegion_equals_image_region_when_only_a_single_pixel_has_value_zero<double>(
    itk::ImageRegion<2>{ itk::Index<2>{ { -1, -2 } }, itk::Size<2>{ { 3, 4 } } });

  // Test 3D images:
  Expect_AxisAlignedBoundingBoxRegion_equals_image_region_when_only_a_single_pixel_has_value_zero<unsigned char>(
    itk::ImageRegion<3>{ itk::Index<3>(), itk::Size<3>::Filled(2) });
  Expect_AxisAlignedBoundingBoxRegion_equals_image_region_when_only_a_single_pixel_has_value_zero<unsigned char>(
    itk::ImageRegion<3>{ itk::Index<3>{ { -1, -2, -3 } }, itk::Size<3>{ { 3, 4, 5 } } });
}


// Tests IsInside for a single zero-valued pixel (non-zero as background).
TEST(ImageMaskSpatialObject, IsInsideSingleZeroPixel)
{
  using ImageType = itk::Image<unsigned char>;
  constexpr auto ImageDimension = ImageType::ImageDimension;
  using SizeType = ImageType::SizeType;
  using PointType = ImageType::PointType;

  // Create an image filled with non-zero valued pixels.
  const auto image = ImageType::New();
  image->SetRegions(SizeType::Filled(8));
  image->Allocate();
  image->FillBuffer(1);

  constexpr itk::IndexValueType indexValue{ 4 };
  image->SetPixel({ { indexValue, indexValue } }, 0);

  const auto spatialObject = itk::ImageMaskSpatialObject<ImageDimension>::New();
  spatialObject->SetImage(image);
  spatialObject->Update();

  EXPECT_FALSE(spatialObject->IsInside(PointType{ indexValue }));
  EXPECT_FALSE(spatialObject->IsInside(PointType{ indexValue - 0.4999 }));
  EXPECT_FALSE(spatialObject->IsInside(PointType{ indexValue + 0.4999 }));
}


// Tests IsInside for a single non-zero-valued pixel (zero as background).
TEST(ImageMaskSpatialObject, IsInsideSingleNonZeroPixel)
{
  using ImageType = itk::Image<unsigned char>;
  constexpr auto ImageDimension = ImageType::ImageDimension;
  using SizeType = ImageType::SizeType;
  using PointType = ImageType::PointType;

  // Create an image filled with zero valued pixels.
  const auto image = ImageType::New();
  image->SetRegions(SizeType::Filled(8));
  image->Allocate(true);

  constexpr itk::IndexValueType indexValue{ 4 };
  image->SetPixel({ { indexValue, indexValue } }, 1);

  const auto spatialObject = itk::ImageMaskSpatialObject<ImageDimension>::New();
  spatialObject->SetImage(image);
  spatialObject->Update();

  EXPECT_TRUE(spatialObject->IsInside(PointType{ indexValue }));
  EXPECT_TRUE(spatialObject->IsInside(PointType{ indexValue - 0.4999 }));
  EXPECT_TRUE(spatialObject->IsInside(PointType{ indexValue + 0.4999 }));
}


// Tests that the result of IsInside(point) is independent of a distant pixel value.
TEST(ImageMaskSpatialObject, IsInsideIndependentOfDistantPixels)
{
  using ImageType = itk::Image<unsigned char>;
  constexpr auto ImageDimension = ImageType::ImageDimension;
  using SizeType = ImageType::SizeType;
  using IndexType = ImageType::IndexType;
  using PointType = ImageType::PointType;

  // Create an image filled with zero valued pixels.
  const auto image = ImageType::New();
  image->SetRegions(SizeType::Filled(10));
  image->Allocate(true);

  // Set the value of a pixel to non-zero.
  constexpr itk::IndexValueType indexValue{ 8 };
  image->SetPixel({ { indexValue, indexValue } }, 1);

  const auto spatialObject = itk::ImageMaskSpatialObject<ImageDimension>::New();
  spatialObject->SetImage(image);
  spatialObject->Update();

  // Point of interest: a point close to the non-zero pixel.
  const PointType pointOfInterest{ indexValue - 0.25 };

  const bool isInsideBefore = spatialObject->IsInside(pointOfInterest);

  // Now also set the value of a pixel at (0, 0) to non-zero. This is the pixel
  // farthest away from the point of interest.
  image->SetPixel(IndexType(), 1);
  spatialObject->Update();

  const bool isInsideAfter = spatialObject->IsInside(pointOfInterest);

  // Expect the same return value of IsInside(point), before and after setting
  // the value of a distant pixel.
  EXPECT_EQ(isInsideBefore, isInsideAfter);
}


#endif
