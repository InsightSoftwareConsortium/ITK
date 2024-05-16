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

// Enable testing legacy member function GetAxisAlignedBoundingBoxRegion()
#ifndef ITK_LEGACY_REMOVE
#  define ITK_LEGACY_TEST
#endif

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


template <typename TPixel, unsigned int VImageDimension>
void
Expect_AxisAlignedBoundingBoxRegion_is_empty_when_all_pixel_values_are_zero(
  const itk::ImageRegion<VImageDimension> & imageRegion)
{
  const auto image = itk::Image<TPixel, VImageDimension>::New();

  image->SetRegions(imageRegion);
  image->AllocateInitialized();

  EXPECT_EQ(ComputeAxisAlignedBoundingBoxRegionInImageGridSpace(*image).GetSize(), itk::Size<VImageDimension>{});
}


template <typename TPixel, unsigned int VImageDimension>
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


template <typename TPixel, unsigned int VImageDimension>
void
Expect_AxisAlignedBoundingBoxRegion_equals_region_of_single_pixel_when_it_is_the_only_non_zero_pixel(
  const itk::ImageRegion<VImageDimension> & imageRegion)
{
  const auto image = itk::Image<TPixel, VImageDimension>::New();

  image->SetRegions(imageRegion);
  image->AllocateInitialized();

  const itk::ImageRegionIndexRange<VImageDimension> indexRange{ imageRegion };

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


template <typename TPixel, unsigned int VImageDimension>
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

  const itk::ImageBufferRange imageRange{ *image };

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
// (This condition should hold for n-dimensional image regions, with N >= 2, and region size >= 2^N.)
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

  EXPECT_FALSE(spatialObject->IsInside(itk::MakeFilled<PointType>(indexValue)));
  EXPECT_FALSE(spatialObject->IsInside(itk::MakeFilled<PointType>(indexValue - 0.4999)));
  EXPECT_FALSE(spatialObject->IsInside(itk::MakeFilled<PointType>(indexValue + 0.4999)));
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
  image->AllocateInitialized();

  constexpr itk::IndexValueType indexValue{ 4 };
  image->SetPixel({ { indexValue, indexValue } }, 1);

  const auto spatialObject = itk::ImageMaskSpatialObject<ImageDimension>::New();
  spatialObject->SetImage(image);
  spatialObject->Update();

  EXPECT_TRUE(spatialObject->IsInside(itk::MakeFilled<PointType>(indexValue)));
  EXPECT_TRUE(spatialObject->IsInside(itk::MakeFilled<PointType>(indexValue - 0.4999)));
  EXPECT_TRUE(spatialObject->IsInside(itk::MakeFilled<PointType>(indexValue + 0.4999)));
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
  image->AllocateInitialized();

  // Set the value of a pixel to non-zero.
  constexpr itk::IndexValueType indexValue{ 8 };
  image->SetPixel({ { indexValue, indexValue } }, 1);

  const auto spatialObject = itk::ImageMaskSpatialObject<ImageDimension>::New();
  spatialObject->SetImage(image);
  spatialObject->Update();

  // Point of interest: a point close to the non-zero pixel.
  const auto pointOfInterest = itk::MakeFilled<PointType>(indexValue - 0.25);

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


// Tests that IsInsideInObjectSpace returns false for a corner point, when the
// mask image is filled with zero values. This test would sometimes fail on
// ITK v5.0.1 and v5.1.0
TEST(ImageMaskSpatialObject, CornerPointIsNotInsideMaskOfZeroValues)
{
  // Create a mask image, and fill the image with zero vales.
  const auto image = itk::Image<unsigned char>::New();
  image->SetRegions(itk::Size<>{ { 2, 2 } });
  image->AllocateInitialized();

  const auto imageMaskSpatialObject = itk::ImageMaskSpatialObject<2>::New();
  imageMaskSpatialObject->SetImage(image);
  const double cornerPoint[] = { 1.5, 1.5 };
  ASSERT_FALSE(imageMaskSpatialObject->IsInsideInObjectSpace(cornerPoint));
}

// Check that the IsInsideInWorldSpace overloads yield the same result, when depth = 0 and name = "".
TEST(ImageMaskSpatialObject, IsInsideInWorldSpaceOverloads)
{
  constexpr auto imageDimension = 2U;
  using ImageMaskSpatialObjectType = itk::ImageMaskSpatialObject<imageDimension>;
  using MaskImageType = ImageMaskSpatialObjectType::ImageType;
  using MaskPixelType = MaskImageType::PixelType;
  using PointType = MaskImageType::PointType;

  // Create a mask image.
  const auto maskImage = MaskImageType::New();
  maskImage->SetRegions(itk::Size<imageDimension>::Filled(2));
  maskImage->AllocateInitialized();
  maskImage->SetPixel({}, MaskPixelType{ 1 });
  maskImage->SetSpacing(itk::MakeFilled<MaskImageType::SpacingType>(0.5));

  const auto imageMaskSpatialObject = ImageMaskSpatialObjectType::New();
  imageMaskSpatialObject->SetImage(maskImage);

  for (const double pointValue : { -1.0, 0.0, 0.5, 1.0 })
  {
    const PointType point(pointValue);

    EXPECT_EQ(imageMaskSpatialObject->IsInsideInWorldSpace(point),
              imageMaskSpatialObject->IsInsideInWorldSpace(point, 0, ""));
  }
}


// Check that regions of the mask image are stored in the spatial object.
TEST(ImageMaskSpatialObject, StoresRegionsFromMaskImage)
{
  using ImageMaskSpatialObjectType = itk::ImageMaskSpatialObject<>;
  using MaskImageType = ImageMaskSpatialObjectType::ImageType;

  // Test image regions of various indices and sizes:
  for (const itk::IndexValueType indexValue : { -1, 0, 1 })
  {
    // Just test some small sizes, to make the test run fast:
    for (itk::SizeValueType sizeValue{ 2 }; sizeValue < 4; ++sizeValue)
    {
      using RegionType = MaskImageType::RegionType;
      using IndexType = MaskImageType::IndexType;
      using SizeType = MaskImageType::SizeType;

      // Create a mask image.
      const auto maskImage = MaskImageType::New();
      maskImage->SetRegions(RegionType{ IndexType::Filled(indexValue), SizeType::Filled(sizeValue) });
      maskImage->AllocateInitialized();

      const auto imageMaskSpatialObject = ImageMaskSpatialObjectType::New();
      imageMaskSpatialObject->SetImage(maskImage);

      EXPECT_EQ(imageMaskSpatialObject->GetLargestPossibleRegion(), maskImage->GetLargestPossibleRegion());
      EXPECT_EQ(imageMaskSpatialObject->GetBufferedRegion(), maskImage->GetBufferedRegion());
      EXPECT_EQ(imageMaskSpatialObject->GetRequestedRegion(), maskImage->GetRequestedRegion());

      // Modify one of the image regions.
      maskImage->SetRequestedRegion(RegionType{ IndexType::Filled(indexValue + 1), SizeType::Filled(sizeValue - 1) });

      // Note: when an image region is modified _after_ calling SetImage, the user must call Update() to keep the
      // regions of the spatial object up-to-date.
      imageMaskSpatialObject->Update();

      // Do the same checks after the Update():
      EXPECT_EQ(imageMaskSpatialObject->GetLargestPossibleRegion(), maskImage->GetLargestPossibleRegion());
      EXPECT_EQ(imageMaskSpatialObject->GetBufferedRegion(), maskImage->GetBufferedRegion());
      EXPECT_EQ(imageMaskSpatialObject->GetRequestedRegion(), maskImage->GetRequestedRegion());
    }
  }
}
