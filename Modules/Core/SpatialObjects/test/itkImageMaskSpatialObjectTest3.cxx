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
// Disable warning for long symbol names in this file only

/*
 * This is a test file for the itkImageMaskSpatialObject class.
 * The supported pixel types does not include itkRGBPixel, itkRGBAPixel, etc...
 * So far it only allows to manage images of simple types like unsigned short,
 * unsigned int, or itk::Vector<...>.
 */

/*
 * This test addresses bug
 * https://public.kitware.com/Bug/view.php?id=11972
 *
 */

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageMaskSpatialObject.h"

int
itkImageMaskSpatialObjectTest3(int, char *[])
{
  constexpr unsigned int VDimension = 3;

  using ImageMaskSpatialObjectType = itk::ImageMaskSpatialObject<VDimension>;
  using PixelType = ImageMaskSpatialObjectType::PixelType;
  using ImageType = itk::Image<PixelType, VDimension>;

  auto                 image = ImageType::New();
  ImageType::SizeType  size = { { 5, 5, 5 } };
  ImageType::PointType origin;
  origin.Fill(0);
  image->SetOrigin(origin);

  ImageType::SpacingType spacing;
  spacing.Fill(1);
  image->SetSpacing(spacing);

  ImageType::IndexType index;
  index.Fill(0);

  ImageType::DirectionType direction;
  direction.Fill(0.0);
  direction[0][1] = 1;
  direction[1][0] = 1;
  direction[2][2] = 1;
  image->SetDirection(direction);

  ImageType::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);
  image->SetRegions(region);
  image->Allocate(true); // initialize buffer to zero

  auto imageMaskSpatialObject = ImageMaskSpatialObjectType::New();
  imageMaskSpatialObject->SetImage(image);
  imageMaskSpatialObject->Update();

  ImageMaskSpatialObjectType::PointType bndMin = imageMaskSpatialObject->GetMyBoundingBoxInWorldSpace()->GetMinimum();
  ImageMaskSpatialObjectType::IndexType bndMinI = image->TransformPhysicalPointToIndex(bndMin);

  ImageMaskSpatialObjectType::PointType bndMax = imageMaskSpatialObject->GetMyBoundingBoxInWorldSpace()->GetMaximum();
  ImageMaskSpatialObjectType::IndexType bndMaxI = image->TransformPhysicalPointToIndex(bndMax);

  ImageMaskSpatialObjectType::RegionType::SizeType regionSize;
  ImageMaskSpatialObjectType::IndexType::IndexType regionIndex;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    regionIndex[i] = bndMinI[i];
    regionSize[i] = bndMaxI[i] - bndMinI[i];
  }

  for (unsigned int i = 0; i < 3; ++i)
  {
    if (regionSize[i] != 0)
    {
      std::cout << "Invalid Region Size " << regionSize << std::endl;
      return EXIT_FAILURE;
    }
    if (regionIndex[i] != 0)
    {
      std::cout << "Invalid Region Index " << regionIndex << std::endl;
      return EXIT_FAILURE;
    }
  }

  using ImageRegionIteratorType = itk::ImageRegionIteratorWithIndex<ImageType>;
  ImageRegionIteratorType it(image, image->GetLargestPossibleRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    ImageType::PointType point;
    image->TransformIndexToPhysicalPoint(it.GetIndex(), point);
    if (imageMaskSpatialObject->IsInsideInWorldSpace(point))
    {
      std::cout << "Pixel Reported Inside mask, even though mask image is all zeros" << std::endl;
      return EXIT_FAILURE;
    }
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
