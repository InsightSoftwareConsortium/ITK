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
 * https://public.kitware.com/Bug/view.php?id=0006340
 *
 */

#include "itkImageRegionIterator.h"
#include "itkImageMaskSpatialObject.h"
#include "itkMath.h"
#include "itkEuler3DTransform.h"

int
itkImageMaskSpatialObjectTest2(int, char *[])
{
  constexpr unsigned int VDimension = 3;
  int                    retval = EXIT_SUCCESS;

  using ImageMaskSpatialObject = itk::ImageMaskSpatialObject<VDimension>;
  using PixelType = ImageMaskSpatialObject::PixelType;
  using ImageType = itk::Image<PixelType, VDimension>;
  using Iterator = itk::ImageRegionIterator<ImageType>;

  // Direction was not taken into account in the image spatial object
  // explicitly test using images with directions set.
  // Also explicitly uses nonzero origin, non identity scales
  // to fully test the commonly encountered cases from the real world

  auto image = ImageType::New();

  // Set the direction for a non-oriented image
  // to better test the frequently encountered case
  // Use non axis aligned image directions
  itk::Euler3DTransform<double>::Pointer tfm = itk::Euler3DTransform<double>::New();
  tfm->SetRotation(30.0 * itk::Math::pi_over_180, 15.0 * itk::Math::pi_over_180, 10.0 * itk::Math::pi_over_180);
  const ImageType::DirectionType direction = tfm->GetMatrix();
  image->SetDirection(direction);

  const ImageType::SizeType size = { { 50, 50, 50 } };
  ImageType::PointType      origin;
  origin[0] = 1.51;
  origin[1] = 2.10;
  origin[2] = -300;
  image->SetOrigin(origin);

  ImageType::SpacingType spacing;
  spacing[0] = 0.5;
  spacing[1] = 0.7;
  spacing[2] = 1.1;
  image->SetSpacing(spacing);
  constexpr unsigned int     index_offset = 6543;
  const ImageType::IndexType index = { { index_offset, index_offset, index_offset } };

  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  image->SetRegions(region);
  image->Allocate(true); // initialize buffer to zero

  ImageType::RegionType  insideRegion;
  constexpr unsigned int INSIDE_SIZE = 30;
  constexpr unsigned int INSIDE_INDEX = index_offset + 10;
  {
    const ImageType::SizeType insideSize = { { INSIDE_SIZE, INSIDE_SIZE, INSIDE_SIZE } };
    insideRegion.SetSize(insideSize);
  }
  {
    const ImageType::IndexType insideIndex = { { INSIDE_INDEX, INSIDE_INDEX, INSIDE_INDEX } };
    insideRegion.SetIndex(insideIndex);
  }
  {
    Iterator it(image, insideRegion);
    it.GoToBegin();
    while (!it.IsAtEnd())
    {
      it.Set(itk::NumericTraits<PixelType>::max());
      ++it;
    }
  }

  auto maskSO = ImageMaskSpatialObject::New();
  maskSO->SetImage(image);
  maskSO->Update();

  { // Replicate use of MaskSpatialObject behavior from itk::ImageToImageMetric.hxx
    Iterator itr(image, region);
    itr.GoToBegin();
    while (!itr.IsAtEnd())
    {
      const ImageType::IndexType constIndex = itr.GetIndex();

      ImageType::PointType point;
      image->TransformIndexToPhysicalPoint(constIndex, point);
      const bool isInsideTest = maskSO->IsInsideInWorldSpace(point);

      double outsideIfZeroValue;
      maskSO->ValueAtInWorldSpace(point, outsideIfZeroValue);
      if (isInsideTest && itk::Math::AlmostEquals(outsideIfZeroValue, 0.0))
      {
        std::cerr << "ERROR: ValueAtInWorldSpace is wrong. " << outsideIfZeroValue
                  << " << computed, but should not be very close to 0.0." << std::endl;
        std::cerr << "     : Index=" << constIndex << "\n     : PhysicalPoint=" << point << "." << std::endl;
        retval = EXIT_FAILURE;
        break;
      }
      ++itr;
    }
  }

  { // Test region based is inside
    Iterator itr(image, region);
    itr.GoToBegin();
    while (!itr.IsAtEnd())
    {
      const ImageType::IndexType constIndex = itr.GetIndex();
      const bool                 reference = insideRegion.IsInside(constIndex);

      ImageType::PointType point;
      image->TransformIndexToPhysicalPoint(constIndex, point);
      const bool test = maskSO->IsInsideInWorldSpace(point);
      if (test != reference)
      {
        std::cerr << "Error in the evaluation of maskSO->IsInsideInWorldSpace() " << std::endl;
        std::cerr << "Index failed = " << constIndex << std::endl;
        std::cerr << "Point failed = " << point << std::endl;
        std::cerr << "Image is a: " << image->GetNameOfClass() << std::endl;
        std::cerr << "Direction is: " << std::endl << image->GetDirection() << std::endl;
        retval = EXIT_FAILURE;
        break;
      }
      // Should be the same as WorldSpace since there is no hierarchy.
      const bool test_object_space = maskSO->IsInsideInObjectSpace(point);
      if (test != test_object_space)
      {
        std::cerr << "IsInsideInObjectSpace !=  IsInsideInWorldSpace for object that does not have hierarchy."
                  << std::endl;
        std::cerr << "Index failed = " << constIndex << std::endl;
        std::cerr << "Point failed = " << point << std::endl;
        std::cerr << "Image is a: " << image->GetNameOfClass() << std::endl;
        std::cerr << "Direction is: " << std::endl << image->GetDirection() << std::endl;
        std::cerr << std::endl;
        retval = EXIT_FAILURE;
        break;
      }
      ++itr;
    }
  }

  if (retval == EXIT_SUCCESS)
  {
    std::cout << "Test with " << image->GetNameOfClass() << " passed." << std::endl;
  }

  // Check if insideregion is properly computed at the image boundary
  {
    ImageType::IndexType startPointIndex = { { INSIDE_SIZE - 2, INSIDE_SIZE - 2, INSIDE_SIZE - 2 } };
    ImageType::IndexType endPointIndex = {
      { INSIDE_INDEX + INSIDE_SIZE + 2, INSIDE_INDEX + INSIDE_SIZE + 2, INSIDE_INDEX + INSIDE_SIZE + 2 }
    };
    ImageType::PointType startPoint;
    ImageType::PointType endPoint;
    image->TransformIndexToPhysicalPoint(startPointIndex, startPoint);
    image->TransformIndexToPhysicalPoint(endPointIndex, endPoint);

    // Traverse along the line that goes through mask boundaries and
    // check if the value and the mask is consistent
    const auto numberOfSteps =
      static_cast<int>(std::sqrt(static_cast<double>(INSIDE_SIZE * INSIDE_SIZE + INSIDE_SIZE * INSIDE_SIZE +
                                                     INSIDE_SIZE * INSIDE_SIZE)) *
                       100.0);
    const ImageType::SpacingType incrementVector = (endPoint - startPoint) / static_cast<double>(numberOfSteps);
    ImageType::PointType         point = startPoint;
    for (int i = 0; i < numberOfSteps; ++i)
    {
      point += incrementVector;
      const bool isInside = maskSO->IsInsideInWorldSpace(point);
      double     value = itk::NumericTraits<PixelType>::ZeroValue();
      maskSO->ValueAtInWorldSpace(point, value);
      const bool isZero = (itk::Math::ExactlyEquals(value, itk::NumericTraits<PixelType>::ZeroValue()));
      if ((isInside && isZero) || (!isInside && !isZero))
      {
        ImageType::IndexType pointIndex = image->TransformPhysicalPointToIndex(point);
        std::cerr
          << "Error in the evaluation ValueAt and IsInside (all the points inside the mask shall have non-zero value) "
          << std::endl;
        std::cerr << "isInside = " << isInside << std::endl;
        std::cerr << "value = " << value << std::endl;
        std::cerr << "Index failed = " << pointIndex << std::endl;
        std::cerr << "Point failed = " << point << std::endl;
        std::cerr << "Image is a: " << image->GetNameOfClass() << std::endl;
        retval = EXIT_FAILURE;
        break;
      }
    }
  }


  std::cout << "Test finished" << std::endl;
  return retval;
}
