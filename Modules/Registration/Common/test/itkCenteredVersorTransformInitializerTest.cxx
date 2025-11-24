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

#include "itkCenteredVersorTransformInitializer.h"

#include "itkImageRegionRange.h"
#include "itkTestingMacros.h"

#include <algorithm> // For fill.

/**
 *  This program tests the use of the CenteredVersorTransformInitializer class
 *
 *
 */

int
itkCenteredVersorTransformInitializerTest(int, char *[])
{

  bool pass = true;

  constexpr unsigned int Dimension{ 3 };

  // Fixed Image Type
  using FixedImageType = itk::Image<unsigned char, Dimension>;

  // Moving Image Type
  using MovingImageType = itk::Image<unsigned char, Dimension>;

  // Size Type
  using SizeType = FixedImageType::SizeType;
  using SpacingType = FixedImageType::SpacingType;
  using PointType = FixedImageType::PointType;
  using IndexType = FixedImageType::IndexType;
  using RegionType = FixedImageType::RegionType;


  // Transform Type
  using TransformType = itk::VersorRigid3DTransform<double>;

  SizeType         size{ { 100, 100, 150 } };
  PointType        fixedOrigin{};
  PointType        movingOrigin{ { 29.0, 17.0, 13.0 } };
  SpacingType      spacing{ { 1.5, 1.5, 1.0 } };
  const RegionType region{ size };


  auto fixedImage = FixedImageType::New();
  auto movingImage = MovingImageType::New();

  fixedImage->SetRegions(region);
  fixedImage->SetSpacing(spacing);
  fixedImage->SetOrigin(fixedOrigin);
  fixedImage->AllocateInitialized();

  movingImage->SetRegions(region);
  movingImage->SetSpacing(spacing);
  movingImage->SetOrigin(movingOrigin);
  movingImage->AllocateInitialized();

  RegionType internalRegion;
  SizeType   internalSize;
  IndexType  internalIndex;

  internalIndex[0] = 20;
  internalIndex[1] = 30;
  internalIndex[2] = 10;

  internalSize[0] = size[0] - 2 * 20;
  internalSize[1] = size[1] - 2 * 30;
  internalSize[2] = size[2] - 2 * 10;


  internalRegion = { internalIndex, internalSize };

  const itk::ImageRegionRange<FixedImageType> fixedImageRegionRange(*fixedImage, internalRegion);
  std::fill(fixedImageRegionRange.begin(), fixedImageRegionRange.end(), 200);

  internalIndex[0] = 10;
  internalIndex[1] = 20;
  internalIndex[2] = 30;

  internalSize[0] = size[0] - 2 * 10;
  internalSize[1] = size[1] - 2 * 20;
  internalSize[2] = size[2] - 2 * 30;


  internalRegion = { internalIndex, internalSize };

  const itk::ImageRegionRange<MovingImageType> movingImageRegionRange(*movingImage, internalRegion);
  std::fill(movingImageRegionRange.begin(), movingImageRegionRange.end(), 200);

  auto transform = TransformType::New();
  transform->SetIdentity();


  using InitializerType = itk::CenteredVersorTransformInitializer<FixedImageType, MovingImageType>;

  auto initializer = InitializerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(initializer, CenteredVersorTransformInitializer, CenteredTransformInitializer);


  initializer->SetFixedImage(fixedImage);
  initializer->SetMovingImage(movingImage);
  initializer->SetTransform(transform);

  initializer->InitializeTransform();

  TransformType::OutputVectorType translation2 = transform->GetTranslation();
  TransformType::OffsetType       offset2 = transform->GetOffset();

  { // Verifications
    TransformType::InputPointType fixedCenter;
    TransformType::InputPointType movingCenter;

    for (unsigned int j = 0; j < Dimension; ++j)
    {
      fixedCenter[j] = fixedOrigin[j] + size[j] * spacing[j] / 2.0;
      movingCenter[j] = movingOrigin[j] + size[j] * spacing[j] / 2.0;
    }

    TransformType::InputVectorType relativeCenter = movingCenter - fixedCenter;


    constexpr double tolerance{ 1e-3 };

    for (unsigned int k = 0; k < Dimension; ++k)
    {
      if (itk::Math::abs(translation2[k] - relativeCenter[k]) > tolerance)
      {
        std::cerr << "Translation differs from expected value" << std::endl;
        std::cerr << "It should be " << relativeCenter << std::endl;
        std::cerr << "but it is    " << translation2 << std::endl;
        pass = false;
        break;
      }
      if (itk::Math::abs(offset2[k] - relativeCenter[k]) > tolerance)
      {
        std::cerr << "Offset differs from expected value" << std::endl;
        std::cerr << "It should be " << relativeCenter << std::endl;
        std::cerr << "but it is    " << offset2 << std::endl;
        pass = false;
        break;
      }
    }

    auto computeRotation = true;
    ITK_TEST_SET_GET_BOOLEAN(initializer, ComputeRotation, computeRotation);

    initializer->InitializeTransform();

    std::cout << "Initialized Transform is" << std::endl;

    transform->Print(std::cout);

    TransformType::InputPointType mappedOrigin = transform->TransformPoint(fixedOrigin);
    TransformType::InputPointType expectedPoint;
    expectedPoint[0] = 29.0;
    expectedPoint[1] = 165.75;
    expectedPoint[2] = 13.25;

    for (unsigned int j = 0; j < Dimension; ++j)
    {
      if (itk::Math::abs(expectedPoint[j] - mappedOrigin[j]) > tolerance)
      {
        std::cerr << "Mapped point differs from expected point" << std::endl;
        std::cerr << "It should be " << expectedPoint << std::endl;
        std::cerr << "but it is    " << mappedOrigin << std::endl;
        pass = false;
        break;
      }
    }
  }

  if (!pass)
  {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}
