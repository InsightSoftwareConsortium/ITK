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

/**
 * This is a test file for the itkBoxSpatialObject class.
 */
#include "itkImageFileWriter.h"
#include "itkGroupSpatialObject.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkBoxSpatialObject.h"
#include "itkTestingMacros.h"

int
itkBoxSpatialObjectTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing Parameters: Usage " << itkNameOfTestExecutableMacro(argv) << "OutputImageFile" << std::endl;
  }

  constexpr unsigned int Dimension = 2;
  using SceneType = itk::GroupSpatialObject<Dimension>;
  using BoxType = itk::BoxSpatialObject<Dimension>;
  using OutputImageType = itk::Image<unsigned char, Dimension>;
  using SpatialObjectToImageFilterType = itk::SpatialObjectToImageFilter<SceneType, OutputImageType>;

  auto scene = SceneType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(scene, GroupSpatialObject, SpatialObject);


  auto box1 = BoxType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(box1, BoxSpatialObject, SpatialObject);


  auto box2 = BoxType::New();
  box1->SetId(1);

  // Test the SetProperty()
  scene->AddChild(box1);
  scene->AddChild(box2);

  BoxType::SizeType boxsize1;
  BoxType::SizeType boxsize2;

  boxsize1[0] = 30;
  boxsize1[1] = 30;
  box1->SetSizeInObjectSpace(boxsize1);
  ITK_TEST_SET_GET_VALUE(boxsize1, box1->GetSizeInObjectSpace());

  boxsize2[0] = 30;
  boxsize2[1] = 30;
  box2->SetSizeInObjectSpace(boxsize2);

  BoxType::TransformType::OffsetType offset1;

  offset1[0] = 29.0;
  offset1[1] = 29.0;
  box1->GetModifiableObjectToParentTransform()->SetOffset(offset1);
  box1->Update();

  BoxType::PointType point1;
  point1[0] = 50.0;
  point1[1] = 50.0;
  box2->SetPositionInObjectSpace(point1);
  ITK_TEST_SET_GET_VALUE(point1, box2->GetPositionInObjectSpace());

  box2->Update();

  scene->Update();

  std::cout << "Test Update(): " << std::endl;
  std::cout << box1->GetMyBoundingBoxInWorldSpace()->GetBounds() << std::endl;
  std::cout << box2->GetMyBoundingBoxInWorldSpace()->GetBounds() << std::endl;
  const BoxType::BoundingBoxType * boundingBox = box1->GetMyBoundingBoxInWorldSpace();

  if (itk::Math::NotAlmostEquals(boundingBox->GetBounds()[0], 29) ||
      itk::Math::NotAlmostEquals(boundingBox->GetBounds()[1], 59) ||
      itk::Math::NotAlmostEquals(boundingBox->GetBounds()[2], 29) ||
      itk::Math::NotAlmostEquals(boundingBox->GetBounds()[3], 59))
  {
    std::cout << "[FAILED] Test returned" << std::endl;
    std::cout << box1->GetMyBoundingBoxInWorldSpace()->GetBounds() << std::endl;
    std::cout << "Instead of [29 59 29 59]" << std::endl;
    return EXIT_FAILURE;
  }

  box2->ComputeFamilyBoundingBox();
  const BoxType::BoundingBoxType * boundingBox2 = box2->GetFamilyBoundingBoxInWorldSpace();
  if (itk::Math::NotAlmostEquals(boundingBox2->GetBounds()[0], 50) ||
      itk::Math::NotAlmostEquals(boundingBox2->GetBounds()[1], 80) ||
      itk::Math::NotAlmostEquals(boundingBox2->GetBounds()[2], 50) ||
      itk::Math::NotAlmostEquals(boundingBox2->GetBounds()[3], 80))
  {
    std::cout << "[FAILED] Test returned" << std::endl;
    std::cout << box2->GetMyBoundingBoxInWorldSpace()->GetBounds() << std::endl;
    std::cout << "Instead of [50 80 50 80]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;

  // Point consistency
  std::cout << "Test Is Inside: ";
  itk::Point<double, 2> in;
  in[0] = 30.0;
  in[1] = 30.0;
  itk::Point<double, 2> out;
  out[0] = 0;
  out[1] = 4;

  if (!box1->IsInsideInWorldSpace(in))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  if (box1->IsInsideInWorldSpace(out))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Test SpatialObjectToImageFilter / IsInside " << std::endl;
  auto imageFilter = SpatialObjectToImageFilterType::New();
  imageFilter->SetInput(scene);

  OutputImageType::SizeType size;
  size[0] = 100;
  size[1] = 100;
  imageFilter->SetSize(size);

  SpatialObjectToImageFilterType::PointType origin;
  origin[0] = 0;
  origin[1] = 0;
  imageFilter->SetOrigin(origin);

  imageFilter->SetInsideValue(255);
  imageFilter->SetOutsideValue(0);
  imageFilter->Update();

  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(imageFilter->GetOutput(), argv[1]));

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
