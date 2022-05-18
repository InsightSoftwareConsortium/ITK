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

#include "itkArrowSpatialObject.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

int
itkArrowSpatialObjectTest(int, char *[])
{
  using ArrowType = itk::ArrowSpatialObject<3>;

  auto myArrow = ArrowType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(myArrow, ArrowSpatialObject, SpatialObject);


  // Testing the position
  std::cout << "Testing position : ";
  ArrowType::PointType pnt;
  pnt[0] = 0;
  pnt[1] = 1;
  pnt[2] = 0;
  myArrow->SetPositionInObjectSpace(pnt);
  myArrow->Update();

  ITK_TEST_SET_GET_VALUE(pnt, myArrow->GetPositionInObjectSpace());

  // Testing the length
  std::cout << "Testing length : ";
  double length = 2;
  myArrow->SetLengthInObjectSpace(length);
  ITK_TEST_SET_GET_VALUE(length, myArrow->GetLengthInObjectSpace());

  // Testing the direction of the arrow
  std::cout << "Testing direction : ";

  ArrowType::VectorType direction;
  direction.Fill(0);
  direction[1] = 1.0;

  myArrow->SetDirectionInObjectSpace(direction);
  myArrow->Update();

  ITK_TEST_SET_GET_VALUE(direction, myArrow->GetDirectionInObjectSpace());

  // Point consistency
  std::cout << "Is Inside (Inside): ";
  itk::Point<double, 3> in;
  in[0] = 0;
  in[1] = 1;
  in[2] = 0;
  if (!myArrow->IsInsideInWorldSpace(in))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Is Inside (Outside): ";
  itk::Point<double, 3> out;
  out[0] = 0;
  out[1] = 2.1;
  out[2] = 0;
  if (myArrow->IsInsideInWorldSpace(out))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;


  std::cout << "Update(): ";
  const ArrowType::BoundingBoxType * boundingBox = myArrow->GetMyBoundingBoxInWorldSpace();

  std::cout << boundingBox->GetBounds() << std::endl;
  if ((itk::Math::NotExactlyEquals(boundingBox->GetBounds()[2], 1)) ||
      (itk::Math::NotExactlyEquals(boundingBox->GetBounds()[3], 1)))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing 2D Arrow:";
  using Arrow2DType = itk::ArrowSpatialObject<2>;
  auto myArrow2D = Arrow2DType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(myArrow2D, ArrowSpatialObject, SpatialObject);


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
