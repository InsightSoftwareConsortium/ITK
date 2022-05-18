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

#include "itkGaussianSpatialObject.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

int
itkGaussianSpatialObjectTest(int, char *[])
{
  using GaussianType = itk::GaussianSpatialObject<4>;

  auto myGaussian = GaussianType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(myGaussian, GaussianSpatialObject, SpatialObject);

  GaussianType::ScalarType maximum = 2;
  myGaussian->SetMaximum(maximum);
  ITK_TEST_SET_GET_VALUE(maximum, myGaussian->GetMaximum());

  GaussianType::ScalarType radius = 3;
  myGaussian->SetRadiusInObjectSpace(radius);
  ITK_TEST_SET_GET_VALUE(radius, myGaussian->GetRadiusInObjectSpace());

  GaussianType::ScalarType sigma = 1.5;
  myGaussian->SetSigmaInObjectSpace(sigma);
  ITK_TEST_SET_GET_VALUE(sigma, myGaussian->GetSigmaInObjectSpace());

  GaussianType::PointType center;
  center.Fill(0.0);
  myGaussian->SetCenterInObjectSpace(center);
  ITK_TEST_SET_GET_VALUE(center, myGaussian->GetCenterInObjectSpace());

  // Point consistency

  itk::Point<double, 4> in;
  in[0] = 1;
  in[1] = 2;
  in[2] = 1;
  in[3] = 1;
  itk::Point<double, 4> out;
  out[0] = 0;
  out[1] = 4;
  out[2] = 0;
  out[3] = 0;

  // Once all values of the Gaussian have been set, it must be updated
  myGaussian->Update();

  double value;
  myGaussian->ValueAtInWorldSpace(in, value);
  std::cout << "ValueAt(" << in << ") = " << value << std::endl;

  std::cout << "Is Inside: ";

  if (!myGaussian->IsInsideInWorldSpace(in))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  if (myGaussian->IsInsideInWorldSpace(out))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;

  std::cout << "GetEllipsoid:" << std::endl;
  std::cout << myGaussian->GetEllipsoid() << std::endl;

  std::cout << "ObjectToWorldTransform" << std::endl;

  // Create myGaussian2 as a child of myGaussian
  auto myGaussian2 = GaussianType::New();
  std::cout << "AddChild" << std::endl;
  myGaussian->AddChild(myGaussian2);

  // Once you add children, update their objectToWorldTransform individually
  //   or by calling it at the top level object
  std::cout << "ComputeObjectToWorld" << std::endl;
  myGaussian->Update();

  std::cout << "SetOffset" << std::endl;
  const GaussianType::TransformType::OffsetType::ValueType offset10 = 10.0;
  GaussianType::TransformType::OffsetType                  offset;
  offset.Fill(offset10);
  myGaussian->GetModifiableObjectToWorldTransform()->SetOffset(offset);
  myGaussian->ComputeObjectToParentTransform();

  // Once you change an object's transform, you must call ComputeObjectToWorld
  //   for it and its children to update their cached transforms
  myGaussian->Update();

  std::cout << "SetOffset2" << std::endl;
  const GaussianType::TransformType::OffsetType::ValueType offset15 = 15.0;
  GaussianType::TransformType::OffsetType                  offset2;
  offset2.Fill(offset15);
  myGaussian2->GetModifiableObjectToWorldTransform()->SetOffset(offset2);
  myGaussian2->ComputeObjectToParentTransform();

  // Once you change an object's transform, you must call ComputeObjectToWorld
  //   for it and its children to update their cached transforms
  myGaussian2->Update();

  GaussianType::TransformType::OffsetType offset3;
  offset3 = myGaussian2->GetObjectToParentTransform()->GetOffset();

  if (itk::Math::NotAlmostEquals(offset3[0], offset15 - offset10) ||
      itk::Math::NotAlmostEquals(offset3[1], offset15 - offset10) ||
      itk::Math::NotAlmostEquals(offset3[2], offset15 - offset10) ||
      itk::Math::NotAlmostEquals(offset3[3], offset15 - offset10))
  {
    std::cout << "[FAILED] : " << offset3 << " != [5.0,5.0,5.0,5.0]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;


  std::cout << "ComputeFamilyBoundingBox: ";
  myGaussian->ComputeFamilyBoundingBox(GaussianType::MaximumDepth);
  const GaussianType::BoundingBoxType * boundingBox = myGaussian->GetFamilyBoundingBoxInWorldSpace();
  std::cout << "World bounds = " << boundingBox->GetBounds() << std::endl;
  std::cout << "World Center = " << myGaussian->GetCenterInObjectSpace() << std::endl;
  std::cout << "World Radius = " << myGaussian->GetRadiusInObjectSpace() << std::endl;
  for (unsigned int i = 0; i < 3; ++i)
  {
    if (itk::Math::NotAlmostEquals(boundingBox->GetBounds()[2 * i], 7.0) ||
        itk::Math::NotAlmostEquals(boundingBox->GetBounds()[2 * i + 1], 16.0))
    {
      std::cout << "[FAILED]" << std::endl;
      return EXIT_FAILURE;
    }
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
