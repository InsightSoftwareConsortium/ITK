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

#include "itkEllipseSpatialObject.h"
#include "itkPointSetToSpatialObjectDemonsRegistration.h"

#include "itkRegularSphereMeshSource.h"
#include "itkTestingMacros.h"


int
itkPointSetToSpatialObjectDemonsRegistrationTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;

  using EllipseType = itk::EllipseSpatialObject<Dimension>;

  // Create an ellipse.
  auto ellipse = EllipseType::New();

  // Set the radius
  ellipse->SetRadiusInObjectSpace(50);

  // Set its position
  EllipseType::TransformType::OffsetType offset;
  offset[0] = 50;
  offset[1] = 50;
  offset[2] = 50;

  ellipse->Update();

  using PointSetType = itk::Mesh<float, Dimension>;

  using SphereType = itk::RegularSphereMeshSource<PointSetType>;

  auto sphereSource = SphereType::New();

  sphereSource->Update();


  using DemonsRegistrationType = itk::PointSetToSpatialObjectDemonsRegistration<PointSetType, EllipseType>;

  auto demonsRegistration = DemonsRegistrationType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(demonsRegistration, PointSetToSpatialObjectDemonsRegistration, ProcessObject);

  auto fixedPointSet = sphereSource->GetOutput();
  demonsRegistration->SetFixedPointSet(fixedPointSet);
  ITK_TEST_SET_GET_VALUE(fixedPointSet, demonsRegistration->GetFixedPointSet());

  demonsRegistration->SetMovingSpatialObject(ellipse);
  ITK_TEST_SET_GET_VALUE(ellipse, demonsRegistration->GetMovingSpatialObject());

  ITK_TRY_EXPECT_NO_EXCEPTION(demonsRegistration->Update());


  std::cout << "Test Succeed!" << std::endl;
  return EXIT_SUCCESS;
}
