/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
// Disable warning for long symbol names in this file only

/** This is a test file for the itkSceneSpatialObject class. */
#include "itkSceneSpatialObject.h"
#include "itkEllipseSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkMath.h"

int itkSceneSpatialObjectTest(int, char* [])
{
  // Create the SceneSpatialObject
  typedef itk::SceneSpatialObject<3>  SceneSpatialObjectType;
  SceneSpatialObjectType::Pointer SceneSpatialObject = SceneSpatialObjectType::New();
  SceneSpatialObject->Print(std::cout);

  // Create two ellipses to put in the SceneSpatialObject
  typedef itk::EllipseSpatialObject<3>   EllipseType;
  EllipseType::Pointer ellipse1 = EllipseType::New();
  EllipseType::Pointer ellipse2 = EllipseType::New();

  SceneSpatialObject->AddSpatialObject(ellipse1);
  SceneSpatialObject->AddSpatialObject(ellipse2);

  if(SceneSpatialObject->GetNumberOfObjects() != 2 )
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  SceneSpatialObject->GetMTime(); // coverage
  std::cout << SceneSpatialObject << std::endl;

  // Test spatial objects for coverage
  typedef itk::GroupSpatialObject<3> SpatialObjectType;
  SpatialObjectType::Pointer object = SpatialObjectType::New();

  std::cout << "Testing Typename: ";
  if(strcmp(object->GetTypeName(),"GroupSpatialObject"))
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  object->GetRequestedRegion();
  object->GetBufferedRegion();
  object->GetLargestPossibleRegion();
  object->SetBoundingBoxChildrenDepth(0);

  std::cout << "Testing  BoundingBoxChildren depth: ";
  if(object->GetBoundingBoxChildrenDepth() != 0)
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing BoundingBoxChildrenName: ";
  if(object->GetBoundingBoxChildrenName() != "")
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing Set/GetParentID: ";
  object->SetParentId(3);

  if(object->GetParentId() != 3)
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing Set/GetSpacing: ";
  double spacing[3];
  spacing[0] = 1;
  spacing[1] = 2;
  spacing[2] = 3;

  object->SetSpacing(spacing);
  const double* res_spacing = object->GetSpacing();
  if((itk::Math::NotExactlyEquals(res_spacing[0], 1)) ||
     (itk::Math::NotExactlyEquals(res_spacing[1], 2)) ||
     (itk::Math::NotExactlyEquals(res_spacing[2], 3)) )
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Testing Clear(): ";
  // Testing the clear function
  SceneSpatialObject->Clear();
  if(SceneSpatialObject->GetNumberOfObjects() != 0)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }

  SceneSpatialObject->Print(std::cout);
  std::cout << "[PASSED]" << std::endl;
  std::cout << "[DONE]" << std::endl;

  return EXIT_SUCCESS;

}
