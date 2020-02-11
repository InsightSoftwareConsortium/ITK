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

/**
 * This is a test file for the itkEllipseSpatialObject class.
 */

#include "itkEllipseSpatialObject.h"
#include "itkMath.h"

int
itkEllipseSpatialObjectTest(int, char *[])
{
  using EllipseType = itk::EllipseSpatialObject<4>;

  EllipseType::Pointer myEllipse = EllipseType::New();
  std::cout << "Testing Print after construction" << std::endl;
  myEllipse->Print(std::cout);

  EllipseType::ArrayType radii;

  for (unsigned int i = 0; i < 4; i++)
  {
    radii[i] = i;
  }

  std::cout << "Testing radii : ";

  myEllipse->SetRadiusInObjectSpace(radii);
  myEllipse->Update();
  EllipseType::ArrayType radii2 = myEllipse->GetRadiusInObjectSpace();
  for (unsigned int i = 0; i < 4; i++)
  {
    if (itk::Math::NotExactlyEquals(radii2[i], i))
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }
  std::cout << "[PASSED]" << std::endl;

  myEllipse->SetRadiusInObjectSpace(3);
  myEllipse->Update();
  EllipseType::ArrayType radii3 = myEllipse->GetRadiusInObjectSpace();
  std::cout << "Testing Global radii : ";
  for (unsigned int i = 0; i < 4; i++)
  {
    if (itk::Math::NotExactlyEquals(radii3[i], 3))
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Point consistency
  std::cout << "Is Inside: ";
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

  if (!myEllipse->IsInsideInWorldSpace(in))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  if (myEllipse->IsInsideInWorldSpace(out))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;


  std::cout << "ObjectToWorldTransform : ";

  // Create myEllipse2 as a child of myEllipse
  EllipseType::Pointer myEllipse2 = EllipseType::New();
  myEllipse2->SetRadiusInObjectSpace(1);
  myEllipse->AddChild(myEllipse2);

  EllipseType::TransformType::OffsetType offset;
  offset.Fill(10);
  myEllipse->GetModifiableObjectToWorldTransform()->SetOffset(offset);
  myEllipse->ComputeObjectToParentTransform();

  EllipseType::TransformType::OffsetType offset2;
  offset2.Fill(15);
  myEllipse2->GetModifiableObjectToWorldTransform()->SetOffset(offset2);
  myEllipse2->ComputeObjectToParentTransform();

  EllipseType::TransformType::OffsetType offset3;
  offset3 = myEllipse2->GetModifiableObjectToParentTransform()->GetOffset();

  if ((itk::Math::NotExactlyEquals(offset3[0], 5)) || (itk::Math::NotExactlyEquals(offset3[1], 5)) ||
      (itk::Math::NotExactlyEquals(offset3[2], 5)) || (itk::Math::NotExactlyEquals(offset3[3], 5)))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  // NOTE: ORDER OF Update() and ComputeFamilyBoundingBox() is important.
  myEllipse->Update();
  myEllipse->ComputeFamilyBoundingBox(EllipseType::MaximumDepth);
  const EllipseType::BoundingBoxType * boundingBox = myEllipse->GetFamilyBoundingBoxInWorldSpace();
  std::cout << "Bounds = " << boundingBox->GetBounds() << std::endl;

  std::cout << "Update(): ";
  for (unsigned int i = 0; i < 3; i++)
  {
    const EllipseType::BoundingBoxType::BoundsArrayType bounds = boundingBox->GetBounds();
    if (itk::Math::NotAlmostEquals(bounds[2 * i], 7) ||
        itk::Math::NotAlmostEquals(bounds[2 * i + 1],
                                   16) // this is 13 if Update() and ComputeFamilyBoundingBox are reversed order.
    )
    {
      std::cout << "[FAILED]" << std::endl;
      return EXIT_FAILURE;
    }
  }
  std::cout << "Testing Print after use" << std::endl;
  myEllipse->Print(std::cout);

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
