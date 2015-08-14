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

/**
 * This is a test file for the itkArrowSpatialObject class.
 */

#include "itkArrowSpatialObject.h"
#include "itkMath.h"

int itkArrowSpatialObjectTest(int, char* [])
{
  typedef itk::ArrowSpatialObject<3>   ArrowType;

  ArrowType::Pointer myArrow = ArrowType::New();

  // Testing the length
  std::cout << "Testing length : ";
  myArrow->SetLength(2);
  if(itk::Math::NotExactlyEquals(myArrow->GetLength(), 2))
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;


  // Testing the direction of the arrow
  std::cout << "Testing direction : ";

  ArrowType::VectorType direction;
  direction.Fill(0);
  direction[1] = 1.0;

  myArrow->SetDirection(direction);
  if(itk::Math::NotExactlyEquals(myArrow->GetDirection()[0], 0)
    || itk::Math::NotExactlyEquals(myArrow->GetDirection()[1], 1)
    || itk::Math::NotExactlyEquals(myArrow->GetDirection()[2], 0)
    )
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  // Point consistency
  std::cout << "Is Inside: ";
  itk::Point<double,3> in;
  in[0]=0;in[1]=1;in[2]=0;
  itk::Point<double,3> out;
  out[0]=0;out[1]=2.1;out[2]=0;

  if(!myArrow->IsInside(in))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  if(myArrow->IsInside(out))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  std::cout<<"[PASSED]"<<std::endl;


  std::cout << "ComputeBoundingBox: ";
  myArrow->ComputeBoundingBox();
  ArrowType::BoundingBoxType * boundingBox = myArrow->GetBoundingBox();

  if( (itk::Math::NotExactlyEquals(boundingBox->GetBounds()[2], 0) )
     || (itk::Math::NotExactlyEquals(boundingBox->GetBounds()[3], 1) )
      )
    {
      std::cout<<"[FAILED]"<<std::endl;
      return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing 2D Arrow:";
  typedef itk::ArrowSpatialObject<2>   Arrow2DType;
  Arrow2DType::Pointer myArrow2D = Arrow2DType::New();
  myArrow2D->Print(std::cout);

  std::cout << "[PASSED]" << std::endl;

  std::cout << "Test: [DONE]" << std::endl;
  return EXIT_SUCCESS;

}
