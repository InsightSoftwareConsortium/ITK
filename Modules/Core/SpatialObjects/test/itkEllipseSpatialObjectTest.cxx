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
 * This is a test file for the itkEllipseSpatialObject class.
 */

#include "itkEllipseSpatialObject.h"
#include "itkMath.h"

int itkEllipseSpatialObjectTest(int, char* [])
{
  typedef itk::EllipseSpatialObject<4>   EllipseType;

  EllipseType::Pointer myEllipse = EllipseType::New();
  std::cout << "Testing Print after construction" << std::endl;
  myEllipse->Print(std::cout);

  EllipseType::ArrayType radius;

  for(unsigned int i = 0; i < 4; i++)
  {
    radius[i] = i;
  }

  std::cout << "Testing radii : ";

  myEllipse->SetRadius(radius);
  EllipseType::ArrayType radius2 = myEllipse->GetRadius();
  for(unsigned int i = 0; i<4;i++)
  {
    if(itk::Math::NotExactlyEquals(radius2[i],i))
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }
  std::cout << "[PASSED]" << std::endl;

  myEllipse->SetRadius(3);
 EllipseType::ArrayType radius3 = myEllipse->GetRadius();
  std::cout << "Testing Global radii : ";
  for(unsigned int i = 0; i<4;i++)
  {
    if(itk::Math::NotExactlyEquals(radius3[i],3))
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

   // Point consistency
  std::cout << "Is Inside: ";
  itk::Point<double,4> in;
  in[0]=1;in[1]=2;in[2]=1;in[3]=1;
  itk::Point<double,4> out;
  out[0]=0;out[1]=4;out[2]=0;out[3]=0;

  if(!myEllipse->IsInside(in))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  if(myEllipse->IsInside(out))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  std::cout<<"[PASSED]"<<std::endl;


   std::cout << "ObjectToWorldTransform : ";

  // Create myEllipse2 as a child of myEllipse
  EllipseType::Pointer myEllipse2 = EllipseType::New();
  myEllipse2->SetRadius(1);
  myEllipse->AddSpatialObject(myEllipse2);

  EllipseType::TransformType::OffsetType offset;
  offset.Fill(10);

  myEllipse->GetModifiableObjectToWorldTransform()->SetOffset(offset);
  myEllipse->ComputeObjectToParentTransform();

  EllipseType::TransformType::OffsetType offset2;
  offset2.Fill(15);
  myEllipse2->GetModifiableObjectToWorldTransform()->SetOffset(offset2);
  myEllipse2->ComputeObjectToParentTransform();

  EllipseType::TransformType::OffsetType offset3;
  offset3 = myEllipse2->GetObjectToParentTransform()->GetOffset();

  if( (itk::Math::NotExactlyEquals(offset3[0],5)) || (itk::Math::NotExactlyEquals(offset3[1],5))
     ||(itk::Math::NotExactlyEquals(offset3[2],5)) ||(itk::Math::NotExactlyEquals(offset3[3],5))
     )
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }
  std::cout<<"[PASSED]"<<std::endl;

  std::cout << "ComputeBoundingBox: ";
  myEllipse->ComputeBoundingBox();
  EllipseType::BoundingBoxType * boundingBox = myEllipse->GetBoundingBox();

  for(unsigned int i=0;i<3;i++)
  {
    if(   itk::Math::NotAlmostEquals(boundingBox->GetBounds()[2*i], 7 )
       || itk::Math::NotAlmostEquals(boundingBox->GetBounds()[2*i+1], 16 )
       )
    {
      std::cout<<"[FAILED]"<<std::endl;
      return EXIT_FAILURE;
    }

  }
  std::cout << "Testing Print after use" << std::endl;
  myEllipse->Print(std::cout);

  std::cout<<"[PASSED]"<<std::endl;
  return EXIT_SUCCESS;

}
