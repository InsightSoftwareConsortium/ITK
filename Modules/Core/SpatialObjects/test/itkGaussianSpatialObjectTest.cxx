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
 * This is a test file for the itkGaussianSpatialObject class.
 */

#include "itkGaussianSpatialObject.h"
#include "itkMath.h"

int itkGaussianSpatialObjectTest(int, char* [])
{
  typedef itk::GaussianSpatialObject<4>   GaussianType;

  GaussianType::Pointer myGaussian = GaussianType::New();
  myGaussian->Print(std::cout);

  myGaussian->SetMaximum(2);
  GaussianType::ScalarType maximum = myGaussian->GetMaximum();
  std::cout << "Testing Maximum: ";
  if( itk::Math::NotExactlyEquals(maximum, 2) )
    {
    std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }

  myGaussian->SetRadius(3);
  GaussianType::ScalarType radius =
    myGaussian->GetRadius();
  std::cout << "Testing Radius: ";
  if( itk::Math::NotExactlyEquals(radius, 3) )
    {
    std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }

  myGaussian->SetSigma(1.5);
  GaussianType::ScalarType sigma =
    myGaussian->GetSigma();
  std::cout << "Testing Sigma: ";
  if( sigma != 1.5 )
    {
    std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }


  std::cout << "[PASSED]" << std::endl;

  // Point consistency

  itk::Point<double,4> in;
  in[0]=1; in[1]=2; in[2]=1; in[3]=1;
  itk::Point<double,4> out;
  out[0]=0; out[1]=4; out[2]=0; out[3]=0;

  double value;
  myGaussian->ValueAt(in, value);
  std::cout << "ValueAt(" << in << ") = " << value << std::endl;

  std::cout << "Is Inside: ";

  if(!myGaussian->IsInside(in))
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }

  if(myGaussian->IsInside(out))
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }

  std::cout<<"[PASSED]"<<std::endl;

  std::cout << "GetEllipsoid:\n"
            << myGaussian->GetEllipsoid() << std::endl;

  std::cout << "ObjectToWorldTransform : ";

  // Create myGaussian2 as a child of myGaussian
  GaussianType::Pointer myGaussian2 = GaussianType::New();
  myGaussian->AddSpatialObject(myGaussian2);

  GaussianType::TransformType::OffsetType offset;
  offset.Fill(10);

  myGaussian->GetModifiableObjectToWorldTransform()->SetOffset(offset);
  myGaussian->ComputeObjectToParentTransform();

  GaussianType::TransformType::OffsetType offset2;
  offset2.Fill(15);
  myGaussian2->GetModifiableObjectToWorldTransform()->SetOffset(offset2);
  myGaussian2->ComputeObjectToParentTransform();

  GaussianType::TransformType::OffsetType offset3;

  offset3 = myGaussian2->GetObjectToParentTransform()->GetOffset();

  if(    itk::Math::NotAlmostEquals(offset3[0], 5.0) || itk::Math::NotAlmostEquals(offset3[1], 5.0)
      || itk::Math::NotAlmostEquals(offset3[2], 5.0) || itk::Math::NotAlmostEquals(offset3[3], 5.0)
    )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  std::cout<<"[PASSED]"<<std::endl;


  std::cout << "ComputeBoundingBox: ";
  myGaussian->ComputeBoundingBox();
  GaussianType::BoundingBoxType * boundingBox = myGaussian->GetBoundingBox();

  for(unsigned int i=0;i<3;i++)
    {
    if(  itk::Math::NotAlmostEquals(boundingBox->GetBounds()[2*i], 7.0 )
      || itk::Math::NotAlmostEquals(boundingBox->GetBounds()[2*i+1], 16.0 )
      )
      {
      std::cout<<"[FAILED]"<<std::endl;
      return EXIT_FAILURE;
      }

    }

  myGaussian->Print(std::cout);

  std::cout<<"[PASSED]"<<std::endl;
  return EXIT_SUCCESS;

}
