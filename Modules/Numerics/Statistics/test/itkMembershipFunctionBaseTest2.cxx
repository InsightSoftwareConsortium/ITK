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

#include <iostream>
#include "itkMembershipFunctionBase.h"

namespace itk {
namespace Statistics {
namespace MembershipFunctionBaseTest {

template <typename TMeasurementVector>
class MyMembershipFunctionBase : public MembershipFunctionBase< TMeasurementVector >
{
public:
  /** Standard class typedef. */
  typedef MyMembershipFunctionBase  Self;

  typedef MembershipFunctionBase< TMeasurementVector > Superclass;

  typedef SmartPointer< Self > Pointer;

  typedef SmartPointer<const Self> ConstPointer;

  /** Standard macros */
  itkTypeMacro(MyMembershipFunctionBase, MembershipFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate membership score */
  double Evaluate(const TMeasurementVector & ) const ITK_OVERRIDE
    {
    double score;
    score = 1;
    return score;
    }

};

}
}
}

/* Test MembershipFunctionBase using a resizable vector type */
int itkMembershipFunctionBaseTest2(int, char* [] )
{

  const unsigned int MeasurementVectorSize = 3;

  typedef itk::Array< float>  MeasurementVectorType;

  typedef itk::Statistics::MembershipFunctionBaseTest::MyMembershipFunctionBase<
    MeasurementVectorType >   MembershipFunctionBaseType;

  MembershipFunctionBaseType::Pointer function = MembershipFunctionBaseType::New();

  std::cout << function->GetNameOfClass() << std::endl;
  std::cout << function->MembershipFunctionBaseType::Superclass::GetNameOfClass() << std::endl;

  function->Print(std::cout);

  function->SetMeasurementVectorSize( MeasurementVectorSize );
  if( function->GetMeasurementVectorSize() != MeasurementVectorSize )
    {
    std::cerr << "Set/GetMeasurementVectorSize() Failed !" << std::endl;
    return EXIT_FAILURE;
    }

  //Test if it is possible to reset the vector size
  function->SetMeasurementVectorSize( MeasurementVectorSize );
  if( function->GetMeasurementVectorSize() != MeasurementVectorSize )
    {
    std::cerr << "Set/GetMeasurementVectorSize() Failed !" << std::endl;
    return EXIT_FAILURE;
    }

  //test if it is possible to change the measurement vector size
  MembershipFunctionBaseType::MeasurementVectorSizeType newSize = 5;
  function->SetMeasurementVectorSize( newSize ); // for code coverage

  if( function->GetMeasurementVectorSize() != newSize )
    {
    std::cerr << "Set/GetMeasurementVectorSize() Failed !" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
