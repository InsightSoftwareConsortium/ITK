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

#include "itkMembershipFunctionBase.h"

#include "itkGTest.h"

#include <iostream>

namespace itk::Statistics::MembershipFunctionBaseTest
{

template <typename TMeasurementVector>
class MyMembershipFunctionBase : public MembershipFunctionBase<TMeasurementVector>
{
public:
  /** Standard class type alias. */
  using Self = MyMembershipFunctionBase;

  using Superclass = MembershipFunctionBase<TMeasurementVector>;

  using Pointer = SmartPointer<Self>;

  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(MyMembershipFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate membership score */
  double
  Evaluate(const TMeasurementVector &) const override
  {
    constexpr double score{ 1 };
    return score;
  }
};

} // namespace itk::Statistics::MembershipFunctionBaseTest


TEST(MembershipFunctionBase, ConvertedLegacyTest)
{
  constexpr unsigned int MeasurementVectorSize{ 17 };

  using MeasurementVectorType = itk::FixedArray<float, MeasurementVectorSize>;

  using MembershipFunctionBaseType =
    itk::Statistics::MembershipFunctionBaseTest::MyMembershipFunctionBase<MeasurementVectorType>;

  auto function = MembershipFunctionBaseType::New();

  std::cout << function->GetNameOfClass() << std::endl;
  std::cout << function->MembershipFunctionBaseType::Superclass::GetNameOfClass() << std::endl;

  function->Print(std::cout);

  function->SetMeasurementVectorSize(MeasurementVectorSize); // for code coverage

  EXPECT_EQ(function->GetMeasurementVectorSize(), MeasurementVectorSize);

  // Test if an exception will be thrown if we try to resize the measurement vector
  // size
  EXPECT_THROW(function->SetMeasurementVectorSize(MeasurementVectorSize + 1), itk::ExceptionObject);
}
