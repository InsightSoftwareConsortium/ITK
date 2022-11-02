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

#include <iostream>
#include "itkMembershipFunctionBase.h"

namespace itk
{
namespace Statistics
{
namespace MembershipFunctionBaseTest
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

  /** Standard macros */
  itkTypeMacro(MyMembershipFunctionBase, MembershipFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate membership score */
  double
  Evaluate(const TMeasurementVector &) const override
  {
    double score;
    score = 1;
    return score;
  }
};

} // namespace MembershipFunctionBaseTest
} // namespace Statistics
} // namespace itk
int
itkMembershipFunctionBaseTest(int, char *[])
{

  constexpr unsigned int MeasurementVectorSize = 17;

  using MeasurementVectorType = itk::FixedArray<float, MeasurementVectorSize>;

  using MembershipFunctionBaseType =
    itk::Statistics::MembershipFunctionBaseTest::MyMembershipFunctionBase<MeasurementVectorType>;

  auto function = MembershipFunctionBaseType::New();

  std::cout << function->GetNameOfClass() << std::endl;
  std::cout << function->MembershipFunctionBaseType::Superclass::GetNameOfClass() << std::endl;

  function->Print(std::cout);

  function->SetMeasurementVectorSize(MeasurementVectorSize); // for code coverage

  if (function->GetMeasurementVectorSize() != MeasurementVectorSize)
  {
    std::cerr << "GetMeasurementVectorSize() Failed !" << std::endl;
    return EXIT_FAILURE;
  }

  // Test if an exception will be thrown if we try to resize the measurement vector
  // size
  try
  {
    function->SetMeasurementVectorSize(MeasurementVectorSize + 1);
    std::cerr
      << "Exception should have been thrown since we are trying to resize non-resizeable measurement vector type "
      << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Caughted expected exception: " << excp << std::endl;
  }

  return EXIT_SUCCESS;
}
