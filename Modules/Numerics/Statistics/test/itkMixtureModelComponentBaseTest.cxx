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

#include "itkMixtureModelComponentBase.h"
#include "itkListSample.h"
#include "itkTestingMacros.h"

namespace itk
{
namespace Statistics
{

template <typename TSample>
class MixtureModelComponentBaseTestHelper : public MixtureModelComponentBase<TSample>
{
public:
  using Self = MixtureModelComponentBaseTestHelper;
  using Superclass = MixtureModelComponentBase<TSample>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(MixtureModelComponentBaseTestHelper, MixtureModelComponentBase);

  itkNewMacro(Self);

  void
  RunTests()
  {
    std::cout << "Superclass Name " << this->Superclass::GetNameOfClass() << std::endl;
    std::cout << "This class Name " << this->GetNameOfClass() << std::endl;

    this->Superclass::Print(std::cout);
    this->Print(std::cout);

    std::cout << "Full Parameters = " << this->Superclass::GetFullParameters() << std::endl;
    std::cout << "Minimal change  = " << this->Superclass::GetMinimalParametersChange() << std::endl;
  }

protected:
  void
  GenerateData() override
  {
    std::cout << "Executing GenerateData() " << std::endl;
  }
};

} // namespace Statistics
} // namespace itk

int
itkMixtureModelComponentBaseTest(int, char *[])
{
  using MeasurementVectorType = itk::Array<double>;
  using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;

  using ComponentType = itk::Statistics::MixtureModelComponentBaseTestHelper<SampleType>;

  auto component = ComponentType::New();
  std::cout << "component->GetWeights(): " << component->GetWeights() << std::endl;
  component->RunTests();

  ITK_TRY_EXPECT_EXCEPTION(component->GetWeight(5));

  std::cerr << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
