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

#include "itkProbabilityDistribution.h"

namespace itk
{
namespace Statistics
{

class ProbabilityDistributionTestingHelper : public ProbabilityDistribution
{
public:
  using Self = ProbabilityDistributionTestingHelper;
  using Superclass = ProbabilityDistribution;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(ProbabilityDistributionTestingHelper, ProbabilityDistribution);

  itkNewMacro(Self);

  SizeValueType
  GetNumberOfParameters() const override
  {
    return 42;
  }
  double
  EvaluatePDF(double) const override
  {
    return 42.0;
  }
  double
  EvaluatePDF(double, const ParametersType &) const override
  {
    return 42.0;
  }
  double
  EvaluateCDF(double) const override
  {
    return 42.0;
  }
  double
  EvaluateCDF(double, const ParametersType &) const override
  {
    return 42.0;
  }
  double
  EvaluateInverseCDF(double) const override
  {
    return 42.0;
  }
  double
  EvaluateInverseCDF(double, const ParametersType &) const override
  {
    return 42.0;
  }
  bool
  HasMean() const override
  {
    return true;
  }
  bool
  HasVariance() const override
  {
    return true;
  }
  double
  GetMean() const override
  {
    return 42.0;
  }
  double
  GetVariance() const override
  {
    return 42.0;
  }

  void
  RunTests()
  {
    std::cout << "Superclass name = " << this->Superclass::GetNameOfClass() << std::endl;
    std::cout << "Parameters = " << this->Superclass::GetParameters() << std::endl;
  }
};

} // namespace Statistics
} // namespace itk

int
itkProbabilityDistributionTest(int, char *[])
{
  std::cout << "itkProbabilityDistributionTest Test \n \n";

  using DistributionType = itk::Statistics::ProbabilityDistributionTestingHelper;

  auto distributionFunction = DistributionType::New();

  std::cout << "GetNameOfClass() = " << distributionFunction->GetNameOfClass() << std::endl;
  std::cout << "HasMean()        = " << distributionFunction->HasMean() << std::endl;
  std::cout << "HasVariance()    = " << distributionFunction->HasVariance() << std::endl;
  std::cout << "Number of parameters = " << distributionFunction->GetNumberOfParameters() << std::endl;

  distributionFunction->Print(std::cout);

  distributionFunction->RunTests();

  return EXIT_SUCCESS;
}
