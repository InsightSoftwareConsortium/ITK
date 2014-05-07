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

#include "itkProbabilityDistribution.h"

namespace itk {
namespace Statistics {

class ProbabilityDistributionTestingHelper : public ProbabilityDistribution
{
public:
  typedef ProbabilityDistributionTestingHelper     Self;
  typedef ProbabilityDistribution                  Superclass;
  typedef SmartPointer<Self>                       Pointer;
  typedef SmartPointer<const Self>                 ConstPointer;

  itkTypeMacro(ProbabilityDistributionTestingHelper, ProbabilityDistribution);

  itkNewMacro(Self);

  virtual SizeValueType GetNumberOfParameters() const ITK_OVERRIDE { return 42; }
  virtual double EvaluatePDF(double ) const ITK_OVERRIDE { return 42.0; }
  virtual double EvaluatePDF(double , const ParametersType&) const ITK_OVERRIDE { return 42.0; }
  virtual double EvaluateCDF(double ) const ITK_OVERRIDE { return 42.0; }
  virtual double EvaluateCDF(double , const ParametersType&) const ITK_OVERRIDE { return 42.0; }
  virtual double EvaluateInverseCDF(double ) const ITK_OVERRIDE  { return 42.0; }
  virtual double EvaluateInverseCDF(double , const ParametersType&) const ITK_OVERRIDE  { return 42.0; }
  virtual bool HasMean() const ITK_OVERRIDE { return true; }
  virtual bool HasVariance() const ITK_OVERRIDE { return true; }
  virtual double GetMean() const ITK_OVERRIDE { return 42.0; }
  virtual double GetVariance() const ITK_OVERRIDE { return 42.0; }

  void RunTests()
    {
    std::cout << "Superclass name = " << this->Superclass::GetNameOfClass() << std::endl;
    std::cout << "Parameters = " << this->Superclass::GetParameters() << std::endl;
    }
};

}
}

int itkProbabilityDistributionTest(int, char* [] )
{
  std::cout << "itkProbabilityDistributionTest Test \n \n";

  typedef itk::Statistics::ProbabilityDistributionTestingHelper DistributionType;

  DistributionType::Pointer distributionFunction = DistributionType::New();

  std::cout << "GetNameOfClass() = " << distributionFunction->GetNameOfClass() << std::endl;
  std::cout << "HasMean()        = " << distributionFunction->HasMean() << std::endl;
  std::cout << "HasVariance()    = " << distributionFunction->HasVariance() << std::endl;
  std::cout << "Number of parameters = " << distributionFunction->GetNumberOfParameters() << std::endl;

  distributionFunction->Print( std::cout );

  distributionFunction->RunTests();

  return EXIT_SUCCESS;
}
