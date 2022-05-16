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
#include "itkOptimizerParameterScalesEstimator.h"

/**
 *  \class OptimizerParameterScalesEstimatorTest for test.
 *  Create a simple scales estimator class to use for testing here.
 */
class OptimizerParameterScalesEstimatorTest : public itk::OptimizerParameterScalesEstimator
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(OptimizerParameterScalesEstimatorTest);

  /** Standard class type aliases. */
  using Self = OptimizerParameterScalesEstimatorTest;
  using Superclass = itk::OptimizerParameterScalesEstimator;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  itkNewMacro(Self);

  itkTypeMacro(OptimizerParameterScalesEstimatorTest, OptimizerParameterScalesEstimator);

  /** Estimate parameter scales */
  void
  EstimateScales(ScalesType & scales) override
  {
    scales.SetSize(2);
    scales.Fill(1.0);
  }

  double
  EstimateStepScale(const ParametersType & step) override
  {
    double norm = step.two_norm();
    return norm;
  }

  /** Estimate the scales of local steps. */
  void
  EstimateLocalStepScales(const ParametersType & step, ScalesType & localStepScales) override
  {
    localStepScales.SetSize(step.size());
  }

  /** Estimate the trusted scale for steps. */
  double
  EstimateMaximumStepSize() override
  {
    return 1.0;
  }

protected:
  OptimizerParameterScalesEstimatorTest() = default;
  ~OptimizerParameterScalesEstimatorTest() override = default;
};

/**
 * The test program for OptimizerParameterScalesEstimator.
 */
int
itkOptimizerParameterScalesEstimatorTest(int, char *[])
{
  auto scalesEstimator = OptimizerParameterScalesEstimatorTest::New();

  OptimizerParameterScalesEstimatorTest::ScalesType scales;
  scalesEstimator->Print(std::cout);

  scalesEstimator->EstimateScales(scales);

  std::cout << "Scales estimated: " << scales << std::endl;
  std::cout << "Test passed" << std::endl;

  return EXIT_SUCCESS;
}
