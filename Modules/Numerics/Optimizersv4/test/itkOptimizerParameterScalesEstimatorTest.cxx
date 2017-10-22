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
#include "itkOptimizerParameterScalesEstimator.h"

/**
 *  \class OptimizerParameterScalesEstimatorTest for test.
 *  Create a simple scales estimator class to use for testing here.
 */
class OptimizerParameterScalesEstimatorTest:
  public itk::OptimizerParameterScalesEstimator
{
public:
  /** Standard class typedefs. */
  typedef OptimizerParameterScalesEstimatorTest                   Self;
  typedef itk::OptimizerParameterScalesEstimator                  Superclass;
  typedef itk::SmartPointer< Self >                               Pointer;
  typedef itk::SmartPointer< const Self >                         ConstPointer;

  itkNewMacro(Self);

  itkTypeMacro(OptimizerParameterScalesEstimatorTest, OptimizerParameterScalesEstimator);

  /** Estimate parameter scales */
  virtual void EstimateScales(ScalesType &scales) ITK_OVERRIDE
    {
    scales.SetSize(2);
    scales.Fill(1.0);
    }

  virtual double EstimateStepScale(const ParametersType &step) ITK_OVERRIDE
    {
    double norm = step.two_norm();
    return norm;
    }

  /** Estimate the scales of local steps. */
  virtual void EstimateLocalStepScales(const ParametersType &step,
    ScalesType &localStepScales) ITK_OVERRIDE
    {
    localStepScales.SetSize(step.size());
    }

  /** Estimate the trusted scale for steps. */
  virtual double EstimateMaximumStepSize() ITK_OVERRIDE
    {
    return 1.0;
    }

protected:
  OptimizerParameterScalesEstimatorTest(){};
  ~OptimizerParameterScalesEstimatorTest() ITK_OVERRIDE {};

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(OptimizerParameterScalesEstimatorTest);

};

/**
 * The test program for OptimizerParameterScalesEstimator.
 */
int itkOptimizerParameterScalesEstimatorTest(int , char* [])
{
  OptimizerParameterScalesEstimatorTest::Pointer scalesEstimator =
    OptimizerParameterScalesEstimatorTest::New();

  OptimizerParameterScalesEstimatorTest::ScalesType scales;
  scalesEstimator->Print( std::cout );

  scalesEstimator->EstimateScales(scales);

  std::cout << "Scales estimated: " << scales << std::endl;
  std::cout << "Test passed" << std::endl;

  return EXIT_SUCCESS;
}
