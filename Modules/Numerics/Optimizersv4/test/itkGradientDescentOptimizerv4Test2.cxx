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
#include "itkGradientDescentOptimizerv4.h"

/* This test simulates the use of a metric with a transform
 * with local support, testing the proper handling of scales for such a case.
 *
 * Cribbed originally from itkGradientDescentOptimizerTest */

/**
 *  \class GradientDescentOptimizerv4Test2Metric for test
 *
 *  The version for this test returns a derivative that simulates
 *  the return from a metric working with a transform with local support.
 *  The derivative does not change, it's only meant to test the mechanics
 *  of applying scales in one iteration of optimization.
 *
 */
class GradientDescentOptimizerv4Test2Metric : public itk::ObjectToObjectMetricBase
{
public:
  using Self = GradientDescentOptimizerv4Test2Metric;
  using Superclass = itk::ObjectToObjectMetricBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(GradientDescentOptimizerv4Test2Metric, ObjectToObjectMetricBase);

  enum
  {
    SpaceDimension = 3
  };

  using ParametersType = Superclass::ParametersType;
  using ParametersValueType = Superclass::ParametersValueType;
  using NumberOfParametersType = Superclass::NumberOfParametersType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  GradientDescentOptimizerv4Test2Metric()
  {
    m_Parameters.SetSize(this->GetNumberOfParameters());
    m_Parameters.Fill(0);
  }

  void
  Initialize() override
  {}

  void
  GetDerivative(DerivativeType & derivative) const override
  {
    MeasureType value;
    GetValueAndDerivative(value, derivative);
  }

  void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override
  {
    if (derivative.Size() != this->GetNumberOfParameters())
    {
      derivative.SetSize(this->GetNumberOfParameters());
    }

    value = 0.0;

    for (NumberOfParametersType i = 0; i < this->GetNumberOfParameters(); ++i)
    {
      derivative[i] = i;
    }

    std::cout << "derivative: " << derivative << std::endl;
  }

  MeasureType
  GetValue() const override
  {
    return 0.0;
  }

  void
  UpdateTransformParameters(const DerivativeType & update, ParametersValueType) override
  {
    m_Parameters += update;
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension * 3;
  }

  bool
  HasLocalSupport() const override
  {
    return false;
  }

  unsigned int
  GetNumberOfLocalParameters() const override
  {
    return SpaceDimension;
  }

  /* These Set/Get methods are only needed for this test derivation that
   * isn't using a transform */
  void
  SetParameters(ParametersType & parameters) override
  {
    m_Parameters = parameters;
  }

  const ParametersType &
  GetParameters() const override
  {
    return m_Parameters;
  }

private:
  ParametersType m_Parameters;
};

///////////////////////////////////////////////////////////
int
itkGradientDescentOptimizerv4Test2(int, char *[])
{
  std::cout << "Gradient Descent Object Optimizer Test ";
  std::cout << std::endl << std::endl;

  using OptimizerType = itk::GradientDescentOptimizerv4;

  using ScalesType = OptimizerType::ScalesType;

  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  // Declaration of the Metric
  auto metric = GradientDescentOptimizerv4Test2Metric::New();

  itkOptimizer->SetMetric(metric);

  using ParametersType = GradientDescentOptimizerv4Test2Metric::ParametersType;
  using NumberOfParametersType = GradientDescentOptimizerv4Test2Metric::NumberOfParametersType;

  ParametersType initialPosition(metric->GetNumberOfParameters());
  initialPosition.Fill(0);
  metric->SetParameters(initialPosition);

  itkOptimizer->SetLearningRate(1.0);
  itkOptimizer->SetNumberOfIterations(1);

  ScalesType scales(metric->GetNumberOfLocalParameters());
  for (NumberOfParametersType i = 0; i < metric->GetNumberOfLocalParameters(); ++i)
  {
    scales[i] = i + 2;
  }
  itkOptimizer->SetScales(scales);

  ParametersType         truth(metric->GetNumberOfParameters());
  NumberOfParametersType numLocal = metric->GetNumberOfLocalParameters();
  NumberOfParametersType numLoops = metric->GetNumberOfParameters() / numLocal;
  for (NumberOfParametersType i = 0; i < numLoops; ++i)
  {
    for (NumberOfParametersType j = 0; j < numLocal; ++j)
    {
      NumberOfParametersType ind = i * numLocal + j;
      truth[ind] = initialPosition[ind] + (ind) / scales[j];
    }
  }
  std::cout << "truth: " << truth << std::endl;

  try
  {
    itkOptimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation() << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
  }

  ParametersType finalPosition = metric->GetParameters();
  std::cout << "finalPosition = " << finalPosition << std::endl;

  //
  // check results to see if it is within range
  //
  for (NumberOfParametersType j = 0; j < metric->GetNumberOfParameters(); ++j)
  {
    if (itk::Math::abs(finalPosition[j] - truth[j]) > 0.000001)
    {
      std::cerr << "Results do not match: " << std::endl
                << "expected: " << truth << std::endl
                << "actual: " << finalPosition << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
