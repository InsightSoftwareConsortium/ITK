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
#include "itkObjectToObjectOptimizerBase.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

/* Create a simple metric to use for testing here. */
template <typename TFixedObject, typename TMovingObject>
class ObjectToObjectOptimizerBaseTestMetric : public itk::ObjectToObjectMetricBase
{
public:
  /** Standard class type aliases. */
  using Self = ObjectToObjectOptimizerBaseTestMetric;
  using Superclass = itk::ObjectToObjectMetricBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  using typename Superclass::MeasureType;
  using typename Superclass::DerivativeType;
  using typename Superclass::ParametersType;
  using typename Superclass::ParametersValueType;

  itkTypeMacro(ObjectToObjectOptimizerBaseTestMetric, ObjectToObjectMetricBase);

  itkNewMacro(Self);

  // Pure virtual functions that all Metrics must provide
  unsigned int
  GetNumberOfParameters() const override
  {
    return 5;
  }

  MeasureType
  GetValue() const override
  {
    return 1.0;
  }

  void
  GetDerivative(DerivativeType & derivative) const override
  {
    derivative.Fill(0.0);
  }

  bool
  HasLocalSupport() const override
  {
    return false;
  }

  void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override
  {
    value = 1.0;
    derivative.Fill(0.0);
  }

  unsigned int
  GetNumberOfLocalParameters() const override
  {
    return 3;
  }

  void
  UpdateTransformParameters(const DerivativeType &, ParametersValueType) override
  {}

  const ParametersType &
  GetParameters() const override
  {
    return m_Parameters;
  }

  void
  SetParameters(ParametersType &) override
  {}

  void
  Initialize() override
  {}

  void
  PrintSelf(std::ostream & os, itk::Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }

  ParametersType m_Parameters;

private:
  ObjectToObjectOptimizerBaseTestMetric() = default;
  ~ObjectToObjectOptimizerBaseTestMetric() override = default;
};

/* Define a simple derived optimizer class.
 * \class ObjectToObjectOptimizerBaseTestOptimizer */
class ObjectToObjectOptimizerBaseTestOptimizer : public itk::ObjectToObjectOptimizerBase
{
public:
  /** Standard "Self" type alias. */
  using Self = ObjectToObjectOptimizerBaseTestOptimizer;
  using Superclass = itk::ObjectToObjectOptimizerBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectToObjectOptimizerBaseTestOptimizer, ObjectToObjectOptimizerBase);

  /* Provide initialization for this class */
  void
  StartOptimization(bool doOnlyInitialization = false) override
  {
    Superclass::StartOptimization(doOnlyInitialization);
    std::cout << "StartOptimization called from derived class. doOnlyInitialization: " << doOnlyInitialization
              << std::endl;
  }

  /** Stop condition return string type */
  const StopConditionReturnStringType
  GetStopConditionDescription() const override
  {
    return std::string("Placeholder test return string");
  }
};

/**
 */
int
itkObjectToObjectOptimizerBaseTest(int, char *[])
{
  constexpr int ImageDimension = 2;
  using ImageType = itk::Image<double, ImageDimension>;

  using MetricType = ObjectToObjectOptimizerBaseTestMetric<ImageType, ImageType>;

  auto metric = MetricType::New();
  auto optimizer = ObjectToObjectOptimizerBaseTestOptimizer::New();

  if (optimizer->GetStopConditionDescription() != std::string("Placeholder test return string"))
  {
    std::cerr << "GetStopConditionDescription did not return properly" << std::endl;
    return EXIT_FAILURE;
  }
  /* exercise some methods */
  optimizer->SetMetric(metric);
  if (optimizer->GetMetric() != metric)
  {
    std::cerr << "Set/GetMetric failed." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "value: " << optimizer->GetCurrentMetricValue() << std::endl;

  /* Test set/get of scales */
  ObjectToObjectOptimizerBaseTestOptimizer::NumberOfParametersType scalesSize = metric->GetNumberOfLocalParameters();
  using ScalesType = ObjectToObjectOptimizerBaseTestOptimizer::ScalesType;
  ScalesType scales(scalesSize);
  scales.Fill(3.19);
  optimizer->SetScales(scales);
  const ScalesType & scalesReturn = optimizer->GetScales();
  if (scalesReturn != scales)
  {
    std::cerr << "Set/GetScales failed." << std::endl;
    return EXIT_FAILURE;
  }

  optimizer->SetNumberOfWorkUnits(1);

  /* Test StartOptimization */
  ITK_TRY_EXPECT_NO_EXCEPTION(optimizer->StartOptimization());

  /* Test with incorrectly-sized scales. Expect exception */
  scales.SetSize(scalesSize + 1);
  optimizer->SetScales(scales);
  ITK_TRY_EXPECT_EXCEPTION(optimizer->StartOptimization());

  /* Test with scales close to identity, within tolerance.
   * The flag indicating identity scales should be set. */
  scales.SetSize(scalesSize);
  scales.Fill(0.999);
  optimizer->SetScales(scales);
  ITK_TRY_EXPECT_NO_EXCEPTION(optimizer->StartOptimization());
  if (!optimizer->GetScalesAreIdentity())
  {
    std::cerr << "Expected GetScalesAreIdentity to return true." << std::endl;
    return EXIT_FAILURE;
  }

  /* Test that weights are init'ed by default to identity */
  ObjectToObjectOptimizerBaseTestOptimizer::NumberOfParametersType weightsSize = metric->GetNumberOfLocalParameters();
  ITK_TRY_EXPECT_NO_EXCEPTION(optimizer->StartOptimization());
  ScalesType weightsReturn = optimizer->GetWeights();
  if (weightsReturn.Size() != 0 || !optimizer->GetWeightsAreIdentity())
  {
    std::cerr << "Expected returned weights to be empty, and flag set to idenity. But got: " << weightsReturn
              << ", GetWeightsAreIdentity: " << optimizer->GetWeightsAreIdentity() << std::endl;
    return EXIT_FAILURE;
  }

  /* Test set/get of weights */
  ScalesType weights(weightsSize);
  weights.Fill(3.19);
  optimizer->SetWeights(weights);
  weightsReturn = optimizer->GetWeights();
  if (weightsReturn != weights)
  {
    std::cerr << "Set/GetWeights failed." << std::endl;
    return EXIT_FAILURE;
  }

  /* Test with incorrectly-sized weights. Expect exception */
  weights.SetSize(weightsSize + 1);
  optimizer->SetWeights(weights);
  ITK_TRY_EXPECT_EXCEPTION(optimizer->StartOptimization());

  /* Test with weights close to identity, within tolerance.
   * The flag indicating identity weights should be set. */
  weights.SetSize(weightsSize);
  weights.Fill(0.99999);
  optimizer->SetWeights(weights);
  ITK_TRY_EXPECT_NO_EXCEPTION(optimizer->StartOptimization());
  if (!optimizer->GetWeightsAreIdentity())
  {
    std::cerr << "Expected GetWeightsAreIdentity to return true." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Printing self.." << std::endl;
  std::cout << optimizer << std::endl;

  // Test streaming enumeration for ObjectToObjectOptimizerBaseTemplateEnums::StopConditionObjectToObjectOptimizer
  // elements
  const std::set<itk::ObjectToObjectOptimizerBaseTemplateEnums::StopConditionObjectToObjectOptimizer>
    allStopConditionObjectToObjectOptimizer{
      itk::ObjectToObjectOptimizerBaseTemplateEnums::StopConditionObjectToObjectOptimizer::MAXIMUM_NUMBER_OF_ITERATIONS,
      itk::ObjectToObjectOptimizerBaseTemplateEnums::StopConditionObjectToObjectOptimizer::COSTFUNCTION_ERROR,
      itk::ObjectToObjectOptimizerBaseTemplateEnums::StopConditionObjectToObjectOptimizer::UPDATE_PARAMETERS_ERROR,
      itk::ObjectToObjectOptimizerBaseTemplateEnums::StopConditionObjectToObjectOptimizer::STEP_TOO_SMALL,
      itk::ObjectToObjectOptimizerBaseTemplateEnums::StopConditionObjectToObjectOptimizer::CONVERGENCE_CHECKER_PASSED,
      itk::ObjectToObjectOptimizerBaseTemplateEnums::StopConditionObjectToObjectOptimizer::
        GRADIENT_MAGNITUDE_TOLEARANCE,
      itk::ObjectToObjectOptimizerBaseTemplateEnums::StopConditionObjectToObjectOptimizer::OTHER_ERROR
    };
  for (const auto & ee : allStopConditionObjectToObjectOptimizer)
  {
    std::cout << "STREAMED ENUM VALUE ObjectToObjectOptimizerBaseTemplateEnums::StopConditionObjectToObjectOptimizer: "
              << ee << std::endl;
  }

  return EXIT_SUCCESS;
}
