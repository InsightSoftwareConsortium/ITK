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
#include "itkGradientDescentOptimizerBasev4.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

/* Create a simple metric to use for testing here. */
template <typename TFixedObject, typename TMovingObject>
class GradientDescentOptimizerBasev4TestMetric : public itk::ObjectToObjectMetricBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GradientDescentOptimizerBasev4TestMetric);

  /** Standard class type aliases. */
  using Self = GradientDescentOptimizerBasev4TestMetric;
  using Superclass = itk::ObjectToObjectMetricBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  using typename Superclass::MeasureType;
  using typename Superclass::DerivativeType;
  using typename Superclass::ParametersType;
  using typename Superclass::ParametersValueType;

  itkTypeMacro(GradientDescentOptimizerBasev4TestMetric, ObjectToObjectMetricBase);

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
    return itk::NumericTraits<MeasureType>::OneValue();
  }

  void
  GetDerivative(DerivativeType & derivative) const override
  {
    derivative.Fill(itk::NumericTraits<ParametersValueType>::ZeroValue());
  }

  void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override
  {
    value = itk::NumericTraits<MeasureType>::OneValue();
    derivative.Fill(itk::NumericTraits<ParametersValueType>::ZeroValue());
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

  bool
  HasLocalSupport() const override
  {
    return false;
  }

  void
  Initialize() override
  {}

  void
  PrintSelf(std::ostream & os, itk::Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }

protected:
  ~GradientDescentOptimizerBasev4TestMetric() override = default;

private:
  GradientDescentOptimizerBasev4TestMetric() = default;
  ParametersType m_Parameters;
};

/* Define a simple derived optimizer class.
 * \class GradientDescentOptimizerBasev4TestOptimizer */
class GradientDescentOptimizerBasev4TestOptimizer : public itk::GradientDescentOptimizerBasev4
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GradientDescentOptimizerBasev4TestOptimizer);

  /** Standard "Self" type alias. */
  using Self = GradientDescentOptimizerBasev4TestOptimizer;
  using Superclass = itk::GradientDescentOptimizerBasev4;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientDescentOptimizerBasev4TestOptimizer, GradientDescentOptimizerBasev4);

  /* Provide an override for the pure virtual StartOptimization */
  void
  StartOptimization(bool doOnlyInitialization = false) override
  {
    Superclass::StartOptimization(doOnlyInitialization);
    std::cout << "StartOptimization called. doOnlyInitialization: " << doOnlyInitialization << std::endl;
  }

  void
  ResumeOptimization() override
  {
    std::cout << "ResumeOptimization called." << std::endl;
  }

  void
  ModifyGradientByScalesOverSubRange(const IndexRangeType & index) override
  {
    std::cout << "ModifyGradientByScalesOverSubRange called with index:" << index << std::endl;
  }

  void
  ModifyGradientByLearningRateOverSubRange(const IndexRangeType & index) override
  {
    std::cout << "ModifyGradientByLearningRateOverSubRange called with index:" << index << std::endl;
  }

protected:
  GradientDescentOptimizerBasev4TestOptimizer() = default;
  ~GradientDescentOptimizerBasev4TestOptimizer() override = default;
};


int
itkGradientDescentOptimizerBasev4Test(int, char *[])
{
  constexpr int ImageDimension = 2;
  using ImageType = itk::Image<double, ImageDimension>;

  using MetricType = GradientDescentOptimizerBasev4TestMetric<ImageType, ImageType>;

  auto metric = MetricType::New();
  auto optimizer = GradientDescentOptimizerBasev4TestOptimizer::New();

  /* exercise some methods */
  optimizer->SetMetric(metric);
  if (optimizer->GetMetric() != metric)
  {
    std::cerr << "Set/GetMetric failed." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "value: " << optimizer->GetCurrentMetricValue() << std::endl;

  optimizer->SetNumberOfWorkUnits(2);

  ITK_TRY_EXPECT_NO_EXCEPTION(optimizer->StartOptimization());

  std::cout << "Printing self.." << std::endl;
  std::cout << optimizer << std::endl;

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
