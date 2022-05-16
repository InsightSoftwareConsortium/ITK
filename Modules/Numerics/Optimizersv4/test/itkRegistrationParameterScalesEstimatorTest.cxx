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
#include "itkRegistrationParameterScalesEstimator.h"
#include "itkImageToImageMetricv4.h"

#include "itkAffineTransform.h"

/**
 *  \class RegistrationParameterScalesEstimatorTestMetric for test.
 *  Create a simple metric to use for testing here.
 */
template <
  typename TFixedImage,
  typename TMovingImage,
  typename TVirtualImage = TFixedImage,
  typename TInternalComputationValueType = double,
  typename TMetricTraits =
    itk::DefaultImageToImageMetricTraitsv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType>>
class RegistrationParameterScalesEstimatorTestMetric
  : public itk::
      ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
{
public:
  /** Standard class type aliases. */
  using Self = RegistrationParameterScalesEstimatorTestMetric;
  using Superclass =
    itk::ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  using typename Superclass::MeasureType;
  using typename Superclass::DerivativeType;
  using typename Superclass::ParametersType;
  using typename Superclass::ParametersValueType;

  itkTypeMacro(RegistrationParameterScalesEstimatorTestMetric, ImageToImageMetricv4);

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
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override
  {
    value = 1.0;
    derivative.Fill(0.0);
  }

  unsigned int
  GetNumberOfLocalParameters() const override
  {
    return 0;
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
  Initialize() override
  {}

  void
  PrintSelf(std::ostream & os, itk::Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }

  ParametersType m_Parameters;

  // Image related types
  using FixedImageType = TFixedImage;
  using MovingImageType = TMovingImage;
  using VirtualImageType = TVirtualImage;

  using FixedImageConstPointer = typename FixedImageType::ConstPointer;
  using MovingImageConstPointer = typename MovingImageType::ConstPointer;
  using VirtualImagePointer = typename VirtualImageType::Pointer;
  using VirtualRegionType = typename VirtualImageType::RegionType;

  /* Image dimension accessors */
  static constexpr itk::SizeValueType FixedImageDimension = FixedImageType::ImageDimension;
  static constexpr itk::SizeValueType MovingImageDimension = MovingImageType::ImageDimension;
  static constexpr itk::SizeValueType VirtualImageDimension = VirtualImageType::ImageDimension;

private:
  RegistrationParameterScalesEstimatorTestMetric() = default;
  ~RegistrationParameterScalesEstimatorTestMetric() override = default;
};

/**
 *  \class RegistrationParameterScalesEstimatorTest for test.
 *  Create a simple scales estimator class to use for testing here.
 */
template <typename TMetric>
class RegistrationParameterScalesEstimatorTest : public itk::RegistrationParameterScalesEstimator<TMetric>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegistrationParameterScalesEstimatorTest);

  /** Standard class type aliases. */
  using Self = RegistrationParameterScalesEstimatorTest;
  using Superclass = itk::RegistrationParameterScalesEstimator<TMetric>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  itkNewMacro(Self);

  itkTypeMacro(RegistrationParameterScalesEstimatorTest, RegistrationParameterScalesEstimator);

  /** Type of scales */
  using typename Superclass::ScalesType;
  /** Type of parameters of the optimizer */
  using typename Superclass::ParametersType;
  /** Type of float */
  using typename Superclass::FloatType;

  using typename Superclass::VirtualPointType;
  using typename Superclass::VirtualIndexType;
  using typename Superclass::MovingTransformType;
  using typename Superclass::FixedTransformType;
  using typename Superclass::JacobianType;
  using typename Superclass::VirtualImageConstPointer;

  /** Estimate parameter scales with maximum squared norms of Jacobians. */
  void
  EstimateScales(ScalesType & parameterScales) override
  {
    this->CheckAndSetInputs();
    this->SetSamplingStrategy(itk::SamplingStrategyEnum::RandomSampling);
    this->SetNumberOfRandomSamples(1000);
    this->SampleVirtualDomain();

    itk::SizeValueType numPara = this->GetTransform()->GetNumberOfParameters();
    parameterScales.SetSize(numPara);

    ParametersType norms(numPara);

    auto numSamples = static_cast<itk::SizeValueType>(this->m_SamplePoints.size());

    norms.Fill(0.0);
    parameterScales.Fill(1.0);

    // checking each sample point
    for (itk::SizeValueType c = 0; c < numSamples; ++c)
    {
      VirtualPointType point = this->m_SamplePoints[c];

      ParametersType squaredNorms(numPara);
      this->ComputeSquaredJacobianNorms(point, squaredNorms);

      for (itk::SizeValueType p = 0; p < numPara; ++p)
      {
        if (norms[p] < squaredNorms[p])
        {
          norms[p] = squaredNorms[p];
        }
      }
    } // for numSamples

    if (numSamples > 0)
    {
      for (itk::SizeValueType p = 0; p < numPara; ++p)
      {
        parameterScales[p] = norms[p];
      }
    }
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

protected:
  RegistrationParameterScalesEstimatorTest() = default;
  ~RegistrationParameterScalesEstimatorTest() override = default;
};

/**
 */
int
itkRegistrationParameterScalesEstimatorTest(int, char *[])
{

  // Image begins
  constexpr itk::SizeValueType ImageDimension = 2;
  using PixelType = double;

  // Image Types
  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;
  using VirtualImageType = itk::Image<PixelType, ImageDimension>;

  auto                      fixedImage = FixedImageType::New();
  auto                      movingImage = MovingImageType::New();
  VirtualImageType::Pointer virtualImage = fixedImage;

  MovingImageType::SizeType size;
  size.Fill(100);

  movingImage->SetRegions(size);
  fixedImage->SetRegions(size);
  // Image done

  // Transform begins
  using MovingTransformType = itk::AffineTransform<double, ImageDimension>;
  auto movingTransform = MovingTransformType::New();
  movingTransform->SetIdentity();

  using FixedTransformType = itk::TranslationTransform<double, ImageDimension>;
  auto fixedTransform = FixedTransformType::New();
  fixedTransform->SetIdentity();
  // Transform done

  // Metric begins
  using MetricType = RegistrationParameterScalesEstimatorTestMetric<FixedImageType, MovingImageType>;
  auto metric = MetricType::New();

  metric->SetVirtualDomainFromImage(virtualImage);
  metric->SetFixedImage(fixedImage);
  metric->SetMovingImage(movingImage);

  metric->SetFixedTransform(fixedTransform);
  metric->SetMovingTransform(movingTransform);
  // Metric done

  // Scales for the affine transform from max squared norm of transform jacobians
  using RegistrationParameterScalesEstimatorTestType = RegistrationParameterScalesEstimatorTest<MetricType>;
  RegistrationParameterScalesEstimatorTestType::Pointer jacobianScaleEstimator =
    RegistrationParameterScalesEstimatorTestType::New();

  jacobianScaleEstimator->SetMetric(metric);
  jacobianScaleEstimator->SetTransformForward(true);
  jacobianScaleEstimator->Print(std::cout);

  RegistrationParameterScalesEstimatorTestType::ScalesType jacobianScales(movingTransform->GetNumberOfParameters());
  jacobianScaleEstimator->EstimateScales(jacobianScales);
  std::cout << "Scales from max squared Jacobian norm for the affine transform = " << jacobianScales << std::endl;

  // Check the correctness
  RegistrationParameterScalesEstimatorTestType::ScalesType theoreticalJacobianScales(
    movingTransform->GetNumberOfParameters());
  VirtualImageType::PointType upperPoint;
  virtualImage->TransformIndexToPhysicalPoint(virtualImage->GetLargestPossibleRegion().GetUpperIndex(), upperPoint);

  itk::SizeValueType param = 0;
  for (itk::SizeValueType row = 0; row < ImageDimension; ++row)
  {
    for (itk::SizeValueType col = 0; col < ImageDimension; ++col)
    {
      // max squared jacobian norms
      theoreticalJacobianScales[param++] = upperPoint[col] * upperPoint[col];
    }
  }
  for (itk::SizeValueType row = 0; row < ImageDimension; ++row)
  {
    theoreticalJacobianScales[param++] = 1;
  }

  bool jacobianPass = true;
  for (itk::SizeValueType p = 0; p < jacobianScales.GetSize(); ++p)
  {
    if (itk::Math::NotAlmostEquals(jacobianScales[p], theoreticalJacobianScales[p]))
    {
      jacobianPass = false;
      break;
    }
  }
  bool nonUniformForJacobian = false;
  for (itk::SizeValueType p = 1; p < jacobianScales.GetSize(); ++p)
  {
    if (itk::Math::NotAlmostEquals(jacobianScales[p], jacobianScales[0]))
    {
      nonUniformForJacobian = true;
      break;
    }
  }
  // Check done

  jacobianScaleEstimator->EstimateScales(jacobianScales);
  bool randomPass = true;
  for (itk::SizeValueType p = 0; p < jacobianScales.GetSize(); ++p)
  {
    if (itk::Math::abs((jacobianScales[p] - theoreticalJacobianScales[p]) / theoreticalJacobianScales[p]) > 0.3)
    {
      randomPass = false;
      break;
    }
  }
  jacobianScaleEstimator->EstimateScales(jacobianScales);
  bool fullDomainPass = true;
  for (itk::SizeValueType p = 0; p < jacobianScales.GetSize(); ++p)
  {
    if (itk::Math::NotAlmostEquals(jacobianScales[p], theoreticalJacobianScales[p]))
    {
      fullDomainPass = false;
      break;
    }
  }

  // Testing RegistrationParameterScalesEstimatorTest done
  std::cout << std::endl;

  if (!jacobianPass)
  {
    std::cout << "Failed: the jacobian scales for the affine transform are not correct." << std::endl;
  }
  else
  {
    std::cout << "Passed: the jacobian scales for the affine transform are correct." << std::endl;
  }

  if (!randomPass)
  {
    std::cout << "Failed: the jacobian scales with random sampling are not correct." << std::endl;
  }
  else
  {
    std::cout << "Passed: the jacobian scales with random sampling are correct." << std::endl;
  }

  if (!fullDomainPass)
  {
    std::cout << "Failed: the jacobian scales from checking the full domain are not correct." << std::endl;
  }
  else
  {
    std::cout << "Passed: the jacobian scales from checking the full domain are correct." << std::endl;
  }

  if (!nonUniformForJacobian)
  {
    std::cout << "Error: the jacobian scales for an affine transform are equal for all parameters." << std::endl;
  }

  // Test streaming enumeration for RegistrationParameterScalesEstimatorEnums::SamplingStrategy elements
  const std::set<itk::RegistrationParameterScalesEstimatorEnums::SamplingStrategy> allSamplingStrategy{
    itk::RegistrationParameterScalesEstimatorEnums::SamplingStrategy::FullDomainSampling,
    itk::RegistrationParameterScalesEstimatorEnums::SamplingStrategy::CornerSampling,
    itk::RegistrationParameterScalesEstimatorEnums::SamplingStrategy::RandomSampling,
    itk::RegistrationParameterScalesEstimatorEnums::SamplingStrategy::CentralRegionSampling,
    itk::RegistrationParameterScalesEstimatorEnums::SamplingStrategy::VirtualDomainPointSetSampling
  };
  for (const auto & ee : allSamplingStrategy)
  {
    std::cout << "STREAMED ENUM VALUE RegistrationParameterScalesEstimatorEnums::SamplingStrategy: " << ee << std::endl;
  }

  if (jacobianPass && nonUniformForJacobian && randomPass && fullDomainPass)
  {
    std::cout << "Test passed" << std::endl;
    return EXIT_SUCCESS;
  }
  else
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }
}
