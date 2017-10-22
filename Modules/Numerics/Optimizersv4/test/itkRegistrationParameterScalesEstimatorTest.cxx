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
#include "itkRegistrationParameterScalesEstimator.h"
#include "itkImageToImageMetricv4.h"

#include "itkAffineTransform.h"

/**
 *  \class RegistrationParameterScalesEstimatorTestMetric for test.
 *  Create a simple metric to use for testing here.
 */
template< typename TFixedImage,typename TMovingImage,typename TVirtualImage = TFixedImage,
          typename TInternalComputationValueType = double,
          typename TMetricTraits = itk::DefaultImageToImageMetricTraitsv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType>
          >
class RegistrationParameterScalesEstimatorTestMetric:
  public itk::ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesEstimatorTestMetric                  Self;
  typedef itk::ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage,
                             TInternalComputationValueType,TMetricTraits> Superclass;
  typedef itk::SmartPointer< Self >                                       Pointer;
  typedef itk::SmartPointer< const Self >                                 ConstPointer;

  typedef typename Superclass::MeasureType          MeasureType;
  typedef typename Superclass::DerivativeType       DerivativeType;
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  itkTypeMacro(RegistrationParameterScalesEstimatorTestMetric, ImageToImageMetricv4);

  itkNewMacro(Self);

  // Pure virtual functions that all Metrics must provide
  unsigned int GetNumberOfParameters() const ITK_OVERRIDE { return 5; }

  MeasureType GetValue() const ITK_OVERRIDE
    {
    return 1.0;
    }

  void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const ITK_OVERRIDE
    {
    value = 1.0;
    derivative.Fill(0.0);
    }

  unsigned int GetNumberOfLocalParameters() const ITK_OVERRIDE
  { return 0; }

  void UpdateTransformParameters( const DerivativeType &, ParametersValueType ) ITK_OVERRIDE {}

  const ParametersType & GetParameters() const ITK_OVERRIDE
  { return m_Parameters; }

  void Initialize(void) throw ( itk::ExceptionObject ) ITK_OVERRIDE {}

  void PrintSelf(std::ostream& os, itk::Indent indent) const ITK_OVERRIDE
  { Superclass::PrintSelf( os, indent ); }

  ParametersType  m_Parameters;

  // Image related types
  typedef TFixedImage                             FixedImageType;
  typedef TMovingImage                            MovingImageType;
  typedef TVirtualImage                           VirtualImageType;

  typedef typename FixedImageType::ConstPointer   FixedImageConstPointer;
  typedef typename MovingImageType::ConstPointer  MovingImageConstPointer;
  typedef typename VirtualImageType::Pointer      VirtualImagePointer;
  typedef typename VirtualImageType::RegionType   VirtualRegionType;

  /* Image dimension accessors */
  itkStaticConstMacro(FixedImageDimension, itk::SizeValueType,
      FixedImageType::ImageDimension);
  itkStaticConstMacro(MovingImageDimension, itk::SizeValueType,
      MovingImageType::ImageDimension);
  itkStaticConstMacro(VirtualImageDimension, itk::SizeValueType,
      VirtualImageType::ImageDimension);

private:

  RegistrationParameterScalesEstimatorTestMetric() {}
  ~RegistrationParameterScalesEstimatorTestMetric() ITK_OVERRIDE {}

};

/**
 *  \class RegistrationParameterScalesEstimatorTest for test.
 *  Create a simple scales estimator class to use for testing here.
 */
template < typename TMetric >
class RegistrationParameterScalesEstimatorTest:
  public itk::RegistrationParameterScalesEstimator< TMetric >
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesEstimatorTest                    Self;
  typedef itk::RegistrationParameterScalesEstimator< TMetric >        Superclass;
  typedef itk::SmartPointer< Self >                                   Pointer;
  typedef itk::SmartPointer< const Self >                             ConstPointer;

  itkNewMacro(Self);

  itkTypeMacro(RegistrationParameterScalesEstimatorTest, RegistrationParameterScalesEstimator);

  /** Type of scales */
  typedef typename Superclass::ScalesType                ScalesType;
  /** Type of parameters of the optimizer */
  typedef typename Superclass::ParametersType            ParametersType;
  /** Type of float */
  typedef typename Superclass::FloatType                 FloatType;

  typedef typename Superclass::VirtualPointType          VirtualPointType;
  typedef typename Superclass::VirtualIndexType          VirtualIndexType;
  typedef typename Superclass::MovingTransformType       MovingTransformType;
  typedef typename Superclass::FixedTransformType        FixedTransformType;
  typedef typename Superclass::JacobianType              JacobianType;
  typedef typename Superclass::VirtualImageConstPointer  VirtualImageConstPointer;

  /** Estimate parameter scales with maximum squared norms of Jacobians. */
  virtual void EstimateScales(ScalesType &parameterScales) ITK_OVERRIDE
    {
    this->CheckAndSetInputs();
    this->SetSamplingStrategy( Superclass::RandomSampling );
    this->SetNumberOfRandomSamples( 1000 );
    this->SampleVirtualDomain();

    itk::SizeValueType numPara = this->GetTransform()->GetNumberOfParameters();
    parameterScales.SetSize(numPara);

    ParametersType norms(numPara);

    itk::SizeValueType numSamples = static_cast<itk::SizeValueType>( this->m_SamplePoints.size() );

    norms.Fill(0.0);
    parameterScales.Fill(1.0);

    // checking each sample point
    for (itk::SizeValueType c=0; c<numSamples; c++)
      {
      VirtualPointType point = this->m_SamplePoints[c];

      ParametersType squaredNorms(numPara);
      this->ComputeSquaredJacobianNorms( point, squaredNorms );

      for (itk::SizeValueType p=0; p<numPara; p++)
        {
        if (norms[p] < squaredNorms[p])
          {
          norms[p] = squaredNorms[p];
          }
        }
      } //for numSamples

    if (numSamples > 0)
      {
      for (itk::SizeValueType p=0; p<numPara; p++)
        {
        parameterScales[p] = norms[p];
        }
      }
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

protected:
  RegistrationParameterScalesEstimatorTest(){};
  ~RegistrationParameterScalesEstimatorTest() ITK_OVERRIDE {};

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegistrationParameterScalesEstimatorTest);

};

/**
 */
int itkRegistrationParameterScalesEstimatorTest(int , char* [])
{

  // Image begins
  const itk::SizeValueType ImageDimension = 2;
  typedef double           PixelType;

  // Image Types
  typedef itk::Image<PixelType,ImageDimension>           FixedImageType;
  typedef itk::Image<PixelType,ImageDimension>           MovingImageType;
  typedef itk::Image<PixelType,ImageDimension>           VirtualImageType;

  FixedImageType::Pointer  fixedImage  = FixedImageType::New();
  MovingImageType::Pointer movingImage = MovingImageType::New();
  VirtualImageType::Pointer virtualImage = fixedImage;

  MovingImageType::SizeType    size;
  size.Fill(100);

  movingImage->SetRegions( size );
  fixedImage->SetRegions( size );
  // Image done

  // Transform begins
  typedef itk::AffineTransform<double, ImageDimension>      MovingTransformType;
  MovingTransformType::Pointer movingTransform =  MovingTransformType::New();
  movingTransform->SetIdentity();

  typedef itk::TranslationTransform<double, ImageDimension> FixedTransformType;
  FixedTransformType::Pointer fixedTransform =    FixedTransformType::New();
  fixedTransform->SetIdentity();
  // Transform done

  // Metric begins
  typedef RegistrationParameterScalesEstimatorTestMetric
    <FixedImageType, MovingImageType> MetricType;
  MetricType::Pointer metric = MetricType::New();

  metric->SetVirtualDomainFromImage( virtualImage );
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );

  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );
  // Metric done

  // Scales for the affine transform from max squared norm of transform jacobians
  typedef RegistrationParameterScalesEstimatorTest< MetricType >
    RegistrationParameterScalesEstimatorTestType;
  RegistrationParameterScalesEstimatorTestType::Pointer jacobianScaleEstimator
    = RegistrationParameterScalesEstimatorTestType::New();

  jacobianScaleEstimator->SetMetric(metric);
  jacobianScaleEstimator->SetTransformForward(true);
  jacobianScaleEstimator->Print( std::cout );

  RegistrationParameterScalesEstimatorTestType::ScalesType jacobianScales( movingTransform->GetNumberOfParameters());
  jacobianScaleEstimator->EstimateScales(jacobianScales);
  std::cout << "Scales from max squared Jacobian norm for the affine transform = "
    << jacobianScales << std::endl;

  // Check the correctness
  RegistrationParameterScalesEstimatorTestType::ScalesType theoreticalJacobianScales( movingTransform->GetNumberOfParameters());
  VirtualImageType::PointType upperPoint;
  virtualImage->TransformIndexToPhysicalPoint(virtualImage->GetLargestPossibleRegion().GetUpperIndex(), upperPoint);

  itk::SizeValueType param = 0;
  for (itk::SizeValueType row = 0; row < ImageDimension; row++)
    {
    for (itk::SizeValueType col = 0; col < ImageDimension; col++)
      {
      // max squared jacobian norms
      theoreticalJacobianScales[param++] = upperPoint[col] * upperPoint[col];
      }
    }
  for (itk::SizeValueType row = 0; row < ImageDimension; row++)
    {
    theoreticalJacobianScales[param++] = 1;
    }

  bool jacobianPass = true;
  for (itk::SizeValueType p = 0; p < jacobianScales.GetSize(); p++)
    {
    if ( itk::Math::NotAlmostEquals(jacobianScales[p], theoreticalJacobianScales[p]) )
      {
      jacobianPass = false;
      break;
      }
    }
  bool nonUniformForJacobian = false;
  for (itk::SizeValueType p = 1; p < jacobianScales.GetSize(); p++)
    {
    if ( itk::Math::NotAlmostEquals(jacobianScales[p], jacobianScales[0]) )
      {
      nonUniformForJacobian = true;
      break;
      }
    }
  // Check done

  jacobianScaleEstimator->EstimateScales(jacobianScales);
  bool randomPass = true;
  for (itk::SizeValueType p = 0; p < jacobianScales.GetSize(); p++)
    {
    if (std::abs( (jacobianScales[p] - theoreticalJacobianScales[p])
      / theoreticalJacobianScales[p] ) > 0.3 )
      {
      randomPass = false;
      break;
      }
    }
  jacobianScaleEstimator->EstimateScales(jacobianScales);
  bool fullDomainPass = true;
  for (itk::SizeValueType p = 0; p < jacobianScales.GetSize(); p++)
    {
    if ( itk::Math::NotAlmostEquals(jacobianScales[p], theoreticalJacobianScales[p]) )
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
