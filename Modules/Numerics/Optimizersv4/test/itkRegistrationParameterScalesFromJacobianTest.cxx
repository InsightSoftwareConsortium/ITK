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
#include "itkRegistrationParameterScalesFromJacobian.h"
#include "itkImageToImageMetricv4.h"

#include "itkAffineTransform.h"
#include "itkDisplacementFieldTransform.h"
#include "itkMath.h"

/**
 *  \class RegistrationParameterScalesFromJacobianTestMetric for test.
 *  Create a simple metric to use for testing here.
 */
template< typename TFixedImage,typename TMovingImage,typename TVirtualImage = TFixedImage >
class RegistrationParameterScalesFromJacobianTestMetric:
  public itk::ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesFromJacobianTestMetric       Self;
  typedef itk::ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
                                                                  Superclass;
  typedef itk::SmartPointer< Self >                               Pointer;
  typedef itk::SmartPointer< const Self >                         ConstPointer;

  typedef typename Superclass::MeasureType          MeasureType;
  typedef typename Superclass::DerivativeType       DerivativeType;
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  itkTypeMacro(RegistrationParameterScalesFromJacobianTestMetric, ImageToImageMetricv4);

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

  RegistrationParameterScalesFromJacobianTestMetric() {}
  ~RegistrationParameterScalesFromJacobianTestMetric() ITK_OVERRIDE {}

};

/**
 */
int itkRegistrationParameterScalesFromJacobianTest(int , char* [])
{

  // Image begins
  const itk::SizeValueType    ImageDimension = 2;
  typedef double              PixelType;
  typedef double              FloatType;

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
  typedef RegistrationParameterScalesFromJacobianTestMetric
    <FixedImageType, MovingImageType>   MetricType;
  MetricType::Pointer metric = MetricType::New();

  metric->SetVirtualDomainFromImage( virtualImage );
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );

  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );
  // Metric done

  // Scales for the affine transform from transform jacobians
  typedef itk::RegistrationParameterScalesFromJacobian< MetricType > RegistrationParameterScalesFromJacobianType;
  RegistrationParameterScalesFromJacobianType::Pointer jacobianScaleEstimator = RegistrationParameterScalesFromJacobianType::New();

  jacobianScaleEstimator->SetMetric(metric);
  jacobianScaleEstimator->SetTransformForward(true); //by default
  jacobianScaleEstimator->Print( std::cout );
  std::cout << std::endl;

  RegistrationParameterScalesFromJacobianType::ScalesType jacobianScales( movingTransform->GetNumberOfParameters() );
  jacobianScaleEstimator->EstimateScales(jacobianScales);
  std::cout << "Jacobian scales for the affine transform = " << jacobianScales << std::endl;

  // Check the correctness
  RegistrationParameterScalesFromJacobianType::ScalesType theoreticalJacobianScales( movingTransform->GetNumberOfParameters() );
  VirtualImageType::PointType upperPoint;
  virtualImage->TransformIndexToPhysicalPoint( virtualImage->GetLargestPossibleRegion().GetUpperIndex(), upperPoint );

  itk::SizeValueType param = 0;
  for (itk::SizeValueType row = 0; row < ImageDimension; row++)
    {
    for (itk::SizeValueType col = 0; col < ImageDimension; col++)
      {
      // uses the corners for affine transform
      // = (0 + 0 + n*n + n*n)/4 = n*n/2
      theoreticalJacobianScales[param++] = upperPoint[col] * upperPoint[col] / 2.0;
      }
    }
  for (itk::SizeValueType row = 0; row < ImageDimension; row++)
    {
    theoreticalJacobianScales[param++] = 1;
    }

  bool jacobianPass = true;
  for (itk::SizeValueType p = 0; p < jacobianScales.GetSize(); p++)
    {
    if (std::abs((jacobianScales[p] - theoreticalJacobianScales[p])
      / theoreticalJacobianScales[p]) > 0.01 )
      {
      jacobianPass = false;
      break;
      }
    }
  if (!jacobianPass)
    {
    std::cout << "Failed: the jacobian scales for the affine transform are not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the jacobian scales for the affine transform are correct." << std::endl;
    }

  bool nonUniformForJacobian = false;
  for (itk::SizeValueType p = 1; p < jacobianScales.GetSize(); p++)
    {
    if (itk::Math::NotExactlyEquals(jacobianScales[p], jacobianScales[0]))
      {
      nonUniformForJacobian = true;
      break;
      }
    }
  if (!nonUniformForJacobian)
    {
    std::cout << "Error: the jacobian scales for an affine transform are equal for all parameters." << std::endl;
    }

  // Testing the step scale for the affine transform
  MovingTransformType::ParametersType movingStep(movingTransform->GetNumberOfParameters());
  movingStep = movingTransform->GetParameters();
  FloatType stepScale = jacobianScaleEstimator->EstimateStepScale(movingStep);
  std::cout << "The step scale of Jacobian for the affine transform = " << stepScale << std::endl;
  FloatType learningRate = 1.0 / stepScale;
  std::cout << "The learning rate of Jacobian for the affine transform = " << learningRate << std::endl;

  FloatType theoreticalStepScale = 0.0;
  FloatType count = 0.0;
  VirtualImageType::PointType lowerPoint;
  virtualImage->TransformIndexToPhysicalPoint(virtualImage->
    GetLargestPossibleRegion().GetIndex(), lowerPoint);

  for (FloatType x=lowerPoint[0]; x<=upperPoint[0]; x+=upperPoint[0]-lowerPoint[0])
    {
    for (FloatType y=lowerPoint[1]; y<=upperPoint[1]; y+=upperPoint[1]-lowerPoint[1])
      {
      theoreticalStepScale += std::sqrt(x*x + y*y);
      count++;
      }
    }
  theoreticalStepScale /= count;

  bool stepScalePass = false;
  if (std::abs( (stepScale - theoreticalStepScale)/theoreticalStepScale ) < 0.01)
    {
    stepScalePass = true;
    }
  if (!stepScalePass)
    {
    std::cout << "Failed: the step scale for the affine transform is not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the step scale for the affine transform is correct." << std::endl;
    }

  // Testing local scales for a transform with local support, ex. DisplacementFieldTransform
  typedef itk::DisplacementFieldTransform<double, ImageDimension>
                                                            DisplacementTransformType;
  typedef DisplacementTransformType::DisplacementFieldType  FieldType;
  typedef itk::Vector<double, ImageDimension>               VectorType;

  VectorType zero;
  zero.Fill(0.0);

  FieldType::Pointer field = FieldType::New();
  field->SetRegions(virtualImage->GetLargestPossibleRegion());
  field->SetSpacing(virtualImage->GetSpacing());
  field->SetOrigin(virtualImage->GetOrigin());
  field->SetDirection(virtualImage->GetDirection());
  field->Allocate();
  field->FillBuffer(zero);

  DisplacementTransformType::Pointer displacementTransform = DisplacementTransformType::New();
  displacementTransform->SetDisplacementField(field);

  metric->SetMovingTransform( displacementTransform );
  jacobianScaleEstimator->SetTransformForward(true);
  RegistrationParameterScalesFromJacobianType::ScalesType localScales;
  jacobianScaleEstimator->EstimateScales(localScales);
  std::cout << "Shift scales for the displacement field transform = "
    << localScales << std::endl;

  // Check the correctness
  RegistrationParameterScalesFromJacobianType::ScalesType theoreticalLocalScales(
    displacementTransform->GetNumberOfLocalParameters());
  theoreticalLocalScales.Fill(1.0);

  bool displacementPass = true;
  for (itk::SizeValueType p = 0; p < theoreticalLocalScales.GetSize(); p++)
    {
    if (std::abs((localScales[p] - theoreticalLocalScales[p])
      / theoreticalLocalScales[p]) > 0.01 )
      {
      displacementPass = false;
      break;
      }
    }
  if (!displacementPass)
    {
    std::cout << "Failed: the shift scales for the displacement field transform are not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the shift scales for the displacement field transform are correct." << std::endl;
    }
  // Testing scales with local support done

  // Testing the step scale for the displacement field transform
  DisplacementTransformType::ParametersType displacementStep(displacementTransform->GetNumberOfParameters());
  displacementStep.Fill(1.0);
  FloatType localStepScale = jacobianScaleEstimator->EstimateStepScale(displacementStep);
  std::cout << "The step scale of Jacobian for the displacement field transform = " << localStepScale << std::endl;
  FloatType localLearningRate = 1.0 / localStepScale;
  std::cout << "The learning rate of Jacobian for the displacement field transform = " << localLearningRate << std::endl;

  bool localStepScalePass = false;
  FloatType theoreticalLocalStepScale = std::sqrt(2.0);
  if (std::abs( (localStepScale - theoreticalLocalStepScale)
    /theoreticalLocalStepScale ) < 0.01)
    {
    localStepScalePass = true;
    }
  if (!localStepScalePass)
    {
    std::cout << "Failed: the step scale for the displacement field transform is not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the step scale for the displacement field transform is correct." << std::endl;
    }
  // Testing the step scale with local support done

  // Check the correctness of all cases above
  std::cout << std::endl;
  if (jacobianPass && nonUniformForJacobian
    && stepScalePass && displacementPass && localStepScalePass)
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
