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
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkImageToImageMetricv4.h"

#include "itkAffineTransform.h"
#include "itkDisplacementFieldTransform.h"
#include "itkMath.h"

/**
 *  \class RegistrationParameterScalesFromPhysicalShiftTestMetric for test.
 *  Create a simple metric to use for testing here.
 */
template< typename TFixedImage,typename TMovingImage,typename TVirtualImage = TFixedImage >
class RegistrationParameterScalesFromPhysicalShiftTestMetric:
  public itk::ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesFromPhysicalShiftTestMetric              Self;
  typedef itk::ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage> Superclass;
  typedef itk::SmartPointer< Self >                                           Pointer;
  typedef itk::SmartPointer< const Self >                                     ConstPointer;

  typedef typename Superclass::MeasureType          MeasureType;
  typedef typename Superclass::DerivativeType       DerivativeType;
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  itkTypeMacro(RegistrationParameterScalesFromPhysicalShiftTestMetric, ImageToImageMetricv4);

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

  RegistrationParameterScalesFromPhysicalShiftTestMetric() {}
  ~RegistrationParameterScalesFromPhysicalShiftTestMetric() ITK_OVERRIDE {}

};

/**
 */
int itkRegistrationParameterScalesFromPhysicalShiftTest(int , char* [])
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

  // Transforms
  typedef itk::AffineTransform<double, ImageDimension>      MovingTransformType;
  MovingTransformType::Pointer movingTransform =  MovingTransformType::New();
  movingTransform->SetIdentity();

  typedef itk::TranslationTransform<double, ImageDimension> FixedTransformType;
  FixedTransformType::Pointer fixedTransform =    FixedTransformType::New();
  fixedTransform->SetIdentity();

  // Metric
  typedef RegistrationParameterScalesFromPhysicalShiftTestMetric
    <FixedImageType, MovingImageType>   MetricType;
  MetricType::Pointer metric = MetricType::New();

  metric->SetVirtualDomainFromImage( virtualImage );
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );

  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );

  //
  // Testing RegistrationParameterScalesFromPhysicalShift
  //
  typedef itk::RegistrationParameterScalesFromPhysicalShift< MetricType >  RegistrationParameterScalesFromPhysicalShiftType;
  RegistrationParameterScalesFromPhysicalShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromPhysicalShiftType::New();

  shiftScaleEstimator->SetMetric(metric);
  shiftScaleEstimator->SetTransformForward(true); //by default, scales for the moving transform
  shiftScaleEstimator->Print( std::cout );
  std::cout << std::endl;

  RegistrationParameterScalesFromPhysicalShiftType::ScalesType movingScales(movingTransform->GetNumberOfParameters());
  shiftScaleEstimator->EstimateScales(movingScales);
  std::cout << "Shift scales for the affine transform = " << movingScales << std::endl;

  // determine truth
  RegistrationParameterScalesFromPhysicalShiftType::ScalesType theoreticalMovingScales( movingTransform->GetNumberOfParameters() );
  VirtualImageType::PointType upperPoint;
  virtualImage->TransformIndexToPhysicalPoint( virtualImage->GetLargestPossibleRegion().GetUpperIndex(), upperPoint );

  itk::SizeValueType param = 0;
  for (itk::SizeValueType row = 0; row < ImageDimension; row++)
    {
    for (itk::SizeValueType col = 0; col < ImageDimension; col++)
      {
      theoreticalMovingScales[param++] = upperPoint[col] * upperPoint[col];
      }
    }
  for (itk::SizeValueType row = 0; row < ImageDimension; row++)
    {
    theoreticalMovingScales[param++] = 1;
    }

  // compare test to truth
  bool affinePass = true;
  for (itk::SizeValueType p = 0; p < theoreticalMovingScales.GetSize(); p++)
    {
    if (std::abs((movingScales[p] - theoreticalMovingScales[p]) / theoreticalMovingScales[p]) > 0.01 )
      {
      affinePass = false;
      break;
      }
    }
  if (!affinePass)
    {
    std::cout << "Failed: the shift scales for the affine transform are not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the shift scales for the affine transform are correct." << std::endl;
    }

  bool nonUniformForAffine = false;
  for (itk::SizeValueType p = 1; p < movingScales.GetSize(); p++)
    {
    if (itk::Math::NotExactlyEquals(movingScales[p], movingScales[0]))
      {
      nonUniformForAffine = true;
      break;
      }
    }
  if (!nonUniformForAffine)
    {
    std::cout << "Error: the shift scales for an affine transform are equal for all parameters." << std::endl;
    }

  //
  // Testing the step scale
  //
  MovingTransformType::ParametersType movingStep(movingTransform->GetNumberOfParameters());
  movingStep = movingTransform->GetParameters(); //the step is an identity transform
  FloatType stepScale = shiftScaleEstimator->EstimateStepScale(movingStep);
  std::cout << "The step scale of shift for the affine transform = " << stepScale << std::endl;
  FloatType learningRate = 1.0 / stepScale;
  std::cout << "The learning rate of shift for the affine transform = " << learningRate << std::endl;

  // compute truth
  FloatType theoreticalStepScale = 0.0;
  for (itk::SizeValueType row = 0; row < ImageDimension; row++)
    {
    theoreticalStepScale += upperPoint[row] * upperPoint[row];
    }
  theoreticalStepScale = std::sqrt(theoreticalStepScale);

  // compare truth and test
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

  //
  // Scales for the fixed transform
  //
  shiftScaleEstimator->SetTransformForward(false);
  RegistrationParameterScalesFromPhysicalShiftType::ScalesType fixedScales( fixedTransform->GetNumberOfParameters() );
  shiftScaleEstimator->EstimateScales(fixedScales);
  std::cout << "Shift scales for the translation transform = " << fixedScales << std::endl;

  // Check the correctness
  RegistrationParameterScalesFromPhysicalShiftType::ScalesType theoreticalFixedScales( fixedTransform->GetNumberOfParameters() );
  theoreticalFixedScales.Fill(1.0);

  bool translationPass = true;
  for (itk::SizeValueType p = 0; p < theoreticalFixedScales.GetSize(); p++)
    {
    if (std::abs((fixedScales[p] - theoreticalFixedScales[p]) / theoreticalFixedScales[p]) > 0.01 )
      {
      translationPass = false;
      break;
      }
    }
  if (!translationPass)
    {
    std::cout << "Failed: the shift scales for the translation transform are not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the shift scales for the translation transform are correct." << std::endl;
    }

  bool uniformForTranslation = true;
  for (itk::SizeValueType p = 1; p < fixedScales.GetSize(); p++)
    {
    if (itk::Math::NotExactlyEquals(fixedScales[p], fixedScales[0]))
      {
      uniformForTranslation = false;
      break;
      }
    }
  if (!uniformForTranslation)
    {
    std::cout << "Error: the shift scales for a translation transform are not equal for all parameters." << std::endl;
    }

  //
  // Testing local scales for a transform with local support, ex. DisplacementFieldTransform
  //
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
  shiftScaleEstimator->SetTransformForward(true);
  RegistrationParameterScalesFromPhysicalShiftType::ScalesType localScales;
  shiftScaleEstimator->EstimateScales(localScales);
  std::cout << "Shift scales for the displacement field transform = " << localScales << std::endl;

  // Check the correctness
  RegistrationParameterScalesFromPhysicalShiftType::ScalesType theoreticalLocalScales(
    displacementTransform->GetNumberOfLocalParameters());
  theoreticalLocalScales.Fill(1.0);

  bool displacementPass = true;
  for (itk::SizeValueType p = 0; p < theoreticalLocalScales.GetSize(); p++)
    {
    if (std::abs((localScales[p] - theoreticalLocalScales[p]) / theoreticalLocalScales[p]) > 0.01 )
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

  //
  // Testing the step scale for the displacement field transform
  //
  DisplacementTransformType::ParametersType displacementStep(displacementTransform->GetNumberOfParameters());
  displacementStep.Fill(1.0);
  FloatType localStepScale = shiftScaleEstimator->EstimateStepScale(displacementStep);
  std::cout << "The step scale of shift for the displacement field transform = " << localStepScale << std::endl;
  FloatType localLearningRate = 1.0 / localStepScale;
  std::cout << "The learning rate of shift for the displacement field transform = " << localLearningRate << std::endl;

  bool localStepScalePass = false;
  FloatType theoreticalLocalStepScale = std::sqrt(2.0);
  if (std::abs( (localStepScale - theoreticalLocalStepScale) /theoreticalLocalStepScale ) < 0.01)
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

  //
  // Check the correctness of all cases above
  //
  std::cout << std::endl;
  if (affinePass && nonUniformForAffine && stepScalePass && displacementPass && localStepScalePass && translationPass && uniformForTranslation )
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
