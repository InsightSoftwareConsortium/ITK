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
#include "itkPointSetToPointSetMetricv4.h"

#include "itkAffineTransform.h"
#include "itkDisplacementFieldTransform.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

/**
 *  \class RegistrationParameterScalesFromPhysicalShiftPointSetTestMetric for test.
 *  Create a simple metric to use for testing here.
 */
template<typename TFixedPointSet, typename TMovingPointSet>
class RegistrationParameterScalesFromPhysicalShiftPointSetTestMetric:
  public itk::PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesFromPhysicalShiftPointSetTestMetric    Self;
  typedef itk::PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>  Superclass;
  typedef itk::SmartPointer< Self >                                         Pointer;
  typedef itk::SmartPointer< const Self >                                   ConstPointer;

  typedef typename Superclass::MeasureType          MeasureType;
  typedef typename Superclass::DerivativeType       DerivativeType;
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;
  typedef typename Superclass::PointType            PointType;
  typedef typename Superclass::PixelType            PixelType;
  typedef typename Superclass::LocalDerivativeType  LocalDerivativeType;
  typedef typename Superclass::FixedPointSetType    FixedPointSetType;
  typedef typename Superclass::MovingPointSetType   MovingPointSetType;

  itkTypeMacro(RegistrationParameterScalesFromPhysicalShiftPointSetTestMetric, PointSetToPointSetMetricv4);

  itkNewMacro(Self);

  virtual MeasureType GetLocalNeighborhoodValue( const PointType &, const PixelType & ) const ITK_OVERRIDE
  { return 1.0; }

  virtual void GetLocalNeighborhoodValueAndDerivative( const PointType &, MeasureType & measure, LocalDerivativeType & derivative, const PixelType & ) const ITK_OVERRIDE
  { measure = 1.0; derivative.Fill(0.0); }

private:

  RegistrationParameterScalesFromPhysicalShiftPointSetTestMetric() {}
  ~RegistrationParameterScalesFromPhysicalShiftPointSetTestMetric() ITK_OVERRIDE {}

};

/**
 */
int itkRegistrationParameterScalesFromPhysicalShiftPointSetTest(int , char* [])
{
  const itk::SizeValueType    Dimension = 2;
  typedef double              PixelType;
  typedef double              FloatType;

  // PointSets
  typedef itk::PointSet<PixelType, Dimension> PointSetType;
  typedef PointSetType::PointType             PointType;

  PointSetType::Pointer fixedPoints = PointSetType::New();
  fixedPoints->Initialize();
  PointSetType::Pointer movingPoints = PointSetType::New();
  movingPoints->Initialize();

  itk::Size<Dimension> virtualDomainSize;
  virtualDomainSize[0] = static_cast<itk::SizeValueType>(10);
  virtualDomainSize[1] = static_cast<itk::SizeValueType>(10);

  PointType upperRightPoint;
  upperRightPoint[0] = virtualDomainSize[0];
  upperRightPoint[1] = virtualDomainSize[1];

  // Make a simple point set
  PointType testPoint;
  PointType::VectorType offset;
  offset.Fill(0.1);
  testPoint[0] = 0.0;
  testPoint[1] = 0.0;
  fixedPoints->SetPoint(0, testPoint);
  movingPoints->SetPoint(0, testPoint + offset);
  testPoint[0] = upperRightPoint[0];
  testPoint[1] = upperRightPoint[1];
  fixedPoints->SetPoint(1, testPoint);
  movingPoints->SetPoint(1, testPoint + offset);
  testPoint[0] =  upperRightPoint[0] / 2.0;
  testPoint[1] =  upperRightPoint[1] / 2.0;
  fixedPoints->SetPoint(2, testPoint);
  movingPoints->SetPoint(2, testPoint + offset);

  // Transforms
  typedef itk::AffineTransform<double, Dimension>      MovingTransformType;
  MovingTransformType::Pointer movingTransform =  MovingTransformType::New();
  movingTransform->SetIdentity();

  typedef itk::TranslationTransform<double, Dimension> FixedTransformType;
  FixedTransformType::Pointer fixedTransform =    FixedTransformType::New();
  fixedTransform->SetIdentity();

  // Metric
  typedef RegistrationParameterScalesFromPhysicalShiftPointSetTestMetric <PointSetType, PointSetType>   MetricType;
  MetricType::Pointer metric = MetricType::New();

  metric->SetFixedPointSet( fixedPoints );
  metric->SetMovingPointSet( movingPoints );
  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );

  //
  // Testing RegistrationParameterScalesFromPhysicalShift
  //
  typedef itk::RegistrationParameterScalesFromPhysicalShift< MetricType >  RegistrationParameterScalesFromPhysicalShiftType;
  RegistrationParameterScalesFromPhysicalShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromPhysicalShiftType::New();

  shiftScaleEstimator->SetMetric(metric);
  shiftScaleEstimator->SetTransformForward(true); //by default, scales for the moving transform
  // must set the virtual domain point set
  shiftScaleEstimator->SetVirtualDomainPointSet( metric->GetVirtualTransformedPointSet() );

  RegistrationParameterScalesFromPhysicalShiftType::ScalesType movingScales(movingTransform->GetNumberOfParameters());
  shiftScaleEstimator->EstimateScales(movingScales);
  std::cout << "Shift scales for the affine transform = " << movingScales << std::endl;

  // determine truth
  RegistrationParameterScalesFromPhysicalShiftType::ScalesType theoreticalMovingScales(movingTransform->GetNumberOfParameters());
  itk::SizeValueType param = 0;
  for (itk::SizeValueType row = 0; row < Dimension; row++)
    {
    for (itk::SizeValueType col = 0; col < Dimension; col++)
      {
      theoreticalMovingScales[param++] = upperRightPoint[col] * upperRightPoint[col];
      }
    }
  for (itk::SizeValueType row = 0; row < Dimension; row++)
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
    std::cerr << "Failed: the shift scales for the affine transform do not match theoretical scales: " << theoreticalMovingScales << std::endl;
    }
  else
    {
    std::cout << "Passed: the shift scales for the affine transform are correct." << std::endl;
    }

  bool nonUniformForAffine = false;
  for (itk::SizeValueType p = 1; p < movingScales.GetSize(); p++)
    {
    if ( itk::Math::NotExactlyEquals(movingScales[p], movingScales[0]) )
      {
      nonUniformForAffine = true;
      break;
      }
    }
  if (!nonUniformForAffine)
    {
    std::cerr << "Error: the shift scales for an affine transform are equal for all parameters." << std::endl;
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
  for (itk::SizeValueType row = 0; row < Dimension; row++)
    {
    theoreticalStepScale += upperRightPoint[row] * upperRightPoint[row];
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
    std::cerr << "Failed: the step scale for the affine transform is not correct." << std::endl;
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
    std::cerr << "Failed: the shift scales for the translation transform are not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the shift scales for the translation transform are correct." << std::endl;
    }

  bool uniformForTranslation = true;
  for (itk::SizeValueType p = 1; p < fixedScales.GetSize(); p++)
    {
    if (std::abs( fixedScales[p] - fixedScales[0] ) > 1e-6 )
      {
      uniformForTranslation = false;
      std::cerr << "fixedScales[" << p << "] - fixedScales[0]: " << fixedScales[p] - fixedScales[0] << std::endl;
      break;
      }
    }
  if (!uniformForTranslation)
    {
    std::cerr << "Error: the shift scales for a translation transform are not equal for all parameters." << std::endl;
    }

  //
  // Testing local scales for a transform with local support, ex. DisplacementFieldTransform
  //
  typedef itk::DisplacementFieldTransform<double, Dimension>  DisplacementTransformType;
  typedef DisplacementTransformType::DisplacementFieldType    FieldType;
  typedef itk::Vector<double, Dimension>                      VectorType;

  VectorType zero;
  zero.Fill(0.0);

  typedef itk::ImageRegion<Dimension> RegionType;
  RegionType region;
  region.SetSize( virtualDomainSize );
  RegionType::IndexType index;
  index.Fill(0);
  region.SetIndex( index );

  FieldType::Pointer field = FieldType::New();
  field->SetRegions( region );
  double spacing[2] = {1.0, 1.0};
  field->SetSpacing( spacing );
  double origin[2] = {0.0, 0.0};
  field->SetOrigin( origin );
  field->Allocate();
  field->FillBuffer(zero);

  DisplacementTransformType::Pointer displacementTransform = DisplacementTransformType::New();
  displacementTransform->SetDisplacementField(field);
  metric->SetMovingTransform( displacementTransform );
  // We must initialize the metric so it will create a virtual domain from the displacement field.
  metric->Initialize();

  // Estimate scales
  shiftScaleEstimator->SetTransformForward(true);
  RegistrationParameterScalesFromPhysicalShiftType::ScalesType localScales;
  shiftScaleEstimator->EstimateScales( localScales );
  std::cout << "Shift scales for the displacement field transform = " << localScales << std::endl;

  // Check the correctness
  RegistrationParameterScalesFromPhysicalShiftType::ScalesType theoreticalLocalScales( displacementTransform->GetNumberOfLocalParameters() );
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
    std::cerr << "Failed: the shift scales for the displacement field transform are not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the shift scales for the displacement field transform are correct." << std::endl;
    }

  //
  // Testing the step scale for the displacement field transform
  //
  DisplacementTransformType::ParametersType displacementStep( displacementTransform->GetNumberOfParameters() );
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
    std::cerr << "Failed: the step scale for the displacement field transform is not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the step scale for the displacement field transform is correct." << std::endl;
    }

  //
  // Test that not setting a virtual domain point set for sampling fill fail
  //
  std::cout << "Test without setting virtual domain point set." << std::endl;
  RegistrationParameterScalesFromPhysicalShiftType::Pointer shiftScaleEstimator2 = RegistrationParameterScalesFromPhysicalShiftType::New();
  shiftScaleEstimator2->SetMetric(metric);
  TRY_EXPECT_EXCEPTION( shiftScaleEstimator2->EstimateStepScale(displacementStep) );

  //
  // Check the correctness of all cases above
  //
  std::cout << std::endl;
  if (affinePass && nonUniformForAffine && stepScalePass && translationPass && uniformForTranslation && displacementPass && localStepScalePass )
    {
    std::cout << "Test passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
}
