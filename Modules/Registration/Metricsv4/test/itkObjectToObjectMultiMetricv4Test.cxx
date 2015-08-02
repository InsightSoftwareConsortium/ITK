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

#include "itkObjectToObjectMultiMetricv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImage.h"
#include "itkGaussianImageSource.h"
#include "itkShiftScaleImageFilter.h"
#include "itkTestingMacros.h"
#include "itkCompositeTransform.h"
#include "itkEuclideanDistancePointSetToPointSetMetricv4.h"
#include "itkExpectationBasedPointSetToPointSetMetricv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"


/** This test illustrates the use of the MultivariateImageToImageMetric class, which
    takes N metrics and assigns a weight to each metric's result.
 */

const unsigned int ObjectToObjectMultiMetricv4TestDimension = 2;
typedef itk::ObjectToObjectMultiMetricv4<ObjectToObjectMultiMetricv4TestDimension,ObjectToObjectMultiMetricv4TestDimension>
                                                                                    ObjectToObjectMultiMetricv4TestMultiMetricType;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int itkObjectToObjectMultiMetricv4TestEvaluate( ObjectToObjectMultiMetricv4TestMultiMetricType::Pointer & multiVariateMetric, bool useDisplacementTransform )
{
  int testStatus = EXIT_SUCCESS;
  typedef ObjectToObjectMultiMetricv4TestMultiMetricType    MultiMetricType;

  // Setup weights
  MultiMetricType::WeightsArrayType origMetricWeights( multiVariateMetric->GetNumberOfMetrics() );
  MultiMetricType::WeightValueType weightSum = 0;
  for( itk::SizeValueType n = 0; n < multiVariateMetric->GetNumberOfMetrics(); n++ )
    {
    origMetricWeights[n] = static_cast<MultiMetricType::WeightValueType>( n + 1 );
    weightSum += origMetricWeights[n];
    }
  multiVariateMetric->SetMetricWeights( origMetricWeights );

  // Initialize. This initializes all the component metrics.
  std::cout << "Initialize" << std::endl;
  multiVariateMetric->Initialize();

  // Print out metric value and derivative.
  typedef MultiMetricType::MeasureType MeasureType;
  MeasureType measure = 0;
  MultiMetricType::DerivativeType DerivResultOfGetValueAndDerivative;
  std::cout << "GetValueAndDerivative" << std::endl;
  try
    {
    multiVariateMetric->GetValueAndDerivative( measure, DerivResultOfGetValueAndDerivative );
    }
  catch (itk::ExceptionObject& exp)
    {
    std::cerr << "Exception caught during call to GetValueAndDerivative:" << std::endl;
    std::cerr << exp << std::endl;
    testStatus = EXIT_FAILURE;
    }
  std::cout << "Multivariate measure: " << measure << std::endl;
  if( ! useDisplacementTransform )
    {
    std::cout << "  Derivative : " << DerivResultOfGetValueAndDerivative << std::endl << std::endl;
    }

  // Test GetDerivative
  MultiMetricType::DerivativeType ResultOfGetDerivative;
  multiVariateMetric->GetDerivative( ResultOfGetDerivative );
  for( MultiMetricType::NumberOfParametersType p = 0; p < multiVariateMetric->GetNumberOfParameters(); p++ )
    {
    //When accumulation is done accross multiple threads, the accumulations can be done
    //in different orders resulting in slightly different numerical results.
    //The FloatAlmostEqual is used to address the multi-threaded accumulation differences
    if( !itk::Math::FloatAlmostEqual( ResultOfGetDerivative[p], DerivResultOfGetValueAndDerivative[p], 8, 1e-15 ) )
      {
      std::cerr << "Results do not match between GetValueAndDerivative and GetDerivative." << std::endl;
      std::cout << ResultOfGetDerivative << " != " << DerivResultOfGetValueAndDerivative << std::endl;
      std::cout << "DIFF: " << ResultOfGetDerivative - DerivResultOfGetValueAndDerivative << std::endl;
      testStatus = EXIT_FAILURE;
      }
    }

  // Test GetValue method
  MeasureType measure2 = 0;
  std::cout << "GetValue" << std::endl;
  try
    {
    measure2 = multiVariateMetric->GetValue();
    }
  catch (itk::ExceptionObject& exp)
    {
    std::cerr << "Exception caught during call to GetValue:" << std::endl;
    std::cerr << exp << std::endl;
    testStatus = EXIT_FAILURE;
    }
  if( ! itk::Math::FloatAlmostEqual( measure2, measure ) )
    {
    std::cerr << "measure does not match between calls to GetValue and GetValueAndDerivative: "
              << "measure: " << measure << " measure2: " << measure2 << std::endl;
    testStatus = EXIT_FAILURE;
    }

  // Evaluate individually
  MeasureType metricValue = itk::NumericTraits<MeasureType>::ZeroValue();
  MeasureType weightedMetricValue = itk::NumericTraits<MeasureType>::ZeroValue();
  MultiMetricType::DerivativeType metricDerivative;
  MultiMetricType::DerivativeType DerivResultOfGetValueAndDerivativeTruth( multiVariateMetric->GetNumberOfParameters() );
  DerivResultOfGetValueAndDerivativeTruth.Fill( itk::NumericTraits<MultiMetricType::DerivativeValueType>::ZeroValue() );
  MultiMetricType::DerivativeValueType totalMagnitude = itk::NumericTraits<MultiMetricType::DerivativeValueType>::ZeroValue();

  for (itk::SizeValueType i = 0; i < multiVariateMetric->GetNumberOfMetrics(); i++)
    {
    std::cout << "GetValueAndDerivative on component metrics" << std::endl;
    multiVariateMetric->GetMetricQueue()[i]->GetValueAndDerivative( metricValue, metricDerivative );
    std::cout << " Metric " << i << " value : " << metricValue << std::endl;
    if( ! useDisplacementTransform )
      {
      std::cout << " Metric " << i << " derivative : " << metricDerivative << std::endl << std::endl;
      }
    if( ! itk::Math::FloatAlmostEqual( metricValue, multiVariateMetric->GetValueArray()[i]  ) )
      {
      std::cerr << "Individual metric value " << metricValue
                << " does not match that returned from multi-variate metric: " << multiVariateMetric->GetValueArray()[i]
                << std::endl;
      testStatus = EXIT_FAILURE;
      }
    weightedMetricValue += metricValue * origMetricWeights[i] / weightSum;
    for( MultiMetricType::NumberOfParametersType p = 0; p < multiVariateMetric->GetNumberOfParameters(); p++ )
      {
      DerivResultOfGetValueAndDerivativeTruth[p] += metricDerivative[p] * ( origMetricWeights[i] / weightSum ) / metricDerivative.magnitude();
      }
    totalMagnitude += metricDerivative.magnitude();
    }
  totalMagnitude /= multiVariateMetric->GetNumberOfMetrics();
  for( MultiMetricType::NumberOfParametersType p = 0; p < multiVariateMetric->GetNumberOfParameters(); p++ )
    {
    DerivResultOfGetValueAndDerivativeTruth[p] *= totalMagnitude;
    }

  if( std::fabs( weightedMetricValue - multiVariateMetric->GetWeightedValue() ) > 1e-6 )
    {
    std::cerr << "Computed weighted metric value " << weightedMetricValue << " does match returned value "
              << multiVariateMetric->GetWeightedValue() << std::endl;
    testStatus = EXIT_FAILURE;
    }

  for( MultiMetricType::NumberOfParametersType p = 0; p < multiVariateMetric->GetNumberOfParameters(); p++ )
    {
    MultiMetricType::DerivativeValueType tolerance = static_cast<MultiMetricType::DerivativeValueType> (1e-6);
    if( std::fabs(DerivResultOfGetValueAndDerivativeTruth[p] - DerivResultOfGetValueAndDerivative[p]) > tolerance )
      {
      std::cerr << "Error: DerivResultOfGetValueAndDerivative does not match expected result." << std::endl;
      if( useDisplacementTransform )
        {
         std::cerr << "  DerivResultOfGetValueAndDerivative[" << p << "]: " << DerivResultOfGetValueAndDerivative[p] << std::endl
                   << "  DerivResultOfGetValueAndDerivativeTruth[" << p << "]: " << DerivResultOfGetValueAndDerivativeTruth[p] << std::endl;
        }
      else
        {
        std::cerr << "  DerivResultOfGetValueAndDerivative: " << DerivResultOfGetValueAndDerivative << std::endl
                  << "  DerivResultOfGetValueAndDerivativeTruth: " << DerivResultOfGetValueAndDerivativeTruth << std::endl;
        }
      testStatus = EXIT_FAILURE;
      }
    }

  return testStatus;
}

////////////////////////////////////////////////////////////

int itkObjectToObjectMultiMetricv4TestRun(bool useDisplacementTransform )
{
  // Create two simple images
  const unsigned int Dimension = ObjectToObjectMultiMetricv4TestDimension;
  typedef double PixelType;
  typedef double CoordinateRepresentationType;

  // Allocate Images
  typedef itk::Image<PixelType,Dimension> FixedImageType;
  typedef itk::Image<PixelType,Dimension> MovingImageType;

   // Declare Gaussian Sources
  typedef itk::GaussianImageSource< FixedImageType  >  FixedImageSourceType;

  // Note: the following declarations are classical arrays
  FixedImageType::SizeValueType     fixedImageSize[]     = {  100,  100 };
  FixedImageType::SpacingValueType  fixedImageSpacing[]  = { 1.0f, 1.0f };
  FixedImageType::PointValueType    fixedImageOrigin[]   = { 0.0f, 0.0f };
  FixedImageSourceType::Pointer     fixedImageSource     = FixedImageSourceType::New();

  fixedImageSource->SetSize(    fixedImageSize    );
  fixedImageSource->SetOrigin(  fixedImageOrigin  );
  fixedImageSource->SetSpacing( fixedImageSpacing );
  fixedImageSource->SetNormalized( false );
  fixedImageSource->SetScale( 1.0f );
  fixedImageSource->Update();   // Force the filter to run
  FixedImageType::Pointer  fixedImage  = fixedImageSource->GetOutput();

  typedef itk::ShiftScaleImageFilter<FixedImageType, MovingImageType> ShiftScaleFilterType;
  ShiftScaleFilterType::Pointer shiftFilter = ShiftScaleFilterType::New();
  shiftFilter->SetInput( fixedImage );
  shiftFilter->SetShift( 2.0 );
  shiftFilter->Update();
  MovingImageType::Pointer movingImage = shiftFilter->GetOutput();

  // Set up the metric.
  typedef ObjectToObjectMultiMetricv4TestMultiMetricType  MultiMetricType;
  MultiMetricType::Pointer multiVariateMetric = MultiMetricType::New();

  // Instantiate and Add metrics to the queue
  typedef itk::JointHistogramMutualInformationImageToImageMetricv4<FixedImageType,MovingImageType> JointHistorgramMetrictype;
  typedef itk::MeanSquaresImageToImageMetricv4<FixedImageType,MovingImageType>                     MeanSquaresMetricType;
  typedef itk::MattesMutualInformationImageToImageMetricv4 <FixedImageType,MovingImageType>        MattesMutualInformationMetricType;
  typedef itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<FixedImageType,MovingImageType>     ANTSNCMetricType;

  MeanSquaresMetricType::Pointer              m1 = MeanSquaresMetricType::New();
  MattesMutualInformationMetricType::Pointer  m2 = MattesMutualInformationMetricType::New();
  JointHistorgramMetrictype::Pointer          m3 = JointHistorgramMetrictype::New();
  ANTSNCMetricType::Pointer                   m4 = ANTSNCMetricType::New();

  // Set up a transform
  typedef itk::Transform<CoordinateRepresentationType, Dimension, Dimension>  TransformType;
  typedef itk::DisplacementFieldTransform<double, Dimension>                  DisplacementTransformType;
  typedef itk::TranslationTransform<CoordinateRepresentationType,Dimension>   TranslationTransformType;
  TransformType::Pointer transform;

  if( useDisplacementTransform )
    {
    typedef DisplacementTransformType::DisplacementFieldType    FieldType;
    typedef itk::Vector<double, Dimension>                      VectorType;

    VectorType zero;
    zero.Fill(0.0);

    FieldType::Pointer field = FieldType::New();
    field->SetRegions( fixedImage->GetBufferedRegion() );
    field->SetSpacing( fixedImage->GetSpacing() );
    field->SetOrigin(  fixedImage->GetOrigin() );
    field->Allocate();
    field->FillBuffer(zero);

    DisplacementTransformType::Pointer displacementTransform = DisplacementTransformType::New();
    displacementTransform->SetDisplacementField(field);
    transform = displacementTransform;
    }
  else
    {
    TranslationTransformType::Pointer translationTransform = TranslationTransformType::New();
    translationTransform->SetIdentity();
    transform = translationTransform;
    }

  // Plug the images and transform into the metrics
  std::cout << "Setup metrics" << std::endl;
  m1->SetFixedImage(fixedImage);
  m1->SetMovingImage(movingImage);
  m1->SetMovingTransform( transform );
  m2->SetFixedImage(fixedImage);
  m2->SetMovingImage(movingImage);
  m2->SetMovingTransform( transform );
  m3->SetFixedImage(fixedImage);
  m3->SetMovingImage(movingImage);
  m3->SetMovingTransform( transform );
  m4->SetFixedImage(fixedImage);
  m4->SetMovingImage(movingImage);
  m4->SetMovingTransform( transform );

  // Add the component metrics
  std::cout << "Add component metrics" << std::endl;
  multiVariateMetric->AddMetric(m1);
  multiVariateMetric->AddMetric(m2);
  multiVariateMetric->AddMetric(m3);
  multiVariateMetric->AddMetric(m4);

  if( multiVariateMetric->GetMetricQueue()[0] != m1 || multiVariateMetric->GetMetricQueue()[3] != m4 )
    {
    std::cerr << "AddMetric or GetMetricQueue failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Expect return true because all image metrics
  if( multiVariateMetric->SupportsArbitraryVirtualDomainSamples() == false )
    {
    std::cerr << "Expected SupportsArbitraryVirtualDomainSamples() to return false, but got true. " << std::endl;
    return EXIT_FAILURE;
    }

  // Test Set/Get Transform mechanics
  multiVariateMetric->Initialize();
  if( multiVariateMetric->GetMovingTransform() != transform.GetPointer() )
    {
    std::cerr << "Automatic transform assignment failed. transform: " << transform.GetPointer() << " GetMovingTranform: " << multiVariateMetric->GetMovingTransform() << std::endl;
    return EXIT_FAILURE;
    }
  multiVariateMetric->SetMovingTransform( ITK_NULLPTR );
  for( itk::SizeValueType n = 0; n < multiVariateMetric->GetNumberOfMetrics(); n++ )
    {
    if( multiVariateMetric->GetMovingTransform() != ITK_NULLPTR || multiVariateMetric->GetMetricQueue()[n]->GetMovingTransform() != ITK_NULLPTR )
      {
      std::cerr << "Assignment of null transform failed. multiVariateMetric->GetMovingTransform(): " << multiVariateMetric->GetMovingTransform()
                << " multiVariateMetric->GetMetricQueue()[" << n << "]->GetMovingTransform(): "
                << multiVariateMetric->GetMetricQueue()[n]->GetMovingTransform() << std::endl;
      return EXIT_FAILURE;
      }
    }
  multiVariateMetric->SetMovingTransform( transform );
  for( itk::SizeValueType n = 0; n < multiVariateMetric->GetNumberOfMetrics(); n++ )
    {
    if( multiVariateMetric->GetMovingTransform() != transform.GetPointer() ||
        multiVariateMetric->GetMetricQueue()[0]->GetMovingTransform() != transform.GetPointer() )
      {
      std::cerr << "Assignment of transform failed." << std::endl;
      return EXIT_FAILURE;
      }
    }
  if( multiVariateMetric->GetMovingTransform() != transform.GetPointer() )
    {
    std::cerr << "Retrieval of transform failed." << std::endl;
    }

  // Test with images
  std::cout << "*** Test image metrics *** " << std::endl;
  if( itkObjectToObjectMultiMetricv4TestEvaluate( multiVariateMetric, useDisplacementTransform ) != EXIT_SUCCESS )
    {
    return EXIT_FAILURE;
    }

  std::cout << "*** Test with mismatched transforms *** " << std::endl;
  TranslationTransformType::Pointer transform2 = TranslationTransformType::New();
  m4->SetMovingTransform( transform2 );
  TRY_EXPECT_EXCEPTION( multiVariateMetric->Initialize() );
  m4->SetMovingTransform( transform );

  std::cout << "*** Test with proper CompositeTransform ***" << std::endl;
  typedef itk::CompositeTransform<CoordinateRepresentationType,Dimension> CompositeTransformType;
  CompositeTransformType::Pointer compositeTransform = CompositeTransformType::New();
  compositeTransform->AddTransform( transform2 );
  compositeTransform->AddTransform( transform );
  compositeTransform->SetOnlyMostRecentTransformToOptimizeOn();
  m4->SetMovingTransform( compositeTransform );
  if( itkObjectToObjectMultiMetricv4TestEvaluate( multiVariateMetric, useDisplacementTransform ) != EXIT_SUCCESS )
    {
    std::cerr << "Failed with proper CompositeTransform." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "*** Test with CompositeTransform - too many active transforms ***" << std::endl;
  compositeTransform->SetAllTransformsToOptimizeOn();
  TRY_EXPECT_EXCEPTION( multiVariateMetric->Initialize() );

  std::cout << "*** Test with CompositeTransform - one active transform, but wrong one ***" << std::endl;
  compositeTransform->SetAllTransformsToOptimizeOff();
  compositeTransform->SetNthTransformToOptimizeOn( 0 );
  TRY_EXPECT_EXCEPTION( multiVariateMetric->Initialize() );

  // Reset transform
  m4->SetMovingTransform( transform );

  //
  // Test with adding point set metrics
  //
  typedef itk::PointSet<float, Dimension> PointSetType;
  PointSetType::Pointer fixedPoints = PointSetType::New();
  PointSetType::Pointer movingPoints = PointSetType::New();
  fixedPoints->Initialize();
  movingPoints->Initialize();

  PointSetType::PointType point;
  for( itk::SizeValueType n = 0; n < 100; n++ )
    {
    point[0] = n * 1.0;
    point[1] = n * 2.0;
    fixedPoints->SetPoint( n, point );
    point[0] += 0.5;
    point[1] += 0.5;
    movingPoints->SetPoint( n, point );
    }

  typedef itk::ExpectationBasedPointSetToPointSetMetricv4<PointSetType> ExpectationPointSetMetricType;
  typedef itk::EuclideanDistancePointSetToPointSetMetricv4<PointSetType> EuclideanPointSetMetricType;
  ExpectationPointSetMetricType::Pointer expectationPointSetMetric = ExpectationPointSetMetricType::New();
  EuclideanPointSetMetricType::Pointer   euclideanPointSetMetric = EuclideanPointSetMetricType::New();

  expectationPointSetMetric->SetFixedPointSet( fixedPoints );
  expectationPointSetMetric->SetMovingPointSet( movingPoints );
  expectationPointSetMetric->SetMovingTransform( transform );
  euclideanPointSetMetric->SetFixedPointSet( fixedPoints );
  euclideanPointSetMetric->SetMovingPointSet( movingPoints );
  euclideanPointSetMetric->SetMovingTransform( transform );

  multiVariateMetric->AddMetric( expectationPointSetMetric );
  multiVariateMetric->AddMetric( euclideanPointSetMetric );


  // Expect return false because of point set metrics
  if( multiVariateMetric->SupportsArbitraryVirtualDomainSamples() == true )
    {
    std::cerr << "Expected SupportsArbitraryVirtualDomainSamples() to return true, but got false. " << std::endl;
    return EXIT_FAILURE;
    }

  // Test
  std::cout << "*** Test with PointSet metrics and Image metrics *** " << std::endl;
  if( itkObjectToObjectMultiMetricv4TestEvaluate( multiVariateMetric, useDisplacementTransform ) != EXIT_SUCCESS )
    {
    return EXIT_FAILURE;
    }

  //
  // Exercise basic operation with a scales estimator
  //
  typedef itk::RegistrationParameterScalesFromPhysicalShift< MultiMetricType > ScalesEstimatorMultiType;
  ScalesEstimatorMultiType::Pointer shiftScaleEstimator = ScalesEstimatorMultiType::New();
  shiftScaleEstimator->SetMetric(multiVariateMetric);
  // Have to assign virtual domain sampling points when using a point set with scales estimator
  shiftScaleEstimator->SetVirtualDomainPointSet( expectationPointSetMetric->GetVirtualTransformedPointSet() );

  ScalesEstimatorMultiType::ScalesType scales;
  shiftScaleEstimator->EstimateScales( scales );
  std::cout << "Estimated scales: " << scales << std::endl;

  ScalesEstimatorMultiType::FloatType stepScale;
  ScalesEstimatorMultiType::ParametersType step;
  step.SetSize( multiVariateMetric->GetNumberOfParameters() );
  step.Fill( itk::NumericTraits<ScalesEstimatorMultiType::ParametersType::ValueType>::OneValue() );
  stepScale = shiftScaleEstimator->EstimateStepScale( step );
  std::cout << "Estimated stepScale: " << stepScale << std::endl;

  //
  // Test that we get the same scales/step estimation
  // with a single metric and the same metric twice in a multimetric
  //
  ScalesEstimatorMultiType::ScalesType singleScales, multiSingleScales, multiDoubleScales;
  ScalesEstimatorMultiType::FloatType  singleStep, multiSingleStep, multiDoubleStep;
  step.SetSize( m1->GetNumberOfParameters() );
  step.Fill( itk::NumericTraits<ScalesEstimatorMultiType::ParametersType::ValueType>::OneValue() );

  typedef itk::RegistrationParameterScalesFromPhysicalShift<MeanSquaresMetricType> ScalesEstimatorMeanSquaresType;
  ScalesEstimatorMeanSquaresType::Pointer singleShiftScaleEstimator = ScalesEstimatorMeanSquaresType::New();
  singleShiftScaleEstimator->SetMetric(m1);
  m1->Initialize();
  singleShiftScaleEstimator->EstimateScales( singleScales );
  std::cout << "Single metric estimated scales: " << singleScales << std::endl;
  singleStep = singleShiftScaleEstimator->EstimateStepScale( step );
  std::cout << "Single metric estimated stepScale: " << singleStep << std::endl;

  MultiMetricType::Pointer multiSingleMetric = MultiMetricType::New();
  multiSingleMetric->AddMetric( m1 );
  multiSingleMetric->Initialize();
  shiftScaleEstimator->SetMetric( multiSingleMetric );
  shiftScaleEstimator->EstimateScales( multiSingleScales );
  std::cout << "multi-single estimated scales: " << multiSingleScales << std::endl;
  multiSingleStep = shiftScaleEstimator->EstimateStepScale( step );
  std::cout << "multi-single estimated stepScale: " << multiSingleStep << std::endl;

  MultiMetricType::Pointer multiDoubleMetric = MultiMetricType::New();
  multiDoubleMetric->AddMetric( m1 );
  multiDoubleMetric->AddMetric( m1 );
  multiDoubleMetric->Initialize();
  shiftScaleEstimator->SetMetric( multiDoubleMetric );
  shiftScaleEstimator->EstimateScales( multiDoubleScales );
  std::cout << "multi-double estimated scales: " << multiDoubleScales << std::endl;
  multiDoubleStep = shiftScaleEstimator->EstimateStepScale( step );
  std::cout << "multi-double estimated stepScale: " << multiDoubleStep << std::endl;

  // Check that results are the same for all three estimations
  bool passedEstimation = true;
  ScalesEstimatorMultiType::FloatType tolerance = static_cast<ScalesEstimatorMultiType::FloatType>(1e-6);
  if( std::fabs(singleStep - multiSingleStep) > tolerance || std::fabs(singleStep - multiDoubleStep) > tolerance )
    {
    std::cerr << "Steps do not match as expected between estimation on same metric." << std::endl;
    passedEstimation = false;
    }
  if( std::fabs(singleScales[0] - multiSingleScales[0] ) > tolerance ||
      std::fabs(singleScales[1] - multiSingleScales[1] ) > tolerance ||
      std::fabs(singleScales[0] - multiDoubleScales[0] ) > tolerance ||
      std::fabs(singleScales[1] - multiDoubleScales[1] ) > tolerance   )
    {
    std::cerr << "Scales do not match as expected between estimation on same metric." << std::endl;
    passedEstimation = false;
    }
  if( ! passedEstimation )
    {
    return EXIT_FAILURE;
    }

  if( ! useDisplacementTransform )
    {
    // Exercising the Print function
    std::cout << "Print: " << std::endl;
    multiVariateMetric->Print(std::cout);

    // Test ClearMetricQueue
    multiVariateMetric->ClearMetricQueue();
    if( multiVariateMetric->GetNumberOfMetrics() != 0 )
      {
      std::cerr << "ClearMetricQueue() failed. Number of metrics is not zero." << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}

int itkObjectToObjectMultiMetricv4Test (int , char *[])
{
  std::cout << "XXX Test with TranslationTransform XXX" << std::endl << std::endl;
  int result = itkObjectToObjectMultiMetricv4TestRun( false );
  if( result == EXIT_FAILURE )
    {
    std::cerr << "Failed test with translation transform. See message above." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << std::endl << std::endl << "XXX Test with DisplacementFieldTransform XXX" << std::endl << std::endl;
  result = itkObjectToObjectMultiMetricv4TestRun( true );
  if( result == EXIT_FAILURE )
    {
    std::cerr << "Failed test with displacement field transform. See message above." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
