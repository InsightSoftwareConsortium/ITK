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

#include "itkCorrelationImageToImageMetricv4.h"
#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImage.h"
#include "itkGaussianImageSource.h"
#include "itkCyclicShiftImageFilter.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkObjectToObjectMultiMetricv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"

/* This test performs a simple registration test using
 * a single metric and a multivariate metric containing
 * two copies of the metric, testing
 * that the results are the same.
 */

template<typename TFilter>
class itkObjectToObjectMultiMetricv4RegistrationTestCommandIterationUpdate : public itk::Command
{
public:
  typedef itkObjectToObjectMultiMetricv4RegistrationTestCommandIterationUpdate   Self;

  typedef itk::Command             Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );

protected:
  itkObjectToObjectMultiMetricv4RegistrationTestCommandIterationUpdate() {};

public:

  virtual void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *) caller, event);
    }

  virtual void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      {
      return;
      }
    const TFilter *optimizer = dynamic_cast< const TFilter * >( object );

    if( !optimizer )
      {
      itkGenericExceptionMacro( "Error dynamic_cast failed" );
      }
    std::cout << "It- " << optimizer->GetCurrentIteration() << " gradient: " << optimizer->GetGradient() << " metric value: " << optimizer->GetCurrentMetricValue()
              << " Params: " << const_cast<TFilter*>(optimizer)->GetCurrentPosition() << std::endl;
    }
};

template<typename TImage>
void ObjectToObjectMultiMetricv4RegistrationTestCreateImages( typename TImage::Pointer & fixedImage, typename TImage::Pointer & movingImage, typename TImage::OffsetType & imageShift )
{
  typedef typename TImage::PixelType  PixelType;
  typedef PixelType                   CoordinateRepresentationType;

  // Create two simple images
  itk::SizeValueType ImageSize = 100;
  itk::OffsetValueType boundary = 6;

   // Declare Gaussian Sources
  typedef itk::GaussianImageSource< TImage >        GaussianImageSourceType;

  typename TImage::SizeType size;
  size.Fill( ImageSize );

  typename TImage::SpacingType spacing;
  spacing.Fill( itk::NumericTraits<CoordinateRepresentationType>::OneValue() );

  typename TImage::PointType origin;
  origin.Fill( itk::NumericTraits<CoordinateRepresentationType>::ZeroValue() );

  typename TImage::DirectionType direction;
  direction.Fill( itk::NumericTraits<CoordinateRepresentationType>::OneValue() );

  typename GaussianImageSourceType::Pointer  fixedImageSource = GaussianImageSourceType::New();

  fixedImageSource->SetSize(    size    );
  fixedImageSource->SetOrigin(  origin  );
  fixedImageSource->SetSpacing( spacing );
  fixedImageSource->SetNormalized( false );
  fixedImageSource->SetScale( 1.0f );
  fixedImageSource->Update();
  fixedImage = fixedImageSource->GetOutput();

  // zero-out the boundary
  itk::ImageRegionIteratorWithIndex<TImage> it( fixedImage, fixedImage->GetLargestPossibleRegion() );
  for( it.GoToBegin(); ! it.IsAtEnd(); ++it )
    {
    for( itk::SizeValueType n=0; n < TImage::ImageDimension; n++ )
      {
      if( it.GetIndex()[n] < boundary || (static_cast<itk::OffsetValueType>(size[n]) - it.GetIndex()[n]) <= boundary )
        {
        it.Set( itk::NumericTraits<PixelType>::ZeroValue() );
        break;
        }
      }
    }

  // shift the fixed image to get the moving image
  typedef itk::CyclicShiftImageFilter<TImage, TImage> CyclicShiftFilterType;
  typename CyclicShiftFilterType::Pointer shiftFilter = CyclicShiftFilterType::New();
  typename CyclicShiftFilterType::OffsetValueType maxImageShift = boundary-1;
  imageShift.Fill( maxImageShift );
  imageShift[0] = maxImageShift / 2;
  shiftFilter->SetInput( fixedImage );
  shiftFilter->SetShift( imageShift );
  shiftFilter->Update();
  movingImage = shiftFilter->GetOutput();
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<typename TMetric>
int ObjectToObjectMultiMetricv4RegistrationTestRun( typename TMetric::Pointer & metric, int numberOfIterations,
                                                    typename TMetric::MeasureType & valueResult, typename TMetric::DerivativeType & derivativeResult,
                                                    typename TMetric::InternalComputationValueType maxStep, bool estimateStepOnce )
{
  // calculate initial metric value
  metric->Initialize();
  typename TMetric::MeasureType initialValue = metric->GetValue();

  // scales estimator
  typedef itk::RegistrationParameterScalesFromPhysicalShift< TMetric > RegistrationParameterScalesFromShiftType;
  typename RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromShiftType::New();
  shiftScaleEstimator->SetMetric(metric);

  //
  // optimizer
  //
  typedef itk::GradientDescentOptimizerv4  OptimizerType;
  typename OptimizerType::Pointer  optimizer = OptimizerType::New();

  optimizer->SetMetric( metric );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetScalesEstimator( shiftScaleEstimator );
  optimizer->SetMaximumStepSizeInPhysicalUnits( maxStep );
  optimizer->SetDoEstimateLearningRateOnce( estimateStepOnce );
  optimizer->SetDoEstimateLearningRateAtEachIteration( ! estimateStepOnce );

  typedef itkObjectToObjectMultiMetricv4RegistrationTestCommandIterationUpdate<OptimizerType> CommandType;
  typename CommandType::Pointer observer = CommandType::New();
  //optimizer->AddObserver( itk::IterationEvent(), observer );

  optimizer->StartOptimization();

  std::cout << "# of iterations: " << optimizer->GetNumberOfIterations() << std::endl;
  std::cout << "DoEstimateLearningRateOnce: " << optimizer->GetDoEstimateLearningRateOnce()
            << " GetDoEstimateLearningRateAtEachIteration: " << optimizer->GetDoEstimateLearningRateAtEachIteration() << std::endl;
  derivativeResult = optimizer->GetCurrentPosition();
  std::cout << "Transform final parameters: " << derivativeResult << " mag: " << derivativeResult.magnitude() << std::endl;

  // final metric value
  valueResult = metric->GetValue();
  std::cout << "metric value: initial: " << initialValue << ", final: " << valueResult << std::endl;

  // scales
  std::cout << "scales: " << optimizer->GetScales() << std::endl;
  std::cout << "optimizer learning rate at end: " << optimizer->GetLearningRate() << std::endl;

  return EXIT_SUCCESS;
}

//////////////////////////////////////////////////////////////
int itkObjectToObjectMultiMetricv4RegistrationTest(int argc, char *argv[])
{
  const int Dimension = 2;
  typedef itk::Image< double, Dimension > ImageType;

  int numberOfIterations = 30;
  if( argc > 1 )
    {
    numberOfIterations = atoi( argv[1] );
    }

  // create an affine transform
  typedef itk::TranslationTransform<double, Dimension> TranslationTransformType;
  TranslationTransformType::Pointer translationTransform = TranslationTransformType::New();
  translationTransform->SetIdentity();

  // create images
  ImageType::Pointer fixedImage = ITK_NULLPTR, movingImage = ITK_NULLPTR;
  ImageType::OffsetType imageShift;
  imageShift.Fill(0);
  ObjectToObjectMultiMetricv4RegistrationTestCreateImages<ImageType>( fixedImage, movingImage, imageShift );

  typedef itk::CorrelationImageToImageMetricv4<ImageType, ImageType> CorrelationMetricType;
  CorrelationMetricType::Pointer correlationMetric = CorrelationMetricType::New();
  correlationMetric->SetFixedImage( fixedImage );
  correlationMetric->SetMovingImage( movingImage );
  correlationMetric->SetMovingTransform( translationTransform );
  correlationMetric->Initialize();

  translationTransform->SetIdentity();

  std::cout << std::endl << "*** Single image metric: " << std::endl;
  CorrelationMetricType::MeasureType singleValueResult = 0.0;
  CorrelationMetricType::DerivativeType singleDerivativeResult;
  singleDerivativeResult.Fill(0);
  ObjectToObjectMultiMetricv4RegistrationTestRun<CorrelationMetricType>( correlationMetric, numberOfIterations, singleValueResult, singleDerivativeResult, 1.0, true );

  std::cout << "*** multi-variate metric: " << std::endl;
  CorrelationMetricType::Pointer metric2 = CorrelationMetricType::New();
  metric2->SetFixedImage( fixedImage );
  metric2->SetMovingImage( movingImage );
  metric2->SetMovingTransform( translationTransform );

  typedef itk::ObjectToObjectMultiMetricv4<Dimension,Dimension> MultiMetricType;
  MultiMetricType::Pointer multiMetric = MultiMetricType::New();
  multiMetric->AddMetric( correlationMetric );
  multiMetric->AddMetric( metric2 );
  multiMetric->AddMetric( metric2 );
  multiMetric->Initialize();

  translationTransform->SetIdentity();

  CorrelationMetricType::MeasureType multiValueResult = 0.0;
  CorrelationMetricType::DerivativeType multiDerivativeResult;
  multiDerivativeResult.Fill(0);
  ObjectToObjectMultiMetricv4RegistrationTestRun<MultiMetricType>( multiMetric, numberOfIterations, multiValueResult, multiDerivativeResult, 1.0, true );

  // Comparison between single-metric and multi-variate metric registrations
  CorrelationMetricType::DerivativeValueType tolerance = static_cast<CorrelationMetricType::DerivativeValueType>(1e-6);
  if( std::fabs( multiDerivativeResult[0] - singleDerivativeResult[0] ) > tolerance ||
      std::fabs( multiDerivativeResult[1] - singleDerivativeResult[1] ) > tolerance )
      {
      std::cerr << "multi-variate registration derivative: " << multiDerivativeResult
                << " are different from single-variate derivative: " << singleDerivativeResult << std::endl;
      return EXIT_FAILURE;
      }
  if( std::fabs( multiValueResult - singleValueResult ) > tolerance )
      {
      std::cerr << "multi-variate registration value: " << multiValueResult
                << " is different from single-variate value: " << singleValueResult << std::endl;
      return EXIT_FAILURE;
      }

  // compare results with truth
  tolerance = static_cast<CorrelationMetricType::DerivativeValueType>(0.05);
  if( std::fabs( multiDerivativeResult[0] - imageShift[0] ) / imageShift[0] > tolerance ||
      std::fabs( multiDerivativeResult[1] - imageShift[1] ) / imageShift[1] > tolerance )
      {
      std::cerr << "multi-variate registration results: " << multiDerivativeResult << " are not as expected: " << imageShift << std::endl;
      return EXIT_FAILURE;
      }


  //
  // Try with step estimation at every iteration
  // Comparison between single-metric and multi-variate metric registrations
  //
  std::cout << std::endl << "*** Single image metric 2: " << std::endl;
  translationTransform->SetIdentity();
  ObjectToObjectMultiMetricv4RegistrationTestRun<CorrelationMetricType>( correlationMetric, numberOfIterations, singleValueResult, singleDerivativeResult, 0.25, false );

  std::cout << std::endl << "*** Multi-variate image metric 2: " << std::endl;
  translationTransform->SetIdentity();
  ObjectToObjectMultiMetricv4RegistrationTestRun<MultiMetricType>( multiMetric, numberOfIterations, multiValueResult, multiDerivativeResult, 0.25, false );

  if( std::fabs( multiDerivativeResult[0] - singleDerivativeResult[0] ) > tolerance ||
      std::fabs( multiDerivativeResult[1] - singleDerivativeResult[1] ) > tolerance )
      {
      std::cerr << "multi-variate registration derivative: " << multiDerivativeResult
                << " are different from single-variate derivative: " << singleDerivativeResult << std::endl;
      return EXIT_FAILURE;
      }
  if( std::fabs( multiValueResult - singleValueResult ) > tolerance )
      {
      std::cerr << "multi-variate registration value: " << multiValueResult
                << " is different from single-variate value: " << singleValueResult << std::endl;
      return EXIT_FAILURE;
      }

  // compare results with truth
  tolerance = static_cast<CorrelationMetricType::DerivativeValueType>(0.05);
  if( std::fabs( multiDerivativeResult[0] - imageShift[0] ) / imageShift[0] > tolerance ||
      std::fabs( multiDerivativeResult[1] - imageShift[1] ) / imageShift[1] > tolerance )
      {
      std::cerr << "multi-variate registration results: " << multiDerivativeResult << " are not as expected: " << imageShift << std::endl;
      return EXIT_FAILURE;
      }

  //
  // Test with two different metric types
  //
  typedef itk::MeanSquaresImageToImageMetricv4<ImageType, ImageType> MeanSquaresMetricType;
  MeanSquaresMetricType::Pointer meanSquaresMetric = MeanSquaresMetricType::New();
  meanSquaresMetric->SetFixedImage( fixedImage );
  meanSquaresMetric->SetMovingImage( movingImage );
  meanSquaresMetric->SetMovingTransform( translationTransform );

  MultiMetricType::Pointer multiMetric2 = MultiMetricType::New();
  multiMetric2->AddMetric( correlationMetric );
  multiMetric2->AddMetric( meanSquaresMetric );
  multiMetric2->Initialize();

  translationTransform->SetIdentity();
  std::cout << "*** Multi-metric with different metric types: " << std::endl;
  ObjectToObjectMultiMetricv4RegistrationTestRun<MultiMetricType>( multiMetric2, numberOfIterations, multiValueResult, multiDerivativeResult, 1.0, true );

  // compare results with truth
  tolerance = static_cast<MeanSquaresMetricType::DerivativeValueType>(0.05);
  if( std::fabs( multiDerivativeResult[0] - imageShift[0] ) / imageShift[0] > tolerance ||
      std::fabs( multiDerivativeResult[1] - imageShift[1] ) / imageShift[1] > tolerance )
      {
      std::cerr << "multi-variate registration results: " << multiDerivativeResult << " are not as expected: " << imageShift << std::endl;
      return EXIT_FAILURE;
      }

  return EXIT_SUCCESS;
}
