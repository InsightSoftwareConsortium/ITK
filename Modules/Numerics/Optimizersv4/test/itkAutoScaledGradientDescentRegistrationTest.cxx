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
#include "itkGradientDescentOptimizerv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkRegistrationParameterScalesFromIndexShift.h"
#include "itkRegistrationParameterScalesFromJacobian.h"

#include "itkSize.h"
#include "itkExceptionObject.h"
#include "itkImageRegistrationMethodImageSource.h"
#include "itkMath.h"

/**
 *  This is a test using GradientDescentOptimizerv4 and parameter scales
 *  estimator. The scales are estimated before the first iteration by
 *  RegistrationParameterScalesFromShift. The learning rates are estimated
 *  at each iteration according to the shift of each step.
 */

template< typename TMovingTransform >
int itkAutoScaledGradientDescentRegistrationTestTemplated(
                                            int numberOfIterations,
                                            double shiftOfStep,
                                            std::string scalesOption,
                                            bool usePhysicalSpaceForShift,
                                            bool estimateLearningRateOnce,
                                            bool estimateLearningRateAtEachIteration,
                                            bool estimateScales )
{
  const unsigned int Dimension = TMovingTransform::SpaceDimension;
  typedef double PixelType;

  // Fixed Image Type
  typedef itk::Image<PixelType,Dimension>               FixedImageType;

  // Moving Image Type
  typedef itk::Image<PixelType,Dimension>               MovingImageType;

  // Size Type
  typedef typename MovingImageType::SizeType            SizeType;

  // ImageSource
  typedef typename itk::testhelper::ImageRegistrationMethodImageSource<
                                  typename FixedImageType::PixelType,
                                  typename MovingImageType::PixelType,
                                  Dimension >         ImageSourceType;

  typename FixedImageType::ConstPointer    fixedImage;
  typename MovingImageType::ConstPointer   movingImage;
  typename ImageSourceType::Pointer        imageSource;

  imageSource   = ImageSourceType::New();

  SizeType size;
  size[0] = 100;
  size[1] = 100;

  imageSource->GenerateImages( size );

  fixedImage    = imageSource->GetFixedImage();
  movingImage   = imageSource->GetMovingImage();

  // Transform for the moving image
  typedef TMovingTransform MovingTransformType;
  typename MovingTransformType::Pointer movingTransform = MovingTransformType::New();
  movingTransform->SetIdentity();

  // Transform for the fixed image
  typedef itk::IdentityTransform<double, Dimension> FixedTransformType;
  typename FixedTransformType::Pointer fixedTransform = FixedTransformType::New();
  fixedTransform->SetIdentity();

  // ParametersType for the moving transform
  typedef typename MovingTransformType::ParametersType ParametersType;

  // Metric
  typedef itk::MeanSquaresImageToImageMetricv4
    < FixedImageType, MovingImageType, FixedImageType > MetricType;
  typename MetricType::Pointer metric = MetricType::New();

  // Assign images and transforms to the metric.
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetVirtualDomainFromImage( const_cast<FixedImageType *>(fixedImage.GetPointer()) );

  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );

  // Initialize the metric to prepare for use
  metric->Initialize();

  // Optimizer
  typedef itk::GradientDescentOptimizerv4  OptimizerType;
  OptimizerType::Pointer optimizer = OptimizerType::New();

  optimizer->SetMetric( metric );
  optimizer->SetNumberOfIterations( numberOfIterations );

  // Instantiate an Observer to report the progress of the Optimization
  typedef itk::CommandIterationUpdate< OptimizerType >  CommandIterationType;
  CommandIterationType::Pointer iterationCommand = CommandIterationType::New();
  iterationCommand->SetOptimizer( optimizer.GetPointer() );

  // Optimizer parameter scales estimator
  typename itk::OptimizerParameterScalesEstimator::Pointer scalesEstimator;

  typedef itk::RegistrationParameterScalesFromPhysicalShift< MetricType > PhysicalShiftScalesEstimatorType;
  typedef itk::RegistrationParameterScalesFromIndexShift< MetricType > IndexShiftScalesEstimatorType;
  typedef itk::RegistrationParameterScalesFromJacobian< MetricType > JacobianScalesEstimatorType;

  if (scalesOption.compare("shift") == 0)
    {
    if( usePhysicalSpaceForShift )
      {
      std::cout << "Testing RegistrationParameterScalesFrom*Physical*Shift" << std::endl;
      typename PhysicalShiftScalesEstimatorType::Pointer shiftScalesEstimator = PhysicalShiftScalesEstimatorType::New();
      shiftScalesEstimator->SetMetric(metric);
      shiftScalesEstimator->SetTransformForward(true); //default
      scalesEstimator = shiftScalesEstimator;
      }
    else
      {
      std::cout << "Testing RegistrationParameterScalesFrom*Index*Shift" << std::endl;
      typename IndexShiftScalesEstimatorType::Pointer shiftScalesEstimator = IndexShiftScalesEstimatorType::New();
      shiftScalesEstimator->SetMetric(metric);
      shiftScalesEstimator->SetTransformForward(true); //default
      scalesEstimator = shiftScalesEstimator;
      }
    }
  else
    {
    std::cout << "Testing RegistrationParameterScalesFrom*Jacobian*" << std::endl;
    typename JacobianScalesEstimatorType::Pointer jacobianScalesEstimator
      = JacobianScalesEstimatorType::New();
    jacobianScalesEstimator->SetMetric(metric);
    jacobianScalesEstimator->SetTransformForward(true); //default
    scalesEstimator = jacobianScalesEstimator;
    }

  optimizer->SetScalesEstimator(scalesEstimator);
  // If SetMaximumStepSizeInPhysicalUnits is not called, it will use voxel spacing.
  optimizer->SetMaximumStepSizeInPhysicalUnits(shiftOfStep);
  optimizer->SetDoEstimateLearningRateOnce( estimateLearningRateOnce );
  optimizer->SetDoEstimateLearningRateAtEachIteration( estimateLearningRateAtEachIteration );
  optimizer->SetDoEstimateScales( estimateScales );

  // Set initial scales to bad values
  OptimizerType::ScalesType initScales( metric->GetNumberOfParameters() );
  initScales.Fill( static_cast<OptimizerType::ScalesType::ValueType>(999999) );
  optimizer->SetScales( initScales );
  std::cout << "Initial Scales: " << optimizer->GetScales() << std::endl;

  // If no learning rate estimate is performed, test with a fixed value
  // close to the result of running this test with learning rate estimation
  // for only the first step.
  const OptimizerType::InternalComputationValueType fixedLearningRate = 0.01501010101010101;
  if( ! estimateLearningRateOnce && ! estimateLearningRateAtEachIteration )
    {
    optimizer->SetLearningRate( fixedLearningRate );
    }
  std::cout << "Initial learning rate: " << optimizer->GetLearningRate() << std::endl;

  std::cout << "**Start optimization..." << std::endl
            << "Number of iterations: " << numberOfIterations << std::endl;
  std::cout << "GetDoEstimateScales: " << optimizer->GetDoEstimateScales() << std::endl;
  std::cout << "GetDoEstimateLearningRateOnce: " << optimizer->GetDoEstimateLearningRateOnce() << std::endl;
  std::cout << "GetDoEstimateLearningRateAtEachIteration: " << optimizer->GetDoEstimateLearningRateAtEachIteration() << std::endl;

  try
    {
    optimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization:" << std::endl;
    std::cout << e.GetLocation() << std::endl;
    std::cout << e.GetDescription() << std::endl;
    std::cout << e.what()    << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "...finished. " << std::endl
            << "StopCondition: " << optimizer->GetStopConditionDescription()
            << std::endl
            << "Metric: NumberOfValidPoints: "
            << metric->GetNumberOfValidPoints() << std::endl
            << "Final scales: " << optimizer->GetScales() << std::endl
            << "Final learning rate: " << optimizer->GetLearningRate()
            << std::endl;

  if( ! estimateLearningRateOnce && ! estimateLearningRateAtEachIteration )
    {
    if( itk::Math::NotExactlyEquals(optimizer->GetLearningRate(), fixedLearningRate) )
      {
      std::cerr << "Expected learning rate not to change." << std::endl;
      return EXIT_FAILURE;
      }
    }

  // If scale estimation was disabled, make sure the scales didn't change
  if( ! estimateScales )
    {
    OptimizerType::ScalesType postScales = optimizer->GetScales();
    for( itk::SizeValueType s=0; s < postScales.Size(); s++ )
      {
      if( itk::Math::NotExactlyEquals(initScales[s], postScales[s]) )
        {
        std::cerr << "Scales were estimated by optimizer despite not being "
                  << "enabled to do so." << std::endl;
        return EXIT_FAILURE;
        }
      }
    // Just return now since we won't get a valid result w/out scales estimation
    // for the jacobian shift case.
    return EXIT_SUCCESS;
    }

  //
  // results
  //
  ParametersType finalParameters  = movingTransform->GetParameters();
  ParametersType fixedParameters  = movingTransform->GetFixedParameters();
  std::cout << "Estimated scales = " << optimizer->GetScales() << std::endl;
  std::cout << "finalParameters = " << finalParameters << std::endl;
  std::cout << "fixedParameters = " << fixedParameters << std::endl;
  bool pass = true;

  ParametersType actualParameters = imageSource->GetActualParameters();
  std::cout << "actualParameters = " << actualParameters << std::endl;
  const unsigned int numbeOfParameters = actualParameters.Size();

  // We know that for the Affine transform the Translation parameters are at
  // the end of the list of parameters.
  const unsigned int offsetOrder = finalParameters.Size()-actualParameters.Size();

  const double tolerance = 1.0;  // equivalent to 1 pixel.

  for(unsigned int i=0; i<numbeOfParameters; i++)
    {
    // the parameters are negated in order to get the inverse transformation.
    // this only works for comparing translation parameters....
    std::cout << finalParameters[i+offsetOrder] << " == " << -actualParameters[i] << std::endl;
    if( itk::Math::abs ( finalParameters[i+offsetOrder] - (-actualParameters[i]) ) > tolerance )
      {
      std::cout << "Tolerance exceeded at component " << i << std::endl;
      pass = false;
      }
    }

  if( !pass )
    {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "Test PASSED." << std::endl;
    return EXIT_SUCCESS;
    }
}

int itkAutoScaledGradientDescentRegistrationTest(int argc, char ** const argv)
{
  if( argc > 6 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " [numberOfIterations=30 shiftOfStep=1.0] ";
    std::cerr << " [estimateLearningRateOnce = true] ";
    std::cerr << " [estimateLearningRateAtEachIteration = false] ";
    std::cerr << " [estimateScales = true] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }
  unsigned int numberOfIterations = 30;
  double shiftOfStep = 1.0;
  bool estimateLearningRateOnce = true;
  bool estimateLearningRateAtEachIteration = false;
  bool estimateScales = true;

  if( argc >= 2 )
    {
    numberOfIterations = atoi( argv[1] );
    }
  if (argc >= 3)
    {
    shiftOfStep = atof( argv[2] );
    }
  if (argc >= 4)
    {
    estimateLearningRateOnce = atoi( argv[3] );
    }
  if (argc >= 5)
    {
    estimateLearningRateAtEachIteration = atoi( argv[4] );
    }
  if (argc >= 6)
    {
    estimateScales = atoi( argv[5] );
    }

  const unsigned int Dimension = 2;

  std::cout << std::endl << "Optimizing translation transform with shift scales" << std::endl;
  typedef itk::TranslationTransform<double, Dimension> TranslationTransformType;
  bool usePhysicalSpaceForShift = false;
  int ret1 = itkAutoScaledGradientDescentRegistrationTestTemplated<TranslationTransformType>(
    numberOfIterations, shiftOfStep, "shift", usePhysicalSpaceForShift,
    estimateLearningRateOnce, estimateLearningRateAtEachIteration, estimateScales);

  std::cout << std::endl << "Optimizing translation transform with Jacobian scales" << std::endl;
  typedef itk::TranslationTransform<double, Dimension> TranslationTransformType;
  int ret2 = itkAutoScaledGradientDescentRegistrationTestTemplated<TranslationTransformType>(
    numberOfIterations, 0.0, "jacobian", usePhysicalSpaceForShift,
    estimateLearningRateOnce, estimateLearningRateAtEachIteration, estimateScales);

  if ( ret1 == EXIT_SUCCESS && ret2 == EXIT_SUCCESS )
    {
    return EXIT_SUCCESS;
    }
  else
    {
    return EXIT_FAILURE;
    }
}
