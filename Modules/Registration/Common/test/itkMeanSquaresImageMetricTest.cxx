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

#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkGaussianImageSource.h"

#include <iostream>
#include "itkStdStreamStateSave.h"

/**
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 *  This test computes the mean squares value and derivatives
 *  for various shift values in (-10,10).
 *
 */

int itkMeanSquaresImageMetricTest(int, char* [] )
{

// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
  itk::StdStreamStateSave coutState(std::cout);

//------------------------------------------------------------
// Create two simple images
//------------------------------------------------------------

  const unsigned int ImageDimension = 2;

  typedef double                   PixelType;

  typedef double                   CoordinateRepresentationType;

  //Allocate Images
  typedef itk::Image<PixelType,ImageDimension>         MovingImageType;
  typedef itk::Image<PixelType,ImageDimension>         FixedImageType;

  // Declare Gaussian Sources
  typedef itk::GaussianImageSource< MovingImageType >  MovingImageSourceType;
  typedef itk::GaussianImageSource< FixedImageType  >  FixedImageSourceType;

  // Note: the following declarations are classical arrays
  FixedImageType::SizeValueType fixedImageSize[]     = {  100,  100 };
  MovingImageType::SizeValueType movingImageSize[]    = {  100,  100 };

  FixedImageType::SpacingValueType fixedImageSpacing[]  = { 1.0f, 1.0f };
  MovingImageType::SpacingValueType movingImageSpacing[] = { 1.0f, 1.0f };

  FixedImageType::PointValueType fixedImageOrigin[]   = { 0.0f, 0.0f };
  MovingImageType::PointValueType movingImageOrigin[]  = { 0.0f, 0.0f };

  MovingImageSourceType::Pointer movingImageSource = MovingImageSourceType::New();
  FixedImageSourceType::Pointer  fixedImageSource  = FixedImageSourceType::New();

  movingImageSource->SetSize(    movingImageSize    );
  movingImageSource->SetOrigin(  movingImageOrigin  );
  movingImageSource->SetSpacing( movingImageSpacing );
  movingImageSource->SetNormalized( false );
  movingImageSource->SetScale( 250.0f );

  fixedImageSource->SetSize(    fixedImageSize    );
  fixedImageSource->SetOrigin(  fixedImageOrigin  );
  fixedImageSource->SetSpacing( fixedImageSpacing );
  fixedImageSource->SetNormalized( false );
  fixedImageSource->SetScale( 250.0f );

  movingImageSource->Update(); // Force the filter to run
  fixedImageSource->Update();  // Force the filter to run

  MovingImageType::Pointer movingImage = movingImageSource->GetOutput();
  FixedImageType::Pointer  fixedImage  = fixedImageSource->GetOutput();


//-----------------------------------------------------------
// Set up  the Metric
//-----------------------------------------------------------
  typedef itk::MeanSquaresImageToImageMetric<
                                       FixedImageType,
                                       MovingImageType >
                                                    MetricType;

  typedef MetricType::TransformType                 TransformBaseType;
  typedef TransformBaseType::ParametersType         ParametersType;

  MetricType::Pointer  metric = MetricType::New();


//-----------------------------------------------------------
// Plug the Images into the metric
//-----------------------------------------------------------
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );

//-----------------------------------------------------------
// Set up a Transform
//-----------------------------------------------------------

  typedef itk::TranslationTransform<
                        CoordinateRepresentationType,
                        ImageDimension >         TransformType;

  TransformType::Pointer transform = TransformType::New();

  metric->SetTransform( transform.GetPointer() );


//------------------------------------------------------------
// Set up an Interpolator
//------------------------------------------------------------
  typedef itk::LinearInterpolateImageFunction<
                    MovingImageType,
                    double > InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  interpolator->SetInputImage( movingImage.GetPointer() );

  metric->SetInterpolator( interpolator.GetPointer() );


//------------------------------------------------------------
// Define the region over which the metric will be computed
//------------------------------------------------------------
  metric->SetFixedImageRegion( fixedImage->GetBufferedRegion() );

  std::cout << metric << std::endl;


//------------------------------------------------------------
// This call is mandatory before start querying the Metric
// This method makes all the necessary connections between the
// internal components: Interpolator, Transform and Images
//------------------------------------------------------------
  try {
    metric->Initialize();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Metric initialization failed" << std::endl;
    std::cout << "Reason " << e.GetDescription() << std::endl;

    return EXIT_FAILURE;
    }


//------------------------------------------------------------
// Set up transform parameters
//------------------------------------------------------------
  ParametersType parameters( transform->GetNumberOfParameters() );

  // initialize the offset/vector part
  for( unsigned int k = 0; k < ImageDimension; k++ )
    {
    parameters[k]= 0.0f;
    }


//---------------------------------------------------------
// Print out metric values
// for parameters[1] = {-10,10}  (arbitrary choice...)
//---------------------------------------------------------

  MetricType::MeasureType     measure;
  MetricType::DerivativeType  derivative;

  std::cout << "param[1]   Metric    d(Metric)/d(param[1]) " << std::endl;

  for( double trans = -10; trans <= 5; trans += 0.2  )
    {
    parameters[1] = trans;
    metric->GetValueAndDerivative( parameters, measure, derivative );

    std::cout.width(5);
    std::cout.precision(5);
    std::cout << trans;
    std::cout.width(15);
    std::cout.precision(5);
    std::cout << measure;
    std::cout.width(15);
    std::cout.precision(5);
    std::cout << derivative[1];
    std::cout << std::endl;

    // exercise the other functions
    metric->GetValue( parameters );
    metric->GetDerivative( parameters, derivative );
    }

  // Compute a reference metric and partial derivative with one
  // thread. NOTE - this test checks for consistency in the answer
  // computed by differing numbers of threads, not correctness.
  metric->SetNumberOfThreads(1);
  metric->Initialize();
  parameters[1] = 2.0;
  MetricType::MeasureType    referenceMeasure;
  MetricType::DerivativeType referenceDerivative;
  referenceMeasure = metric->GetValue(parameters);
  metric->GetDerivative( parameters, referenceDerivative );

  std::cout << "Testing consistency of the metric value computed by "
            << "several different thread counts." << std::endl;

  // Now check that the same metric value is computed when the number
  // of threads is adjusted from 1 to 8.
  for (int currNumThreadsToTest = 1; currNumThreadsToTest <= 8; currNumThreadsToTest++)
    {
    itk::MultiThreader::SetGlobalMaximumNumberOfThreads(currNumThreadsToTest);
    metric->SetNumberOfThreads(currNumThreadsToTest);
    metric->Initialize();

    std::cout << "Threads Metric    d(Metric)/d(param[1]) " << std::endl;

    measure = metric->GetValue( parameters );
    metric->GetDerivative( parameters, derivative );
    std::cout.width(4);
    std::cout << currNumThreadsToTest;
    std::cout.width(10);
    std::cout.precision(5);
    std::cout << measure;
    std::cout.width(10);
    std::cout.precision(5);
    std::cout << derivative[1];
    std::cout << std::endl;

    bool sameDerivative = true;
    for (unsigned int d = 0; d < parameters.Size(); d++)
      {
      if ( fabs(derivative[d] - referenceDerivative[d]) > 1e-5 )
        {
        sameDerivative = false;
        break;
        }
      }

    if ( fabs(measure - referenceMeasure) > 1e-5 || !sameDerivative )
      {
      std::cout << "Testing different number of threads... FAILED" << std::endl;
      std::cout << "Metric value computed with " << currNumThreadsToTest
                << " threads is incorrect. Computed value is "
                << measure << ", should be " << referenceMeasure
                << ", computed derivative is " << derivative
                << ", should be " << referenceDerivative << std::endl;

      return EXIT_FAILURE;
      }
    }
  std::cout << "Testing different number of threads... PASSED." << std::endl;

  // Now check that the same metric value is computed when the number
  // of threads in the metric is set to 8 and the global max number of
  // threads is reduced to 2. These are arbitrary numbers of threads
  // used to verify the correctness of the metric under a particular
  // usage scenario.
  metric->SetNumberOfThreads(8);
  const int numThreads = 2;
  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(numThreads);
  metric->Initialize();

  std::cout << "Threads Metric    d(Metric)/d(param[1]) " << std::endl;

  measure = metric->GetValue( parameters );
  std::cout.width(4);
  std::cout << numThreads;
  std::cout.width(10);
  std::cout.precision(5);
  std::cout << measure;
  std::cout.width(10);
  std::cout.precision(5);
  std::cout << derivative[1];
  std::cout << std::endl;
  if ( fabs(measure - referenceMeasure) > 1e-5 )
    {
    std::cout << "Test reducing global max number of threads... FAILED." << std::endl;
    std::cout << "Metric value computed with " << numThreads
              << " threads is incorrect. Computed value is "
              << measure << ", should be " << referenceMeasure << std::endl;

    return EXIT_FAILURE;
    }
  std::cout << "Test reducing global max number of threads... PASSED." << std::endl;

//-------------------------------------------------------
// exercise Print() method
//-------------------------------------------------------
  metric->Print( std::cout );

//-------------------------------------------------------
// exercise misc member functions
//-------------------------------------------------------
  std::cout << "FixedImage: " << metric->GetFixedImage() << std::endl;
  std::cout << "MovingImage: " << metric->GetMovingImage() << std::endl;
  std::cout << "Transform: " << metric->GetTransform() << std::endl;
  std::cout << "Interpolator: " << metric->GetInterpolator() << std::endl;
  std::cout << "NumberOfPixelsCounted: " << metric->GetNumberOfPixelsCounted() << std::endl;
  std::cout << "FixedImageRegion: " << metric->GetFixedImageRegion() << std::endl;

  std::cout << "Check case when Target is ITK_NULLPTR" << std::endl;
  metric->SetFixedImage( ITK_NULLPTR );
  try
    {
    std::cout << "Value = " << metric->GetValue( parameters );
    std::cout << "If you are reading this message the Metric " << std::endl;
    std::cout << "is NOT managing exceptions correctly    " << std::endl;

    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception received (as expected) "    << std::endl;
    std::cout << "Description : " << e.GetDescription() << std::endl;
    std::cout << "Location    : " << e.GetLocation()    << std::endl;
    std::cout << "Test for exception throwing... PASSED ! " << std::endl;
    }

  try
    {
    metric->GetValueAndDerivative( parameters, measure, derivative );
    std::cout << "Value = " << measure << std::endl;
    std::cout << "If you are reading this message the Metric " << std::endl;
    std::cout << "is NOT managing exceptions correctly    " << std::endl;

    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception received (as expected) "    << std::endl;
    std::cout << "Description : " << e.GetDescription() << std::endl;
    std::cout << "Location    : " << e.GetLocation()    << std::endl;
    std::cout << "Test for exception throwing... PASSED ! "  << std::endl;
    }

 bool pass;
#define TEST_INITIALIZATION_ERROR( ComponentName, badComponent, goodComponent ) \
  metric->Set##ComponentName( badComponent ); \
  try \
    { \
    pass = false; \
    metric->Initialize(); \
    } \
  catch( itk::ExceptionObject& err ) \
    { \
    std::cout << "Caught expected ExceptionObject" << std::endl; \
    std::cout << err << std::endl; \
    pass = true; \
    } \
  metric->Set##ComponentName( goodComponent ); \
  \
  if( !pass ) \
    { \
    std::cout << "Test failed." << std::endl; \
    return EXIT_FAILURE; \
    }

  TEST_INITIALIZATION_ERROR( Transform, ITK_NULLPTR, transform );
  TEST_INITIALIZATION_ERROR( FixedImage, ITK_NULLPTR, fixedImage );
  TEST_INITIALIZATION_ERROR( MovingImage, ITK_NULLPTR, movingImage );
  TEST_INITIALIZATION_ERROR( Interpolator, ITK_NULLPTR, interpolator );

  std::cout << "Test passed. " << std::endl;
  return EXIT_SUCCESS;

}
