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
#include "itkImageFileReader.h"
#include "itkTranslationTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkMattesMutualInformationImageToImageMetric.h"

int itkOptMattesMutualInformationImageToImageMetricThreadsTest1( int argc, char* argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage " << std::endl;
    std::cerr << argv[0] << " fixedImage movingImage [verbose(1/0)] [numberOfSamples]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "OPTIMIZED ON" << std::endl;

  const unsigned int maximumNumberOfThreads = itk::MultiThreader::GetGlobalMaximumNumberOfThreads();
  const unsigned int defaultNumberOfThreads = itk::MultiThreader::GetGlobalDefaultNumberOfThreads();

  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << " Global Maximum Number of Threads " << maximumNumberOfThreads << std::endl;
  std::cout << " Global Default Number of Threads " << defaultNumberOfThreads << std::endl;
  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << std::endl;


  typedef unsigned char  PixelType;
  const unsigned int     Dimension = 2;

  typedef itk::Image< PixelType > ImageType;

  typedef itk::ImageFileReader< ImageType  > ImageReaderType;

  ImageReaderType::Pointer fixedImageReader  = ImageReaderType::New();
  ImageReaderType::Pointer movingImageReader = ImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  bool verbose = false;

  if( argc > 3 )
    {
    verbose = atoi( argv[3] );
    }

  try
    {
    fixedImageReader->Update();
    movingImageReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::NearestNeighborInterpolateImageFunction< ImageType > InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  typedef itk::MattesMutualInformationImageToImageMetric< ImageType, ImageType > MetricType;
  MetricType::Pointer metric = MetricType::New();

  typedef itk::TranslationTransform< double, Dimension >  TranformType;
  TranformType::Pointer transform = TranformType::New();

  unsigned int numberOfSamples = 100;

  if( argc > 4 )
    {
    numberOfSamples = atoi( argv[4] );
    }
  metric->SetNumberOfFixedImageSamples( numberOfSamples );

  metric->SetTransform( transform );
  metric->SetInterpolator( interpolator );
  metric->SetFixedImage( fixedImageReader->GetOutput() );
  metric->SetMovingImage( movingImageReader->GetOutput() );
  metric->SetFixedImageRegion(  fixedImageReader->GetOutput()->GetBufferedRegion()  );

  MetricType::TransformParametersType displacement( Dimension );

  displacement[0] = 17;
  displacement[1] = 19;

  typedef MetricType::MeasureType      MeasureType;
  typedef MetricType::DerivativeType   DerivativeType;

  MeasureType value_combined;
  DerivativeType derivative_combined;

  MeasureType value_separate;
  DerivativeType derivative_separate;

  std::vector< MeasureType > values;
  std::vector< DerivativeType > derivatives;

  // By now restrict the number of threads to test to the range 1 to 4.
  const unsigned int maximumNumberOfThreadsToTest = defaultNumberOfThreads;

  for( unsigned int numberOfThreads = 1; numberOfThreads < maximumNumberOfThreadsToTest; numberOfThreads++ )
    {
    try
      {
      metric->SetNumberOfThreads( numberOfThreads );
      metric->ReinitializeSeed( 76926294 );
      metric->Initialize();


      value_separate = metric->GetValue( displacement );
      metric->GetDerivative( displacement, derivative_separate );
      metric->GetValueAndDerivative( displacement, value_combined, derivative_combined );
      }
    catch( itk::ExceptionObject & excep )
      {
      std::cerr << excep << std::endl;
      return EXIT_FAILURE;
      }

    values.push_back( value_combined );
    derivatives.push_back( derivative_combined );

    if( verbose )
      {
      std::cout << numberOfThreads;
      std::cout << " : " << value_combined;
      std::cout << " : " << value_separate;
      std::cout << " : " << derivative_combined;
      std::cout << " : " << derivative_separate << std::endl;
      std::cout << std::endl << std::endl;
      }
    }

  bool testFailed = false;

  const double tolerance = 1e-7;

  for( unsigned int i = 0; i < values.size(); i++ )
    {
    for( unsigned int j = i; j < values.size(); j++ )
      {
      const double difference = values[i] - values[j];

      if( itk::Math::abs( difference ) > tolerance )
        {
        if( verbose )
          {
          std::cerr << i+1  << "thread vs. " << j+1;
          std::cerr << " thread differ by " << difference;
          std::cerr << " from " << values[i];
          std::cerr << " to " << values[j];
          std::cerr << "   ## Derivatives " << derivatives[i] << " vs. " << derivatives[j];
          std::cerr << std::endl;
          }
        testFailed = true;
        }
      }
    }

  if( testFailed )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
