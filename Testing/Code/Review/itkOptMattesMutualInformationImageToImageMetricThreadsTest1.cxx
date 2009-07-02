/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptMattesMutualInformationImageToImageMetricThreadsTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
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

#ifdef ITK_USE_OPTIMIZED_REGISTRATION_METHODS
  std::cout << "OPTIMIZED ON" << std::endl;
#else
  std::cout << "OPTIMIZED OFF" << std::endl;  
#endif

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

#ifdef ITK_USE_OPTIMIZED_REGISTRATION_METHODS
  unsigned int numberOfSamples = 100;

  if( argc > 4 )
    {
    numberOfSamples = atoi( argv[4] );
    }

  metric->SetNumberOfFixedImageSamples( numberOfSamples );
#endif

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

  MeasureType value;
  DerivativeType derivative;

  MeasureType value1;
  DerivativeType derivative1;

  std::vector< MeasureType > values;
  std::vector< DerivativeType > derivatives;

  // By now restrict the number of threads to test to the range 1 to 4.
  const unsigned int maximumNumberOfThreadsToTest = 5;

  for( unsigned int numberOfThreads = 1; numberOfThreads < maximumNumberOfThreadsToTest; numberOfThreads++ )
    {
    try 
      {
#ifdef ITK_USE_OPTIMIZED_REGISTRATION_METHODS
      metric->SetNumberOfThreads( numberOfThreads );
#endif
      metric->ReinitializeSeed( 76926294 );
      metric->Initialize();
      
      metric->GetValueAndDerivative( displacement, value, derivative );

      value1 = metric->GetValue( displacement );
      metric->GetDerivative( displacement, derivative1 );
      }
    catch( itk::ExceptionObject & excep )
      {
      std::cerr << excep << std::endl;
      return EXIT_FAILURE;
      }

    values.push_back( value );
    derivatives.push_back( derivative );

    if( verbose )
      {
      std::cout << numberOfThreads;
      std::cout << " : " << value;
      std::cout << " : " << value1;
      std::cout << " : " << derivative;
      std::cout << " : " << derivative1 << std::endl;
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

      if( vnl_math_abs( difference ) > tolerance )
        {
        if( verbose )
          {
          std::cerr << i << " : " << j;
          std::cerr << " Differ by " << difference;
          std::cerr << " from " << values[i];
          std::cerr << " to " << values[j];
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
