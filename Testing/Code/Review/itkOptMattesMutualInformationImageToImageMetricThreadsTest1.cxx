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

int itkOptMattesMutualInformationImageToImageMetricThreadsTest1( int , char* argv[] )
{
#ifdef ITK_USE_OPTIMIZED_REGISTRATION_METHODS
  std::cout << "OPTIMIZED ON" << std::endl;
#else
  std::cout << "OPTIMIZED OFF" << std::endl;  
#endif

  const unsigned int maximumNumberOfThreads = itk::MultiThreader::GetGlobalMaximumNumberOfThreads();
  const unsigned int defaultNumberOfThreads = itk::MultiThreader::GetGlobalDefaultNumberOfThreads();

  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << "\t Global Maximum Number of Threads " << maximumNumberOfThreads << std::endl;
  std::cout << "\t Global Default Number of Threads " << defaultNumberOfThreads << std::endl;
  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << std::endl;


  typedef unsigned char  PixelType;
  const unsigned int     Dimension = 2;

  typedef itk::Image< PixelType > ImageType;

  typedef itk::ImageFileReader< ImageType  > ImageReaderType;

  ImageReaderType::Pointer fixedImageReader  = ImageReaderType::New();
  ImageReaderType::Pointer movingImageReader = ImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

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

  metric->SetTransform( transform );
  metric->SetInterpolator( interpolator );
  metric->SetFixedImage( fixedImageReader->GetOutput() ); 
  metric->SetMovingImage( movingImageReader->GetOutput() ); 
  metric->SetFixedImageRegion(  fixedImageReader->GetOutput()->GetBufferedRegion()  );

  try 
    {
    metric->Initialize();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  MetricType::TransformParametersType displacement( Dimension );

  displacement[0] = 17;
  displacement[1] = 19;

  for( unsigned int numberOfThreads = 1; numberOfThreads < maximumNumberOfThreads; numberOfThreads++ )
    {
    metric->SetNumberOfThreads( numberOfThreads );
    const double value = metric->GetValue( displacement );
    std::cout << numberOfThreads << " : " << value << std::endl;
    }

  return EXIT_SUCCESS;
}
