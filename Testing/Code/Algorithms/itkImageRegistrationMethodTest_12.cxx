/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegistrationMethodTest_12.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkPatternIntensityImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkCommandIterationUpdate.h"

#include "itkImageRegistrationMethodImageSource.h"

/** 
 *  This program tests one instantiation of the itk::ImageRegistrationMethod class
 * 
 *  
 */ 

int itkImageRegistrationMethodTest_12(int argc, char* argv[] )
{

  bool pass = true;

  const unsigned int dimension = 2;

  // Fixed Image Type
  typedef itk::Image<float,dimension>               FixedImageType;

  // Moving Image Type
  typedef itk::Image<float,dimension>               MovingImageType;

  // Size Type
  typedef MovingImageType::SizeType                 SizeType;


  // ImageSource
  typedef itk::testhelper::ImageRegistrationMethodImageSource<
                                  FixedImageType::PixelType,
                                  MovingImageType::PixelType,
                                  dimension >         ImageSourceType;
  // Transform Type
  typedef itk::TranslationTransform< double, dimension > TransformType;
  typedef TransformType::ParametersType             ParametersType;

  // Optimizer Type
  typedef itk::RegularStepGradientDescentOptimizer                  OptimizerType;

  // Metric Type
  typedef itk::PatternIntensityImageToImageMetric< 
                                    FixedImageType, 
                                    MovingImageType >    MetricType;

  // Interpolation technique
  typedef itk:: LinearInterpolateImageFunction< 
                                    MovingImageType,
                                    double >             InterpolatorType;

  // Registration Method
  typedef itk::ImageRegistrationMethod< 
                                    FixedImageType, 
                                    MovingImageType >    RegistrationType;

  typedef itk::CommandIterationUpdate<  
                                  OptimizerType >    CommandIterationType;


  MetricType::Pointer         metric        = MetricType::New();
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  TransformType::Pointer      trasform      = TransformType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  ImageSourceType::Pointer    imageSource   = ImageSourceType::New();

  SizeType size;
  size[0] = 100;
  size[1] = 100;
  
  imageSource->GenerateImages( size );

  FixedImageType::ConstPointer     fixedImage    = imageSource->GetFixedImage();
  MovingImageType::ConstPointer    movingImage   = imageSource->GetMovingImage();

  //
  // Connect all the components required for Registratio
  //
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetFixedImage(    fixedImage    );
  registration->SetMovingImage(   movingImage   );
  registration->SetInterpolator(  interpolator  );


  // Select the Region of Interest over which the Metric will be computed
  // Registration time will be proportional to the number of pixels in this region.
  metric->SetFixedImageRegion( fixedImage->GetBufferedRegion() );

  // Instantiate an Observer to report the progress of the Optimization
  CommandIterationType::Pointer iterationCommand = CommandIterationType::New();
  iterationCommand->SetOptimizer(  optimizer.GetPointer() );

  // Scale the translation components of the Transform in the Optimizer
  OptimizerType::ScalesType scales( transform->GetNumberOfParameters() );
  scales.Fill( 1.0 );

  
  unsigned long   numberOfIterations =   100;
  double          translationScale   =  1e-6;
  double          maximumStepLenght  =  10.0; // no step will be larger than this
  double          minimumStepLenght  =   0.1; // convergence criterion
  double          gradientTolerance  =   0.01; // convergence criterion

  if( argc > 1 )
    {
    numberOfIterations = atol( argv[1] );
    std::cout << "numberOfIterations = " << numberOfIterations << std::endl;
    }
  if( argc > 2 )
    {
    translationScale = atof( argv[2] );
    std::cout << "translationScale = " << translationScale << std::endl;
    }
  if( argc > 3 )
    {
    maximumStepLenght = atof( argv[3] );
    std::cout << "maximumStepLenght = " << maximumStepLenght << std::endl;
    }
  if( argc > 4 )
    {
    minimumStepLenght = atof( argv[4] );
    std::cout << "minimumStepLenght = " << minimumStepLenght << std::endl;
    }
  if( argc > 5 )
    {
    gradientTolerance = atof( argv[5] );
    std::cout << "gradientTolerance = " << gradientTolerance << std::endl;
    }




  for( unsigned int i=0; i<dimension; i++)
    {
    scales[ i ] = translationScale;
    }

  optimizer->SetScales( scales );
  optimizer->SetMaximize( true );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetMinimumStepLength( minimumStepLenght );
  optimizer->SetMaximumStepLength( maximumStepLenght );
  optimizer->SetGradientMagnitudeTolerance( gradientTolerance );

  // Start from an Identity transform (in a normal case, the user 
  // can probably provide a better guess than the identity...
  transform->SetIdentity();
  registration->SetInitialTransformParameters( transform->GetParameters() );

  // Initialize the internal connections of the registration method. 
  // This can potentially throw an exception
  try
    {
    registration->StartRegistration();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << e << std::endl;
    pass = false;
    }

  ParametersType actualParameters = imageSource->GetActualParameters();
  ParametersType finalParameters  = registration->GetLastTransformParameters();

  const unsigned int numbeOfParameters = actualParameters.Size();


  const double tolerance = 1.0;  // equivalent to 1 pixel.

  for(unsigned int i=0; i<numbeOfParameters; i++) 
    {
    // the parameters are negated in order to get the inverse transformation.
    // this only works for comparing translation parameters....
    std::cout << finalParameters[i] << " == " << -actualParameters[i] << std::endl;
    if( fabs ( finalParameters[i] - (-actualParameters[i]) ) > tolerance )
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

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;


}
