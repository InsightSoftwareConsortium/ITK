/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionImageRegistrationMethodTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"

#include "itkTextOutput.h"


/** 
 *  This program test one instantiation of the 
 *  itk::MultiResolutionImageRegistrationMethod class
 * 
 *  This file tests initialization errors.
 */ 

int itkMultiResolutionImageRegistrationMethodTest(int, char* [] )
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  bool pass;

  const unsigned int dimension = 3;

  // Fixed Image Type
  typedef itk::Image<float,dimension>                    FixedImageType;

  // Moving Image Type
//  typedef itk::Image<char,dimension>                     MovingImageType;
  typedef itk::Image<float,dimension>                     MovingImageType;

  // Transform Type
  typedef itk::TranslationTransform< double, dimension > TransformType;

  // Optimizer Type
  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;

  // Metric Type
  typedef itk::MeanSquaresImageToImageMetric< 
                                    FixedImageType, 
                                    MovingImageType >    MetricType;

  // Interpolation technique
  typedef itk:: LinearInterpolateImageFunction< 
                                    MovingImageType,
                                    double          >    InterpolatorType;

  // Fixed Image Pyramid Type
  typedef itk::RecursiveMultiResolutionPyramidImageFilter<
                                    FixedImageType,
                                    FixedImageType  >    FixedImagePyramidType;

  // Moving Image Pyramid Type
  typedef itk::RecursiveMultiResolutionPyramidImageFilter<
                                    MovingImageType,
                                    MovingImageType  >   MovingImagePyramidType;


  // Registration Method
  typedef itk::MultiResolutionImageRegistrationMethod< 
                                    FixedImageType, 
                                    MovingImageType >    RegistrationType;


  MetricType::Pointer         metric        = MetricType::New();
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  FixedImageType::Pointer     fixedImage    = FixedImageType::New();  
  MovingImageType::Pointer    movingImage   = MovingImageType::New();  
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  FixedImagePyramidType::Pointer fixedImagePyramid = 
    FixedImagePyramidType::New();
  MovingImagePyramidType::Pointer movingImagePyramid =
    MovingImagePyramidType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  FixedImageType::SizeType size;
  size.Fill( 8 );

  FixedImageType::RegionType region( size );
  fixedImage->SetRegions( region );
  fixedImage->Allocate();
  fixedImage->FillBuffer( 3.0 );

  movingImage->SetRegions( region );
  movingImage->Allocate();
  movingImage->FillBuffer( 4.0 );

  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetFixedImage(    fixedImage    );
  registration->SetMovingImage(   movingImage   );
  registration->SetInterpolator(  interpolator  );
  registration->SetFixedImagePyramid( fixedImagePyramid );
  registration->SetMovingImagePyramid( movingImagePyramid );
  registration->SetFixedImageRegion( fixedImage->GetBufferedRegion() );

  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );
  initialParameters.Fill(0);

  registration->SetInitialTransformParameters( initialParameters );

  registration->SetNumberOfLevels( 2 );

  registration->Print( std::cout );


  // Exercise Get methods
  std::cout << "metric: " << registration->GetMetric() << std::endl;
  std::cout << "optimizer: " << registration->GetOptimizer() << std::endl;
  std::cout << "transform: " << registration->GetTransform() << std::endl;
  std::cout << "fixed image: " << registration->GetFixedImage() << std::endl;
  std::cout << "moving image: " << registration->GetMovingImage() << std::endl;
  std::cout << "interpolator: " << registration->GetInterpolator() << std::endl;
  std::cout << "fixed image region: " << registration->GetFixedImageRegion() << std::endl;
  std::cout << "fixed image pyramid: " << registration->GetFixedImagePyramid() << std::endl;
  std::cout << "moving image pyramid: " << registration->GetMovingImagePyramid() << std::endl;

  std::cout << "initial parameters: ";
  std::cout << registration->GetInitialTransformParameters() << std::endl;

  std::cout << "no. levels: " << registration->GetNumberOfLevels() << std::endl;
  std::cout << "current level: " << registration->GetCurrentLevel() << std::endl;


  /****************************************************
   * Test out initialization errors
   ****************************************************/

#define TEST_INITIALIZATION_ERROR( ComponentName, badComponent, goodComponent ) \
  registration->Set##ComponentName( badComponent ); \
  try \
    { \
    pass = false; \
    registration->StartRegistration(); \
    } \
  catch( itk::ExceptionObject& err ) \
    { \
    std::cout << "Caught expected ExceptionObject" << std::endl; \
    std::cout << err << std::endl; \
    pass = true; \
    } \
  registration->Set##ComponentName( goodComponent ); \
  \
  if( !pass ) \
    { \
    std::cout << "Test failed." << std::endl; \
    return EXIT_FAILURE; \
    } 

  TEST_INITIALIZATION_ERROR( Metric, NULL, metric );
  TEST_INITIALIZATION_ERROR( Optimizer, NULL, optimizer );
  TEST_INITIALIZATION_ERROR( Transform, NULL, transform );
  TEST_INITIALIZATION_ERROR( FixedImage, NULL, fixedImage );
  TEST_INITIALIZATION_ERROR( MovingImage, NULL, movingImage );
  TEST_INITIALIZATION_ERROR( Interpolator, NULL, interpolator );
  TEST_INITIALIZATION_ERROR( FixedImagePyramid, NULL, fixedImagePyramid );
  TEST_INITIALIZATION_ERROR( MovingImagePyramid, NULL, movingImagePyramid );
  TEST_INITIALIZATION_ERROR( InitialTransformParameters, ParametersType(1),
                                                         initialParameters );


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}
