/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquaresImageMetricTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkGaussianImageSource.h"

#include <iostream>

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
  typedef MovingImageSourceType::Pointer               MovingImageSourcePointer;
  typedef FixedImageSourceType::Pointer                FixedImageSourcePointer;

  // Note: the following declarations are classical arrays
  unsigned long fixedImageSize[]     = {  100,  100 };
  unsigned long movingImageSize[]    = {  100,  100 }; 

  float         fixedImageSpacing[]  = { 1.0f, 1.0f }; 
  float         movingImageSpacing[] = { 1.0f, 1.0f }; 

  float         fixedImageOrigin[]   = { 0.0f, 0.0f }; 
  float         movingImageOrigin[]  = { 0.0f, 0.0f }; 

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
  typedef TransformBaseType::JacobianType           JacobianType;

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
  

 //------------------------------------------------------------
 // Define the scale at which the gradient will be computed
 //------------------------------------------------------------
    metric->SetScaleGradient( 1.0 );
   
  
  std::cout << metric << std::endl;


//------------------------------------------------------------
// This call is mandatory before start querying the Metric
// This method do all the necesary connections between the 
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

  std::cout << "param[1]   Metric    d(Metric)/d(param[1] " << std::endl;

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
  std::cout << "ScaleGradient: " << metric->GetScaleGradient() << std::endl;

  std::cout << "Check case when Target is NULL" << std::endl;
  metric->SetFixedImage( NULL );
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

  TEST_INITIALIZATION_ERROR( Transform, NULL, transform );
  TEST_INITIALIZATION_ERROR( FixedImage, NULL, fixedImage );
  TEST_INITIALIZATION_ERROR( MovingImage, NULL, movingImage );
  TEST_INITIALIZATION_ERROR( Interpolator, NULL, interpolator );
 
  std::cout << "Test passed. " << std::endl;
  return EXIT_SUCCESS;

}

