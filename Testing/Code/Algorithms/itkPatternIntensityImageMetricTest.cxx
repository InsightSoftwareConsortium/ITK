/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPatternIntensityImageMetricTest.cxx
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
#include "itkPatternIntensityImageToImageMetric.h"
#include "itkGaussianImageSource.h"

#include <iostream>

/**
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 *  This test computes the PatternIntensity value and derivatives
 *  for various shift values in (-10,10).
 *
 */

int itkPatternIntensityImageMetricTest(int, char* [] )
{

//------------------------------------------------------------
// Create two simple images
//------------------------------------------------------------

  const unsigned int ImageDimension = 2;

  typedef unsigned char            PixelType;

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

  fixedImageSource->SetSize(    fixedImageSize    );
  fixedImageSource->SetOrigin(  fixedImageOrigin  );
  fixedImageSource->SetSpacing( fixedImageSpacing );
  fixedImageSource->SetNormalized( true );
  fixedImageSource->SetScale( 1.0f );

  movingImageSource->SetSize(    movingImageSize    );
  movingImageSource->SetOrigin(  movingImageOrigin  );
  movingImageSource->SetSpacing( movingImageSpacing );
  movingImageSource->SetNormalized( true );
  movingImageSource->SetScale( 1.0f );

  movingImageSource->Update();  // Force the filter to run
  fixedImageSource->Update();   // Force the filter to run

  MovingImageType::Pointer movingImage = movingImageSource->GetOutput();
  FixedImageType::Pointer  fixedImage  = fixedImageSource->GetOutput();


//-----------------------------------------------------------
// Set up  the Metric
//-----------------------------------------------------------
  typedef itk::PatternIntensityImageToImageMetric<  
                                       FixedImageType, 
                                       MovingImageType >   
                                                    MetricType;

  typedef MetricType::TransformType                 TransformBaseType;
  typedef TransformBaseType::ParametersType         ParametersType;
  typedef TransformBaseType::JacobianType           JacobianType;

  MetricType::Pointer  metric = MetricType::New();
  metric->Print(std::cout);

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
// The lambda value is the intensity difference that should
// make the metric drop by 50%
//------------------------------------------------------------
   metric->SetLambda( 10 );


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

  for( double trans = -10; trans <= 5; trans += 0.5 )
    {
    parameters[1] = trans;
    metric->GetValueAndDerivative( parameters, measure, derivative );

    std::cout.width(8);
    std::cout << trans;
    std::cout.width(8);
    std::cout << measure;
    std::cout.width(8);
    std::cout << derivative[1];

    // exercise the other functions
    metric->GetValue( parameters );
    metric->GetDerivative( parameters, derivative );

    }

//-------------------------------------------------------
// exercise misc member functions
//-------------------------------------------------------
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

  return EXIT_SUCCESS;

}

