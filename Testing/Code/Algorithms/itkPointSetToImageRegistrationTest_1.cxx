/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageRegistrationTest_1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef _WIN32
#pragma warning ( disable : 4786 )
#endif

#include "itkImageRegionIterator.h"
#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNormalizedCorrelationPointSetToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkGaussianImageSource.h"
#include "itkImage.h"
#include "itkPointSet.h"
#include "itkPointSetToImageRegistrationMethod.h"
#include "itkCommandIterationUpdate.h"
#include "itkImageRegistrationMethodImageSource.h"

#include <iostream>

/**
 *
 *  This program tests the registration of a PointSet against an image. 
 *
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 */

int itkPointSetToImageRegistrationTest_1(int, char* [] )
{

//------------------------------------------------------------
// Create two simple images
//------------------------------------------------------------

  const unsigned int ImageDimension = 2;

  typedef double                   PixelType;

  typedef double                   CoordinateRepresentationType;

  //Allocate Images
  typedef itk::Image<PixelType,ImageDimension>   MovingImageType;
  typedef itk::Image<PixelType,ImageDimension>   FixedImageType;

  // ImageSource
  typedef itk::testhelper::ImageRegistrationMethodImageSource<
    PixelType,
    PixelType,
    ImageDimension >   ImageSourceType;

  ImageSourceType::Pointer    imageSource   = ImageSourceType::New();

  itk::Size<ImageDimension> size;
  size[0] = 100;
  size[1] = 100;
  
  imageSource->GenerateImages( size );

  MovingImageType::ConstPointer movingImage = imageSource->GetMovingImage();
  FixedImageType::ConstPointer  fixedImage  = imageSource->GetFixedImage();

//-----------------------------------------------------------
// Create the point set and load it with data by sampling 
// the fixed image
//-----------------------------------------------------------
  typedef itk::PointSet< float, 2 >   FixedPointSetType;
  FixedPointSetType::Pointer fixedPointSet = FixedPointSetType::New();

  const unsigned int numberOfPoints = 10000;

  fixedPointSet->SetPointData( FixedPointSetType::PointDataContainer::New() );

  fixedPointSet->GetPoints()->Reserve( numberOfPoints );
  fixedPointSet->GetPointData()->Reserve( numberOfPoints );

  typedef itk::ImageRegionConstIterator< FixedImageType > ImageIteratorType;

  ImageIteratorType it( fixedImage, fixedImage->GetBufferedRegion() );

  const unsigned int skip = 
    fixedImage->GetBufferedRegion().GetNumberOfPixels() / numberOfPoints;

  unsigned int counter = 0;

  FixedPointSetType::PointIdentifier pointId = 0;
  FixedPointSetType::PointType  point;

  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    if( counter==0 )
      {
      fixedImage->TransformIndexToPhysicalPoint( it.GetIndex(), point );
      fixedPointSet->SetPoint( pointId, point );
      fixedPointSet->SetPointData( pointId, it.Get() );
      ++pointId; 
      if( pointId == numberOfPoints )
        {
        break;
        }
      counter = skip;
      }
    --counter;
    ++it;
    }

//-----------------------------------------------------------
// Set up  the Metric
//-----------------------------------------------------------
  typedef itk::NormalizedCorrelationPointSetToImageMetric<  
    FixedPointSetType, 
    MovingImageType >   
    MetricType;

  typedef MetricType::TransformType                 TransformBaseType;
  typedef TransformBaseType::ParametersType         ParametersType;
  typedef TransformBaseType::JacobianType           JacobianType;

  MetricType::Pointer  metric = MetricType::New();

  metric->SetScaleGradient( 1.0 );



//-----------------------------------------------------------
// Set up a Transform
//-----------------------------------------------------------

  typedef itk::TranslationTransform< 
    CoordinateRepresentationType, 
    ImageDimension >         TransformType;

  TransformType::Pointer transform = TransformType::New();


//------------------------------------------------------------
// Set up an Interpolator
//------------------------------------------------------------
  typedef itk::LinearInterpolateImageFunction< 
    MovingImageType,
    double > InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();



  // Optimizer Type
  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;

  OptimizerType::Pointer      optimizer     = OptimizerType::New();



  // Registration Method
  typedef itk::PointSetToImageRegistrationMethod< 
    FixedPointSetType, 
    MovingImageType >    RegistrationType;


  RegistrationType::Pointer   registration  = RegistrationType::New();


  typedef itk::CommandIterationUpdate<  
    OptimizerType >    CommandIterationType;


  // Instantiate an Observer to report the progress of the Optimization
  CommandIterationType::Pointer iterationCommand = CommandIterationType::New();
  iterationCommand->SetOptimizer(  optimizer.GetPointer() );


  // Scale the translation components of the Transform in the Optimizer
  OptimizerType::ScalesType scales( transform->GetNumberOfParameters() );
  scales.Fill( 1.0 );

  
  unsigned long   numberOfIterations =   50;
  //double          translationScale   =  1.0; // not used
  double          maximumStepLenght  =  1.0;  // no step will be larger than this
  double          minimumStepLenght  =  0.01; // convergence criterion
  double          gradientTolerance  =  1e-6; // convergence criterion


  optimizer->SetScales( scales );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetMinimumStepLength( minimumStepLenght );
  optimizer->SetMaximumStepLength( maximumStepLenght );
  optimizer->SetGradientMagnitudeTolerance( gradientTolerance );
  optimizer->MinimizeOn();

  // Start from an Identity transform (in a normal case, the user 
  // can probably provide a better guess than the identity...
  transform->SetIdentity();
  registration->SetInitialTransformParameters( transform->GetParameters() );





  //------------------------------------------------------
  // Connect all the components required for Registration
  //------------------------------------------------------
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetFixedPointSet( fixedPointSet );
  registration->SetMovingImage(   movingImage   );
  registration->SetInterpolator(  interpolator  );




//------------------------------------------------------------
// Set up transform parameters
//------------------------------------------------------------
  ParametersType parameters( transform->GetNumberOfParameters() );

  // initialize the offset/vector part
  for( unsigned int k = 0; k < ImageDimension; k++ )
    {
    parameters[k]= 0.0f;
    }


  try 
    {
    registration->StartRegistration();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << e << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}

