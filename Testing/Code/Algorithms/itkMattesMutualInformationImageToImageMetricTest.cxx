/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMattesMutualInformationImageToImageMetricTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "itkAffineTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkTextOutput.h"


#include <iostream>

/**
 *  This templated function test the MattesMutualInformationImageToMetric
 *  class using an AfffineTransform and various interpolators.
 *
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 *  This test computes the mutual information value and derivatives
 *  for various shift values in (-10,10). Then it checks the numerical
 *  accuracy of computed derivatives by perturbing parameters by
 *  delta = 0.001.
 *
 *
 */
template< class TImage, class TInterpolator>
int TestMattesMetricWithAffineTransform(
TInterpolator * interpolator )
{

//------------------------------------------------------------
// Create two simple images
//------------------------------------------------------------

  //Allocate Images
  typedef TImage           MovingImageType;
  typedef TImage           FixedImageType;
  enum { ImageDimension = MovingImageType::ImageDimension };

  typename MovingImageType::SizeType size = {{100,100}};
  typename MovingImageType::IndexType index = {{0,0}};
  typename MovingImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  typename MovingImageType::Pointer imgMoving = MovingImageType::New();
  imgMoving->SetLargestPossibleRegion( region );
  imgMoving->SetBufferedRegion( region );
  imgMoving->SetRequestedRegion( region );
  imgMoving->Allocate();

  typename FixedImageType::Pointer imgFixed = FixedImageType::New();
  imgFixed->SetLargestPossibleRegion( region );
  imgFixed->SetBufferedRegion( region );
  imgFixed->SetRequestedRegion( region );
  imgFixed->Allocate();

  // Fill images with a 2D gaussian
  typedef  itk::ImageRegionIterator<MovingImageType>
    ReferenceIteratorType;
  typedef  itk::ImageRegionIterator<FixedImageType>
    TargetIteratorType;

  itk::Point<double,2> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;

  const double s = (double)region.GetSize()[0]/2.0;

  itk::Point<double,2>  p;
  itk::Vector<double,2> d;

  // Set the displacement
  itk::Vector<double,2> displacement;
  displacement[0] = 5;
  displacement[1] = 5;

  ReferenceIteratorType ri(imgMoving,region);
  TargetIteratorType ti(imgFixed,region);
  ri.Begin();
  while(!ri.IsAtEnd())
    {
    p[0] = ri.GetIndex()[0];
    p[1] = ri.GetIndex()[1];
    d = p-center;
    d += displacement;
    const double x = d[0];
    const double y = d[1];
    ri.Set( (unsigned char) ( 200.0 * exp( - ( x*x + y*y )/(s*s) ) ) );
    ++ri;
    }


  ti.Begin();
  while(!ti.IsAtEnd())
    {
    p[0] = ti.GetIndex()[0];
    p[1] = ti.GetIndex()[1];
    d = p-center;
    const double x = d[0];
    const double y = d[1];
    ti.Set( (unsigned char) ( 200.0 * exp( - ( x*x + y*y )/(s*s) ) ) );
    ++ti;
    }

//-----------------------------------------------------------
// Set up a transformer
//-----------------------------------------------------------
  typedef itk::AffineTransform<
    double, ImageDimension > TransformType;
  typedef typename TransformType::ParametersType ParametersType;

  typename TransformType::Pointer transformer = TransformType::New();


//------------------------------------------------------------
// Set up the metric
//------------------------------------------------------------
  typedef itk::MattesMutualInformationImageToImageMetric<
    FixedImageType, MovingImageType > MetricType;

  typename MetricType::Pointer metric = MetricType::New();

  // connect the interpolator
  metric->SetInterpolator( interpolator );

  // connect the transform
  metric->SetTransform( transformer );

  // connect the images to the metric
  metric->SetFixedImage( imgFixed );
  metric->SetMovingImage( imgMoving );

  // set the number of histogram bins
  metric->SetNumberOfHistogramBins( 50 );

  // set the number of samples to use
  metric->SetNumberOfSpatialSamples( 500 );

  // set the region over which to compute metric
  index.Fill(2);
  size.Fill(96);
  region.SetSize( size );
  region.SetIndex( index );
  metric->SetFixedImageRegion( region );

  // initialize the metric before use
  metric->DebugOn();
  metric->Initialize();
  metric->DebugOff();

//------------------------------------------------------------
// Set up a affine transform parameters
//------------------------------------------------------------
  unsigned int numberOfParameters = transformer->GetNumberOfParameters();
  ParametersType parameters( numberOfParameters );

  // set the parameters to the identity
  unsigned long count = 0;

     // initialize the linear/matrix part
  for( unsigned int row = 0; row < ImageDimension; row++ )
    {
    for( unsigned int col = 0; col < ImageDimension; col++ )
      {
      parameters[count] = 0;
      if( row == col )
        {
        parameters[count] = 1;
        }
      ++count;
      }
    }

     // initialize the offset/vector part
  for( unsigned int k = 0; k < ImageDimension; k++ )
    {
    parameters[count] = 0;
    ++count;
    }


//---------------------------------------------------------
// Print out mutual information values
// for parameters[4] = {-10,10} (arbitrary choice)
//---------------------------------------------------------

  typename MetricType::MeasureType measure, measure2;
  typename MetricType::DerivativeType derivative( numberOfParameters );

  std::cout << "param[4]\tMI\tMI2\tdMI/dparam[4]" << std::endl;
  
  for( double trans = -10; trans <= 5; trans += 0.5 )
    {
    parameters[4] = trans;
    metric->GetValueAndDerivative( parameters, measure, derivative );
    measure2 = metric->GetValue( parameters );

    std::cout << trans << "\t" << measure << "\t" <<
      measure2 << "\t" << derivative[4] <<std::endl;

    // exercise the other functions
    metric->GetDerivative( parameters, derivative );

    }


//---------------------------------------------------------
// Check output gradients for numerical accuracy
//---------------------------------------------------------
  parameters[4] = 0;
  metric->GetValueAndDerivative( parameters, measure, derivative );

  ParametersType parametersPlus( numberOfParameters );
  ParametersType parametersMinus( numberOfParameters );
  typename MetricType::MeasureType measurePlus;
  typename MetricType::MeasureType measureMinus;

  double delta = 0.001;

  for( unsigned int i = 0; i < numberOfParameters; ++i )
    {
    //copy the parameters and perturb the current one.
    for( unsigned int j = 0; j < numberOfParameters; ++j )
      {
      if( j == i )
        {
        parametersPlus[j] = parameters[i] + delta;    //positive perturbation
        parametersMinus[j] = parameters[i] - delta;  //negative perturbation
        }
      else
        {
        parametersPlus[j] = parameters[j];
        parametersMinus[j] = parameters[j];
        }
      }

    measurePlus = metric->GetValue( parametersPlus );
    measureMinus = metric->GetValue( parametersMinus );

    double approxDerivative = ( measurePlus - measureMinus ) / ( 2 * delta );
    double ratio = derivative[i]/approxDerivative;

    std::cout << i << "\t";
    std::cout << parameters[i] << "\t";
    std::cout << derivative[i] << "\t";
    std::cout << approxDerivative << "\t";
    std::cout << ratio << "\t";
    std::cout << std::endl;

    if ( vnl_math_abs( ratio - 1.0 ) > 0.01 )
      {
      std::cout << "computed derivative differ from central difference." << std::endl;
      return EXIT_FAILURE;
      }

  }


//-------------------------------------------------------
// exercise misc member functions
//-------------------------------------------------------
  std::cout << "Name of class: " <<
    metric->GetNameOfClass() << std::endl;
  std::cout << "No. of samples used = " << 
    metric->GetNumberOfSpatialSamples() << std::endl;
  std::cout << "No. of histogram bin used = " <<
    metric->GetNumberOfHistogramBins() << std::endl;

  std::cout << metric;

  return EXIT_SUCCESS;

}



int itkMattesMutualInformationImageToImageMetricTest(int, char* [] )
{

  int failed;
  typedef itk::Image<unsigned char,2> ImageType;

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());


  // Test metric with a linear interpolator
  typedef itk::LinearInterpolateImageFunction< ImageType, double >
    LinearInterpolatorType;

  LinearInterpolatorType::Pointer linearInterpolator 
    = LinearInterpolatorType::New();

  failed = TestMattesMetricWithAffineTransform<ImageType,LinearInterpolatorType>(
    linearInterpolator );

  if ( failed ) 
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Test metric with a BSpline interpolator
  typedef itk::BSplineInterpolateImageFunction< ImageType, double >
    BSplineInterpolatorType;

  BSplineInterpolatorType::Pointer bSplineInterpolator
    = BSplineInterpolatorType::New();

  bSplineInterpolator->SetSplineOrder( 3 );

  failed = TestMattesMetricWithAffineTransform<ImageType,BSplineInterpolatorType>(
    bSplineInterpolator );

  if ( failed ) 
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
  

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}
