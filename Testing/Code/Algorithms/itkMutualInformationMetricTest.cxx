/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkMutualInformationMetricTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkPhysicalImage.h"
#include "itkSimpleImageRegionIterator.h"

#include "itkImageMapper.h"
#include "itkAffineRegistrationTransform.h"
#include "itkMutualInformationImageToImageMetric.h"

#include <iostream>

/**
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 *  This test computes the mutual information value and derivatives
 *  for various shift values in (-10,10).
 *
 */

int main()
{

//------------------------------------------------------------
// Create two simple images
//------------------------------------------------------------

  //Allocate Images
  typedef itk::PhysicalImage<unsigned char,2>           ReferenceType;
  typedef itk::PhysicalImage<unsigned char,2>           TargetType;
  enum { ImageDimension = ReferenceType::ImageDimension };

  ReferenceType::SizeType size = {{100,100}};
  ReferenceType::IndexType index = {{0,0}};
  ReferenceType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  ReferenceType::Pointer imgReference = ReferenceType::New();
  imgReference->SetLargestPossibleRegion( region );
  imgReference->SetBufferedRegion( region );
  imgReference->SetRequestedRegion( region );
  imgReference->Allocate();

  TargetType::Pointer imgTarget = TargetType::New();
  imgTarget->SetLargestPossibleRegion( region );
  imgTarget->SetBufferedRegion( region );
  imgTarget->SetRequestedRegion( region );
  imgTarget->Allocate();

  // Fill images with a 2D gaussian
  typedef  itk::SimpleImageRegionIterator<ReferenceType>
    ReferenceIteratorType;
  typedef  itk::SimpleImageRegionIterator<TargetType>
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
  displacement[1] =	0;

  ReferenceIteratorType ri(imgReference,region);
  TargetIteratorType ti(imgTarget,region);
  ri.Begin();
  while(!ri.IsAtEnd())
  {
    p[0] = ri.GetIndex()[0];
    p[1] = ri.GetIndex()[1];
	  d = p-center;
	  d += displacement;
	  const double x = d[0];
	  const double y = d[1];
    ri.Set( 200.0 * exp( - ( x*x + y*y )/(s*s) ) );
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
    ti.Set( 200.0 * exp( - ( x*x + y*y )/(s*s) ) );
    ++ti;
  }

//-----------------------------------------------------------
// Set up a transformer
//-----------------------------------------------------------
  enum{ ParametersDimension = ImageDimension * ( ImageDimension + 1 ) };
  typedef itk::Vector<double,ParametersDimension> ParametersType;
  typedef itk::AffineRegistrationTransform<
    double, ImageDimension, ParametersType > TransformationType;

  TransformationType::Pointer transformer = TransformationType::New();

//------------------------------------------------------------
// Set up a mapper
//------------------------------------------------------------
  typedef itk::ImageMapper< ReferenceType, TransformationType >
     MapperType;

  MapperType::Pointer mapper = MapperType::New();

  // connect the transformer to the mapper
  mapper->SetTransformation( transformer );

  // connect the reference image to the mapper
  mapper->SetDomain( imgReference );

//------------------------------------------------------------
// Set up the metric
//------------------------------------------------------------
  typedef itk::MutualInformationImageToImageMetric<
    TargetType, MapperType, double, double > MetricType;

  MetricType::Pointer metric = MetricType::New();

  // connect the mapper to the metric
  metric->SetMapper( mapper );

  // connect the target image to the mapper
  metric->SetTarget( imgTarget );

  // set the standard deviations
  metric->SetTargetStandardDeviation( 20.0 );
  metric->SetReferenceStandardDeviation( 20.0 );

  // set the number of samples to use
  metric->SetNumberOfSpatialSamples( 100 );

//------------------------------------------------------------
// Set up a affine transform parameters
//------------------------------------------------------------
  ParametersType parameters;

  // set the parameters to the identity
  ParametersType::Iterator it = parameters.Begin();

     // initialize the linear/matrix part
  for( unsigned int row = 0; row < ImageDimension; row++ )
    {
    for( unsigned int col = 0; col < ImageDimension; col++ )
      {
      *it = 0;
      if( row == col )
        {
        *it = 1;
        }
      ++it;
      }
    }

     // initialize the offset/vector part
  for( unsigned int k = 0; k < ImageDimension; k++ )
    {
    *it = 0;
    ++it;
    }


//---------------------------------------------------------
// Print out mutual information values
// for parameters[4] = {-10,10}
//---------------------------------------------------------

  MetricType::MeasureType measure;
  MetricType::DerivativeType derivative;

  printf("%s\t%s\t%s\n", "param[4]", "MI", "dMI/dparam[4]" );

  for( double trans = -10; trans <= 5; trans += 0.5 )
    {
    parameters[4] = trans;
    metric->SetParameters( parameters );
    metric->GetValueAndDerivative( measure, derivative );

    printf( "%f\t%f\t%f\n", trans, measure,
      derivative[4] );

    // exercise the other functions
    metric->GetValue();
    metric->GetDerivative();

    }

  return EXIT_SUCCESS;

}

