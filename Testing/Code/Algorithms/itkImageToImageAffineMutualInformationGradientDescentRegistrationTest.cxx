/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImageToImageAffineMutualInformationGradientDescentRegistrationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkPhysicalImage.h"
#include "itkImageRegionIterator.h"

#include "itkImageToImageAffineMutualInformationGradientDescentRegistration.h"
#include "vnl/vnl_math.h"

#include <iostream>

int main()
{

//------------------------------------------------------------
// Create two simple images
// Two Gaussians with one translated (7,3) pixels from another
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
  typedef  itk::ImageRegionIterator<ReferenceType>
    ReferenceIteratorType;
  typedef  itk::ImageRegionIterator<TargetType>
    TargetIteratorType;

  itk::Point<double,2> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;

  const double s = (double)region.GetSize()[0]/2.0;

  itk::Point<double,2>  p;
  itk::Vector<double,2> d;

  // Set the displacement
  itk::Vector<double,2> displacement;
  displacement[0] = 7;
  displacement[1] =	3;

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
    ri.Set( (unsigned char)( 200.0 * exp( - ( x*x + y*y )/(s*s) ) ) );
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
    ti.Set( (unsigned char)( 200.0 * exp( - ( x*x + y*y )/(s*s) ) ) );
    ++ti;
  }

//-----------------------------------------------------------
// Set up a the registrator
//-----------------------------------------------------------
  typedef itk::ImageToImageAffineMutualInformationGradientDescentRegistration<
    ReferenceType,TargetType> RegistrationType;

  RegistrationType::Pointer registrationMethod = RegistrationType::New();

  // connect the images
  registrationMethod->SetReference(imgReference);
  registrationMethod->SetTarget(imgTarget);

  // set the transformation centers
  RegistrationType::PointType transCenter;
  for( unsigned int j = 0; j < 2; j++ )
    {
    transCenter[j] = double(size[j]) / 2;
    }

  registrationMethod->SetTargetTransformationCenter( transCenter );
  registrationMethod->SetReferenceTransformationCenter( transCenter );

  // set optimization related parameters
  registrationMethod->SetNumberOfIterations( 500 );
  registrationMethod->SetLearningRate( 0.2 );

  //
  // only allow translation - since the metric will allow any
  // rotation without penalty as image is circular
  //
  RegistrationType::ParametersType weights;

  for( unsigned int j = 0; j < 4; j++ )
    {
    weights[j] = 0.0;
    }
  for( unsigned int j=4; j < 6; j++ )
    {
    weights[j] = 1.0;
    }
  registrationMethod->SetScalingWeights( weights );


  // set metric related parameters
  registrationMethod->GetMetric()->SetTargetStandardDeviation( 20.0 );
  registrationMethod->GetMetric()->SetReferenceStandardDeviation( 20.0 );
  registrationMethod->GetMetric()->SetNumberOfSpatialSamples( 50 );

  // start registration
  registrationMethod->StartRegistration();

  // get the results
  RegistrationType::ParametersType solution = 
    registrationMethod->GetParameters();

  std::cout << "Solution is: " << solution << std::endl;

  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[6] = { 1, 0, 0, 1, -7, -3 };
  for( unsigned int j = 0; j < 4; j++ )
    {
    if( vnl_math_abs( solution[j] - trueParameters[j] ) > 0.02 )
      pass = false;
    }
  for( unsigned int j = 4; j < 6; j++ )
    {
    if( vnl_math_abs( solution[j] - trueParameters[j] ) > 1.0 )
      pass = false;
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
