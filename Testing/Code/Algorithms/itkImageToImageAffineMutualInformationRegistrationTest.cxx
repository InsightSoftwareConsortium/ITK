/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImageToImageAffineMutualInformationRegistrationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkPhysicalImage.h"
#include "itkSimpleImageRegionIterator.h"

#include "itkImageToImageAffineMutualInformationRegistration.h"
#include "vnl/vnl_math.h"

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
// Set up a the registrator
//-----------------------------------------------------------
  typedef itk::ImageToImageAffineMutualInformationRegistration<ReferenceType,TargetType> 
    RegistrationType;

  RegistrationType::Pointer registrationMethod = RegistrationType::New();

  registrationMethod->SetReference(imgReference);
  registrationMethod->SetTarget(imgTarget);

  registrationMethod->SetNumberOfIterations( 500 );
  registrationMethod->SetLearningRate( 0.1 );
  registrationMethod->ScreenDumpOn();
  
  //
  // only allow translation - since the metric will allow any
  // rotation without penalty as image is circular
  //
  RegistrationType::ScalingWeightsPointer weights =
   RegistrationType::ScalingWeightsType::New();

  weights->Reserve( RegistrationType::TransformationType::ParametersDimension );
  for( int j = 0; j < 4; j++ )
    {
    weights->SetElement(j,0.0);
    }
  for( int j=4; j < 6; j++ )
    {
    weights->SetElement(j,1.0);
    }
  registrationMethod->SetScalingWeights( weights );

  registrationMethod->GetMetric()->SetTargetStandardDeviation( 20.0 );
  registrationMethod->GetMetric()->SetReferenceStandardDeviation( 20.0 );
  registrationMethod->GetMetric()->SetNumberOfSpatialSamples( 50 );

  registrationMethod->StartRegistration();
  
  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[6] = { 1, 0, 0, 1, -5, 0 };
  for( int j = 0; j < 4; j++ )
    {
    if( vnl_math_abs( registrationMethod->GetParameters()->GetElement(j) - trueParameters[j] ) > 0.01 ) 
      pass = false;
    }
  for( int j = 4; j < 6; j++ )
    {
    if( vnl_math_abs( registrationMethod->GetParameters()->GetElement(j) - trueParameters[j] ) > 0.5 ) 
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