/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImageToImageTranslationMeanSquaresGradientDescentRegistrationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkPhysicalImage.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkImageToImageTranslationMeanSquaresGradientDescentRegistration.h"

/** 
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 10 pixels from the other.
 *  therefore the solution of the registration is |-5 0|
 *  This test uses LevenbergMarquart Optimizer but
 *  conjugate gradient optimizer tolerances are also defined
 *  in the itkImageToImageTranslationMeanSquaresGradientDescentRegistration.txx file
 *  (you need to change the type of the optimizer in the header file
 *  ie itkImageToImageTranslationMeanSquaresGradientDescentRegistration.h)
 */ 

int main()
{

  /*Allocate Images*/
  typedef itk::PhysicalImage<unsigned char,2>           ReferenceType;
  typedef itk::PhysicalImage<unsigned char,2>           TargetType;

  typedef itk::ImageToImageTranslationMeanSquaresGradientDescentRegistration<ReferenceType,TargetType> RegistrationType;

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

  /* Fill images with a 2D gaussian*/
  typedef  itk::SimpleImageRegionIterator<ReferenceType> ReferenceIteratorType;

  typedef  itk::SimpleImageRegionIterator<TargetType> TargetIteratorType;


  itk::Point<double,2> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;

  const double s = (double)region.GetSize()[0]/2.0;

  itk::Point<double,2>  p;
  itk::Vector<double,2> d;

  /* Set the displacement */
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

  

  RegistrationType::Pointer registrationMethod = RegistrationType::New();

  registrationMethod->SetReference(imgReference);
  registrationMethod->SetTarget(imgTarget);

  registrationMethod->GetOptimizer()->SetLearningRate(2e4);
  registrationMethod->GetOptimizer()->SetNumberOfIterations(500);

  registrationMethod->StartRegistration();


  // get the results
  RegistrationType::ParametersType solution = 
    registrationMethod->GetOptimizer()->GetCurrentPosition();

  std::cout << "Solution is: " << solution << std::endl;

  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[2] = { -7, -3 };
  for( unsigned int j = 0; j < 2; j++ )
    {
    if( vnl_math_abs( solution[j] - trueParameters[j] ) > 0.02 )
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
