/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageTranslationNormalizedCorrelationGradientDescentRegistrationTest.cxx
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
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageToImageTranslationNormalizedCorrelationGradientDescentRegistration.h"

#include "itkCommandIterationUpdate.h"

/** 
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 10 pixels from the other.
 *  therefore the solution of the registration is |-5 0|
 *  This test uses LevenbergMarquart Optimizer but
 *  conjugate gradient optimizer tolerances are also defined
 *  in the itkImageToImageTranslationNormalizedCorrelationGradientDescentRegistration.txx file
 *  (you need to change the type of the optimizer in the header file
 *  ie itkImageToImageTranslationNormalizedCorrelationGradientDescentRegistration.h)
 */ 

int main()
{

  /*Allocate Images*/
  typedef itk::Image<unsigned char,2>           ReferenceType;
  typedef itk::Image<unsigned char,2>           TargetType;

  typedef itk::ImageToImageTranslationNormalizedCorrelationGradientDescentRegistration<
                                               ReferenceType,TargetType> RegistrationType;


  typedef itk::CommandIterationUpdate<  RegistrationType::OptimizerType  >
                                                           CommandIterationType;

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
  typedef  itk::ImageRegionIteratorWithIndex<ReferenceType> ReferenceIteratorType;

  typedef  itk::ImageRegionIteratorWithIndex<TargetType> TargetIteratorType;


  itk::Point<double,2> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;

  const double s = (double)region.GetSize()[0]/2.0;

  itk::Point<double,2>  p;
  itk::Vector<double,2> d;

  /* Set the displacement */
  itk::Vector<double,2> displacement;
  displacement[0] = 7;
  displacement[1] = 3;

  ReferenceIteratorType ri(imgReference,region);
  TargetIteratorType ti(imgTarget,region);
  while(!ri.IsAtEnd())
  {
    p[0] = ri.GetIndex()[0];
    p[1] = ri.GetIndex()[1];
    d = p-center;
    d += displacement;
    const double x = d[0];
    const double y = d[1];
    const double value = 200.0 * exp( - ( x*x + y*y )/(s*s) );
    ri.Set( static_cast<ReferenceType::PixelType>( value ) );
    ++ri;
  }


  while(!ti.IsAtEnd())
  {
    p[0] = ti.GetIndex()[0];
    p[1] = ti.GetIndex()[1];
    d = p-center;
    const double x = d[0];
    const double y = d[1];
    const double value = 200.0 * exp( - ( x*x + y*y )/(s*s) );
    ti.Set( static_cast<ReferenceType::PixelType>( value ) );
    ++ti;
  }

  

  RegistrationType::Pointer registrationMethod = RegistrationType::New();

  CommandIterationType::Pointer iterationCommand = CommandIterationType::New();

  iterationCommand->SetOptimizer(  registrationMethod->GetOptimizer() );

  registrationMethod->GetOptimizer()->AddObserver( itk::IterationEvent(),
                                                   iterationCommand ); 


  registrationMethod->SetReference(imgReference);
  registrationMethod->SetTarget(imgTarget);

  registrationMethod->GetOptimizer()->SetLearningRate(200);
  registrationMethod->GetOptimizer()->SetNumberOfIterations(100);

  registrationMethod->StartRegistration();


  // get the results
  RegistrationType::ParametersType solution = 
    registrationMethod->GetOptimizer()->GetCurrentPosition();


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
