/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionMutualInformationAffineRegistrationTest.cxx
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
#include "itkMultiResolutionMutualInformationAffineRegistration.h"
#include "itkCommandIterationUpdate.h"
#include "itkOutputWindow.h"

#include <iostream>
namespace
{
// this class is used to send output to stdout and not the itk window
class TextOutput : public itk::OutputWindow
{
public:
  typedef TextOutput              Self;
  typedef itk::SmartPointer<Self>  Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;
  itkNewMacro(TextOutput);
  virtual void DisplayText(const char* s)
    {
      std::cout << s << std::endl;
    }
};


/**
 * This function defines the test image pattern.
 * The pattern is a 3D gaussian in the middle
 * and some directional pattern on the outside.
 */
double F( double x, double y, double z )
{
  const double s = 50;
  double value = 200.0 * exp( - ( x*x + y*y + z*z )/(s*s) );
  x -= 8; y += 3; z += 0;
  double r = vnl_math_sqrt( x*x + y*y + z*z );
  if( r > 35 )
    {
    value = 2 * ( vnl_math_abs( x ) + 
      0.8 * vnl_math_abs( y ) +
      0.5 * vnl_math_abs( z ) );
    }
  if( r < 4 )
    {
    value = 400;
    }

  return value;

}
}


int itkMultiResolutionMutualInformationAffineRegistrationTest(int, char**)
{

  itk::OutputWindow::SetInstance(TextOutput::New().GetPointer());

//------------------------------------------------------------
// Create two simple images
// Translate and dilate one of the image
//------------------------------------------------------------

  // Allocate Images
  typedef float PixelType;
  typedef itk::Image<PixelType,3>           ReferenceType;
  typedef itk::Image<PixelType,3>           TargetType;
  enum { ImageDimension = ReferenceType::ImageDimension };

  ReferenceType::SizeType size = {{100,100,40}};
  ReferenceType::IndexType index = {{0,0,0}};
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

  // Fill images with a 3D gaussian with some directional pattern
  // in the background
  typedef  itk::ImageRegionIterator<ReferenceType>
    ReferenceIteratorType;
  typedef  itk::ImageRegionIterator<TargetType>
    TargetIteratorType;

  itk::Point<double,3> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;
  center[2] = (double)region.GetSize()[2]/2,0;

  itk::Point<double,3>  p;
  itk::Vector<double,3> d;

  // Set the displacement and scale
  itk::Vector<double,3> displacement;
  displacement[0] = 7;
  displacement[1] = 3;
  displacement[2] = 2;
// Thirty percent dilation
//  double scale[3] = { 0.70, 1.0, 1.0};
// Twenty percent dilation - use this so system test don't run for too long
  double scale[3] = { 0.80, 1.0, 1.0 };

  ReferenceIteratorType ri(imgReference,region);
  TargetIteratorType ti(imgTarget,region);

  while(!ri.IsAtEnd())
  {
    p[0] = ri.GetIndex()[0];
    p[1] = ri.GetIndex()[1];
    p[2] = ri.GetIndex()[2];
    d = p-center;
    const double x = d[0] * scale[0] + displacement[0];
    const double y = d[1] * scale[1] + displacement[1];
    const double z = d[2] * scale[2] + displacement[2];
    ri.Set( (PixelType) F(x,y,z) );
    ++ri;
  }


  while(!ti.IsAtEnd())
  {
    p[0] = ti.GetIndex()[0];
    p[1] = ti.GetIndex()[1];
    p[2] = ti.GetIndex()[2];
    d = p-center;
    const double x = d[0];
    const double y = d[1];
    const double z = d[2];
    ti.Set( (PixelType) F(x,y,z) );
    ++ti;
  }

  // set image origin to be center of the image
  double transCenter[3];
  for( unsigned int j = 0; j < 3; j++ )
    {
    transCenter[j] = -0.5 * double(size[j]);
    }

  imgReference->SetOrigin( transCenter );
  imgTarget->SetOrigin( transCenter );


 /**
  * Setup the registrator
  */
  typedef itk::MultiResolutionMutualInformationAffineRegistration<
    ReferenceType,TargetType> MRRegistrationType;
  typedef MRRegistrationType::RegistrationType InternalRegistrationType;

  MRRegistrationType::Pointer registrator = MRRegistrationType::New();

  registrator->SetTarget( imgTarget );
  registrator->SetReference( imgReference );
  registrator->SetNumberOfLevels( 3 );

// This set of parameters can recover thirty percent dilation
//  unsigned int niter[4] = { 500, 3000, 3000 };
//  double rates[4] = { 1e-3, 5e-4, 1e-4 };

// This set of parameters can recover twenty percent dilation
  unsigned int niter[4] = { 100, 300, 550 };
  double rates[4] = { 1e-3, 5e-4, 1e-4 };

  registrator->SetNumberOfIterations( niter );
  registrator->SetLearningRates( rates );


  MRRegistrationType::RegistrationPointer method = 
    registrator->GetInternalRegistrationMethod();

  // Define the type for the observer command to monitor progress
  typedef itk::CommandIterationUpdate<  InternalRegistrationType::OptimizerType  >
                                                           CommandIterationType;

  CommandIterationType::Pointer iterationCommand = CommandIterationType::New();

  iterationCommand->SetOptimizer(  method->GetOptimizer() );

  method->GetOptimizer()->AddObserver( itk::IterationEvent(),
                                           iterationCommand ); 

  // set metric related parameters
  method->GetMetric()->SetTargetStandardDeviation( 5.0 );
  method->GetMetric()->SetReferenceStandardDeviation( 5.0 );
  method->GetMetric()->SetNumberOfSpatialSamples( 50 );

  // set optimizer related parameters
  typedef InternalRegistrationType::OptimizerType OptimizerType;
  typedef OptimizerType::TransformType::ParametersType ScaleType;

  ScaleType scales;
  scales.Fill( 1.0 );
  for( unsigned j = 9; j < 12; j++ )
    {
    scales[j] = 0.0001;
    }

  method->GetOptimizer()->GetTransform()->SetScale( scales );

  /**
   * Do the registration
   */
  registrator->DebugOn();
  registrator->Print( std::cout );
  registrator->StartRegistration();


  /**
   * Check the results
   */
  MRRegistrationType::RegistrationType::ParametersType solution = 
    method->GetParameters();

  std::cout << "Solution is: " << solution << std::endl;

  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[12] = { 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0 };
  trueParameters[ 0] = 1/scale[0];
  trueParameters[ 4] = 1/scale[1];
  trueParameters[ 8] = 1/scale[2];
  trueParameters[ 9] = - displacement[0]/scale[0];
  trueParameters[10] = - displacement[1]/scale[1];
  trueParameters[11] = - displacement[2]/scale[2];
  std::cout << "True solution is: ";
  for ( unsigned int j = 0; j < 12; j++)
      std::cout << trueParameters[j] << "  ";
  std::cout << std::endl;
  for( unsigned int j = 0; j < 9; j++ )
    {
    if( vnl_math_abs( solution[j] - trueParameters[j] ) > 0.02 )
      {
      pass = false;
      }
    }
  for( unsigned int j = 9; j < 12; j++ )
    {
    if( vnl_math_abs( solution[j] - trueParameters[j] ) > 1.0 )
      {
      pass = false;
      }
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}


