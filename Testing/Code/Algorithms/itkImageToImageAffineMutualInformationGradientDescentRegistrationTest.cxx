/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImageToImageAffineMutualInformationGradientDescentRegistrationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "itkImageToImageAffineMutualInformationGradientDescentRegistration.h"
#include "vnl/vnl_math.h"
#include "itkExceptionObject.h"

#include "itkCommandIterationUpdate.h"

#include <iostream>

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


int main()
{

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
// Twenty percent dilation
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


//-----------------------------------------------------------
// Set up a the registrator
//-----------------------------------------------------------
  typedef itk::ImageToImageAffineMutualInformationGradientDescentRegistration<
    ReferenceType,TargetType> RegistrationType;

  RegistrationType::Pointer registrationMethod = RegistrationType::New();


  // connect the images
  registrationMethod->SetReference(imgReference);
  registrationMethod->SetTarget(imgTarget);


  // set translation scale
  typedef RegistrationType::OptimizerType OptimizerType;
  typedef OptimizerType::TransformType::ParametersType ScaleType;

  ScaleType scales;
  scales.Fill( 1.0 );
  for( unsigned j = 9; j < 12; j++ )
    {
    scales[j] = 0.0001;
    }

  registrationMethod->GetOptimizer()->GetTransform()->SetScale( scales );

  // set metric related parameters
  registrationMethod->GetMetric()->SetTargetStandardDeviation( 5.0 );
  registrationMethod->GetMetric()->SetReferenceStandardDeviation( 5.0 );
  registrationMethod->GetMetric()->SetNumberOfSpatialSamples( 50 );



  // Define the type for the observer command to monitor progress
  typedef itk::CommandIterationUpdate<  RegistrationType::OptimizerType  >
                                                           CommandIterationType;

  CommandIterationType::Pointer iterationCommand = CommandIterationType::New();

  iterationCommand->SetOptimizer(  registrationMethod->GetOptimizer() );

  registrationMethod->GetOptimizer()->AddObserver( itk::Command::IterationEvent,
                                                   iterationCommand );


  // do the registration
  // reduce learning rate as we go

  unsigned int iter[3]  = {300,300,350};
  double       rates[3] = {1e-3, 5e-4, 1e-4};

  for( unsigned int i = 0; i < 3; i++ )
    {
    registrationMethod->SetNumberOfIterations( iter[i] );
    registrationMethod->SetLearningRate( rates[i] );

    try
      {
      registrationMethod->StartRegistration();
      }
    catch(itk::ExceptionObject& err)
      {
      // caught an exception object
      std::cout << "Caught an ExceptionObject" << std::endl;
      std::cout << err.GetLocation() << std::endl;
      std::cout << err.GetDescription() << std::endl;
      std::cout << "Test failed." << std::endl;
      return EXIT_FAILURE;

      }
    catch(...)
      {
      // caught some other error
      std::cout << "Caught unknown exception" << std::endl;
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
      }

    }

  // get the results
  RegistrationType::ParametersType solution =
    registrationMethod->GetParameters();

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
    if( vnl_math_abs( solution[j] - trueParameters[j] ) > 0.025 )
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


  // check for parzen window exception
  double oldValue = registrationMethod->GetMetric()->GetReferenceStandardDeviation();
  registrationMethod->GetMetric()->SetReferenceStandardDeviation( 0.005 );

  try
    {
    pass = false;
    registrationMethod->StartRegistration();
    }
  catch(itk::ExceptionObject& err)
    {
    std::cout << "Caught expected ExceptionObject" << std::endl;
    std::cout << err.GetLocation() << std::endl;
    std::cout << err.GetDescription() << std::endl;
    pass = true;
    }

  if( !pass )
    {
    std::cout << "Should have caught an exception" << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // check for mapped out of image error
  registrationMethod->GetMetric()->SetReferenceStandardDeviation( oldValue );

  solution[5] = 1000;
  registrationMethod->SetParameters(solution);

  try
    {
    pass = false;
    registrationMethod->StartRegistration();
    }
  catch(itk::ExceptionObject& err)
    {
    std::cout << "Caught expected ExceptionObject" << std::endl;
    std::cout << err.GetLocation() << std::endl;
    std::cout << err.GetDescription() << std::endl;
    pass = true;
    }

  if( !pass )
    {
    std::cout << "Should have caught an exception" << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
