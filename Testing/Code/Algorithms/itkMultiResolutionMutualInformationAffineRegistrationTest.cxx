/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionMutualInformationAffineRegistrationTest.cxx
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
#include "itkMultiResolutionMutualInformationAffineRegistration.h"

#include <iostream>

int main()
{

//------------------------------------------------------------
// Create two simple images
// Two Gaussians with one translated (7,3,2) pixels from another
//------------------------------------------------------------

  //Allocate Images
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

  // Fill images with a 3D gaussian
  typedef  itk::ImageRegionIterator<ReferenceType>
    ReferenceIteratorType;
  typedef  itk::ImageRegionIterator<TargetType>
    TargetIteratorType;

  itk::Point<double,3> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;
  center[2] = (double)region.GetSize()[2]/2,0;

  const double s = (double)region.GetSize()[0]/2.0;

  itk::Point<double,3>  p;
  itk::Vector<double,3> d;

  // Set the displacement and scale
  itk::Vector<double,3> displacement;
  displacement[0] = 7;
  displacement[1] = 3;
  displacement[2] = 2;
  double scale[3] = { 2.0, 1.0, 1.0};

  ReferenceIteratorType ri(imgReference,region);
  TargetIteratorType ti(imgTarget,region);
  while(!ri.IsAtEnd())
  {
    p[0] = ri.GetIndex()[0];
    p[1] = ri.GetIndex()[1];
    p[2] = ri.GetIndex()[2];
	  d = p-center;
	  d += displacement;
	  const double x = d[0];
	  const double y = d[1];
    const double z = d[2];
    double v = scale[0]*x*x + scale[1]*y*y + scale[2]*z*z;
    ri.Set( (PixelType)( 200.0 * exp( - ( v )/(s*s) ) ) );
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
    ti.Set( (PixelType)( 200.0 * exp( - ( x*x + y*y + z*z )/(s*s) ) ) );
    ++ti;
  }

  // set image origin to be center of the image
  double transCenter[3];
  for( unsigned int j = 0; j < 3; j++ )
    {
    transCenter[j] = -0.5 * double(size[j] - 1);
    }

  imgReference->SetOrigin( transCenter );
  imgTarget->SetOrigin( transCenter );


 /**
  * Setup the registrator
  */
  typedef itk::MultiResolutionMutualInformationAffineRegistration<
    ReferenceType,TargetType> MRRegistrationType;

  MRRegistrationType::Pointer registrator = MRRegistrationType::New();

  registrator->SetTarget( imgTarget );
  registrator->SetReference( imgReference );
  registrator->SetNumberOfLevels( 3 );

  unsigned int niter[4] = { 300, 300, 300 };
  double rates[4] = { 1e-6, 1e-6, 1e-7 };
  double scales[4] = { 1000, 100, 100 };

  registrator->SetNumberOfIterations( niter );
  registrator->SetLearningRates( rates );
  registrator->SetTranslationScales( scales );


  MRRegistrationType::RegistrationPointer method = 
    registrator->GetInternalRegistrationMethod();


  // set metric related parameters
  method->GetMetric()->SetTargetStandardDeviation( 5.0 );
  method->GetMetric()->SetReferenceStandardDeviation( 5.0 );
  method->GetMetric()->SetNumberOfSpatialSamples( 50 );

  std::cout << "Target schedule: " << std::endl;
  std::cout << registrator->GetTargetPyramid()->GetSchedule() << std::endl;

  std::cout << "Reference schedule: " << std::endl;
  std::cout << registrator->GetReferencePyramid()->GetSchedule() << std::endl;

  /**
   * Do the registration
   */
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
  trueParameters[ 0] = sqrt(1.0/scale[0]);
  trueParameters[ 4] = sqrt(1.0/scale[1]);
  trueParameters[ 8] = sqrt(1.0/scale[2]);
  trueParameters[ 9] = - displacement[0];
  trueParameters[10] = - displacement[1];
  trueParameters[11] = - displacement[2];
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
