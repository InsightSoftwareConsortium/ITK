/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionImagePyramidTest.cxx
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
#include "itkMultiResolutionImagePyramid.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkImageRegionIterator.h"
#include "itkCommand.h"
#include "itkCastImageFilter.h"
#include "itkStreamingImageFilter.h"

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

// The following three classes are used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};

int main()
{

//------------------------------------------------------------
// Create a simple image
//------------------------------------------------------------

  // Allocate Images
  typedef signed short PixelType;
  typedef itk::Image<PixelType,3>           InputImageType;
  typedef itk::Image<float,3>               OutputImageType;
  enum { ImageDimension = InputImageType::ImageDimension };

  InputImageType::SizeType size = {{100,100,40}};
  InputImageType::IndexType index = {{0,0,0}};
  InputImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  InputImageType::Pointer imgTarget = InputImageType::New();
  imgTarget->SetLargestPossibleRegion( region );
  imgTarget->SetBufferedRegion( region );
  imgTarget->SetRequestedRegion( region );
  imgTarget->Allocate();

  // Fill images with a 3D gaussian with some directional pattern
  // in the background
  typedef  itk::ImageRegionIterator<InputImageType> Iterator;

  itk::Point<double,3> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;
  center[2] = (double)region.GetSize()[2]/2,0;

  itk::Point<double,3>  p;
  itk::Vector<double,3> d;

  Iterator ti(imgTarget,region);


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

  imgTarget->SetOrigin( transCenter );


 /**
  * Setup a multi-resolution pyramid
  */
  typedef itk::MultiResolutionImagePyramid<InputImageType,OutputImageType>
    PyramidType;
  typedef PyramidType::ScheduleType ScheduleType;
  PyramidType::Pointer pyramid = PyramidType::New();

  pyramid->SetInput( imgTarget );

  bool pass = true;
  unsigned int numLevels = 3;
  itk::Vector<unsigned int,ImageDimension> factors;

  // set schedule by specifying the number of levels;
  numLevels = 3;
  factors.Fill( 1 << (numLevels - 1) );
  pyramid->SetNumberOfLevels( numLevels );

  // check the schedule
  ScheduleType schedule;
  unsigned int j, k;

  schedule.resize( numLevels, ImageDimension );
  for( k = 0; k < numLevels; k++ )
    {
    unsigned int denominator = 1 << k;
    for( j = 0; j < ImageDimension; j++ )
      {
      schedule[k][j] = factors[j] / denominator;
      if( schedule[k][j] == 0 )
        {
        schedule[k][j] = 1;
        }
      }
    }

  if( schedule != pyramid->GetSchedule() )
    {
    pass = false;
    std::cout << "Schedule should be: " << std::endl;
    std::cout << schedule << std::endl;
    std::cout << "instead of: " << std::endl;
    std::cout << pyramid->GetSchedule();
    }

  // set schedule by specifying the starting shrink factors
  numLevels = 4;
  factors[0] = 8; factors[1] = 4; factors[2] = 2;
  pyramid->SetNumberOfLevels( numLevels );
  pyramid->SetStartingShrinkFactors( factors.Begin() );

  // check the schedule;
  schedule.resize( numLevels, ImageDimension );
  for( k = 0; k < numLevels; k++ )
    {
    unsigned int denominator = 1 << k;
    for( j = 0; j < ImageDimension; j++ )
      {
      schedule[k][j] = factors[j] / denominator;
      if( schedule[k][j] == 0 )
        {
        schedule[k][j] = 1;
        }
      }
    }

  if( schedule != pyramid->GetSchedule() )
    {
    pass = false;
    std::cout << "Schedule should be: " << std::endl;
    std::cout << schedule << std::endl;
    std::cout << "instead of: " << std::endl;
    std::cout << pyramid->GetSchedule();
    }

  // generate output at a level with progress
  std::cout << "Run MultiResolutionImagePyramid in standalone mode with progress";
  std::cout << std::endl;

  ShowProgressObject progressWatch(pyramid);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  pyramid->AddObserver(itk::Command::ProgressEvent, command);

  unsigned int currentLevel = 1;
  pyramid->SetCurrentLevel( currentLevel );
  pyramid->Update();


  pyramid->Print( std::cout );

  // check the output image information
  InputImageType::SizeType inputSize =
    pyramid->GetInput()->GetLargestPossibleRegion().GetSize();
  const double * inputOrigin =
    pyramid->GetInput()->GetOrigin();
  const double * inputSpacing =
    pyramid->GetInput()->GetSpacing();

  OutputImageType::SizeType outputSize =
    pyramid->GetOutput()->GetLargestPossibleRegion().GetSize();
  const double * outputOrigin =
    pyramid->GetOutput()->GetOrigin();
  const double * outputSpacing =
    pyramid->GetOutput()->GetSpacing();

  for( j = 0; j < ImageDimension; j++ )
    {
    if( outputOrigin[j] != inputOrigin[j] )
      {
      break;
      }
    if( outputSpacing[j] !=
      inputSpacing[j] * (double) schedule[currentLevel][j] )
      {
      break;
      }
    unsigned int size = inputSize[j] / schedule[currentLevel][j];
    if( size == 0 ) size = 1;
    if( outputSize[j] != size )
      {
      break;
      }
    }

  if( j != ImageDimension )
    {
    pass = false;
    pyramid->GetInput()->Print(std::cout);
    pyramid->GetOutput()->Print(std::cout);
    }


  // run in streamed mode
  std::cout << "Run ImagePyramid with streamer";
  std::cout << std::endl;
  
  typedef itk::CastImageFilter<InputImageType,InputImageType> CasterType;
  CasterType::Pointer caster = CasterType::New();

  caster->SetInput( pyramid->GetInput() );

  PyramidType::Pointer pyramid2 = PyramidType::New();
  pyramid2->SetInput( caster->GetOutput() );
  pyramid2->SetNumberOfLevels( pyramid->GetNumberOfLevels() );
  pyramid2->SetSchedule( pyramid->GetSchedule() );
  pyramid2->SetCurrentLevel( pyramid->GetCurrentLevel() );

  typedef itk::StreamingImageFilter<OutputImageType,OutputImageType>
    StreamerType;
  StreamerType::Pointer streamer = StreamerType::New();
  streamer->SetInput( pyramid2->GetOutput() );
  streamer->SetNumberOfStreamDivisions( 3 );
  streamer->Update();

  std::cout << "Compare standalone and streamed outputs" << std::endl;
  typedef itk::ImageRegionIterator<OutputImageType> OutputIterator;
  OutputIterator iter1( pyramid->GetOutput(),
    pyramid->GetOutput()->GetBufferedRegion() );
  OutputIterator iter2( streamer->GetOutput(),
    streamer->GetOutput()->GetBufferedRegion() );

  while( !iter1.IsAtEnd() )
    {
    if( iter1.Get() != iter2.Get() )
      {
      std::cout << "Streamed output is different!!!" << std::endl;
      pass = false;
      }
    ++iter1;
    ++iter2;
    }



  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}


