/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionPyramidImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMultiResolutionPyramidImageFilter.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkImageRegionIterator.h"
#include "itkCommand.h"
#include "itkCastImageFilter.h"
#include "itkStreamingImageFilter.h"

#include <iostream>

namespace
{
  
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
  double r = vcl_sqrt( x*x + y*y + z*z );
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
}

int itkMultiResolutionPyramidImageFilterTest(int, char* [] )
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
  typedef itk::MultiResolutionPyramidImageFilter<InputImageType,OutputImageType>
    PyramidType;
  typedef PyramidType::ScheduleType ScheduleType;
  PyramidType::Pointer pyramid = PyramidType::New();

  pyramid->SetInput( imgTarget );

  bool pass = true;
  unsigned int numLevels;
  itk::Vector<unsigned int,ImageDimension> factors;

  // set schedule by specifying the number of levels;
  numLevels = 3;
  factors.Fill( 1 << (numLevels - 1) );
  pyramid->SetNumberOfLevels( numLevels );

  // check the schedule
  ScheduleType schedule( numLevels, ImageDimension );
  unsigned int j, k;

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
  ScheduleType temp( numLevels, ImageDimension );
  temp.Fill(0);
  schedule = temp;
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

  // test start factors
  const unsigned int * ss = pyramid->GetStartingShrinkFactors();
  for( j = 0; j < ImageDimension; j++ )
    {
    if( ss[j] != factors[j] )
      {
      pass = false;
      std::cout << "Returned starting factors incorrect" << std::endl;
      break;
      }
    }

  // test divisibility
  if( !PyramidType::IsScheduleDownwardDivisible( pyramid->GetSchedule() ) )
    {
    pass = false;
    std::cout << "Schedule should be downward divisible" << std::endl;
    }

  // generate output at a level with progress
  std::cout << "Run MultiResolutionPyramidImageFilter in standalone mode with progress";
  std::cout << std::endl;

  ShowProgressObject progressWatch(pyramid);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  pyramid->AddObserver(itk::ProgressEvent(), command);

  pyramid->Print( std::cout );

//  update pyramid at a particular level
  unsigned int testLevel = 2;
  pyramid->GetOutput( testLevel )->Update();

// test output at another level
  testLevel = 1;

  // check the output image information
  InputImageType::SizeType inputSize =
    pyramid->GetInput()->GetLargestPossibleRegion().GetSize();
  const double * inputOrigin =
    pyramid->GetInput()->GetOrigin();
  const double * inputSpacing =
    pyramid->GetInput()->GetSpacing();

  OutputImageType::SizeType outputSize =
    pyramid->GetOutput( testLevel )->GetLargestPossibleRegion().GetSize();
  const double * outputOrigin =
    pyramid->GetOutput( testLevel )->GetOrigin();
  const double * outputSpacing =
    pyramid->GetOutput( testLevel )->GetSpacing();

  for( j = 0; j < ImageDimension; j++ )
    {
    if( outputOrigin[j] != inputOrigin[j] )
      {
      break;
      }
    if( outputSpacing[j] !=
      inputSpacing[j] * (double) schedule[testLevel][j] )
      {
      break;
      }
    unsigned int size = inputSize[j] / schedule[testLevel][j];
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
    pyramid->GetOutput( testLevel )->Print(std::cout);
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

  typedef itk::StreamingImageFilter<OutputImageType,OutputImageType>
    StreamerType;
  StreamerType::Pointer streamer = StreamerType::New();
  streamer->SetInput( pyramid2->GetOutput( testLevel ) );
  streamer->SetNumberOfStreamDivisions( 3 );
  streamer->Update();

// comment out for now until streaming issues are fixed
/*
  std::cout << "Compare standalone and streamed outputs" << std::endl;
  typedef itk::ImageRegionIterator<OutputImageType> OutputIterator;
  OutputIterator iter1( pyramid->GetOutput( testLevel ),
    pyramid->GetOutput( testLevel )->GetBufferedRegion() );
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
*/



  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}


