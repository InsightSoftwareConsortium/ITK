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

  InputImageType::SizeType size = {{101,101,41}};
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
    std::cout << "Schedule should be: " << std::endl;
    std::cout << schedule << std::endl;
    std::cout << "instead of: " << std::endl;
    std::cout << pyramid->GetSchedule();
    return EXIT_FAILURE;
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
    std::cout << "Schedule should be: " << std::endl;
    std::cout << schedule << std::endl;
    std::cout << "instead of: " << std::endl;
    std::cout << pyramid->GetSchedule();
    return EXIT_FAILURE;
    }

  // test start factors
  const unsigned int * ss = pyramid->GetStartingShrinkFactors();
  for( j = 0; j < ImageDimension; j++ )
    {
    if( ss[j] != factors[j] )
      {
      std::cout << "Returned starting factors incorrect" << std::endl;
      return EXIT_FAILURE;
      }
    }

  // test divisibility
  if( !PyramidType::IsScheduleDownwardDivisible( pyramid->GetSchedule() ) )
    {
    std::cout << "Schedule should be downward divisible" << std::endl;
    return EXIT_FAILURE;
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
    std::cout << "Output meta information incorrect." << std::endl;
    pyramid->GetInput()->Print(std::cout);
    pyramid->GetOutput( testLevel )->Print(std::cout);
    return EXIT_FAILURE;
    }

  // check that the buffered region is equivalent the largestpossible
  if( pyramid->GetOutput(numLevels-1)->GetBufferedRegion() !=
    pyramid->GetOutput(numLevels-1)->GetLargestPossibleRegion() )
    {
    std::cout << "Output buffered region incorrect. " << std::endl;
    pyramid->GetOutput(numLevels-1)->Print(std::cout);
    return EXIT_FAILURE;
    }

  // Test schedule checking code
  factors.Fill( 0 );
  pyramid->SetStartingShrinkFactors( factors.Begin() );

  schedule = pyramid->GetSchedule();
  pyramid->SetSchedule( schedule );
  schedule.Fill( 0 );
  pyramid->SetSchedule( schedule );

  ScheduleType temp2( pyramid->GetNumberOfLevels() - 1, ImageDimension );
  temp2.Fill( 1 );
  pyramid->SetSchedule( temp2 );
  
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}


