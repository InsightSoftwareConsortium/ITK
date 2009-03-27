/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShrinkImageStreamingTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkShrinkImageFilter.h"
#include "itkFileOutputWindow.h"
#include "../IO/itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"

int itkShrinkImageStreamingTest(int, char* [] )
{
  
  const unsigned int numberOfStreamDivisions = 4;
  
  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   ShortImage;
  ShortImage::Pointer sourceImage = ShortImage::New();

  typedef itk::PipelineMonitorImageFilter<ShortImage> MonitorFilter;

  // fill in an image
  ShortImage::IndexType  index = {{100, 100}};
  ShortImage::SizeType   size = {{8, 12}};
  ShortImage::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  sourceImage->SetRegions( region );
  sourceImage->Allocate();

  itk::ImageRegionIterator<ShortImage> iterator(sourceImage, region);

  short i=0;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
    iterator.Set( i );
    }

  
  MonitorFilter::Pointer monitor1 = MonitorFilter::New();
  monitor1->SetInput( sourceImage );

  
  // Create a filter, shrink by 2,3
  itk::ShrinkImageFilter< ShortImage, ShortImage >::Pointer shrink;
  shrink = itk::ShrinkImageFilter< ShortImage, ShortImage >::New();
  shrink->SetInput( monitor1->GetOutput() );
  
  unsigned int factors[2] = { 2, 3 };
  shrink->SetShrinkFactors(factors);


  MonitorFilter::Pointer monitor2 = MonitorFilter::New();
  monitor2->SetInput(shrink->GetOutput());
  
  itk::StreamingImageFilter<ShortImage, ShortImage>::Pointer streamer;
  streamer = itk::StreamingImageFilter<ShortImage, ShortImage>::New();
  streamer->SetInput( monitor2->GetOutput() );
  streamer->SetNumberOfStreamDivisions( numberOfStreamDivisions );
  streamer->Update();

  
  // this verifies that the pipeline was executed as expected allong
  // with correct region propagation and output information
  if (!monitor2->VerifyAllInputCanStream(4)) 
    {
    std::cout << "Filter failed to execute as expected!" << std::endl;
    std::cout << monitor2;
    return EXIT_FAILURE;
    }
  
  return EXIT_SUCCESS;
  

}
