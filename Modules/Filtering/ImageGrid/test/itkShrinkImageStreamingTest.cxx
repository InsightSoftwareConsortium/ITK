/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkCastImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkTestingMacros.h"

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


  // use caster to copy source to intermediate image of only the
  // requested region
  itk::CastImageFilter<ShortImage, ShortImage>::Pointer caster;
  caster = itk::CastImageFilter<ShortImage, ShortImage>::New();
  caster->SetInput( sourceImage );


  MonitorFilter::Pointer monitor1 = MonitorFilter::New();
  monitor1->SetInput( caster->GetOutput() );

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
  if (!monitor2->VerifyAllInputCanStream(numberOfStreamDivisions))
    {
    std::cout << "Filter failed to execute as expected!" << std::endl;
    std::cout << monitor2;
    return EXIT_FAILURE;
    }

  // Verify that only the data needed is requested;
  MonitorFilter::RegionVectorType rr = monitor1->GetOutputRequestedRegions();
  for (unsigned int j = 0; j < rr.size(); ++j )
    {
    TEST_EXPECT_TRUE(rr[j].GetSize(1)%factors[1] == 1);
    }


  return EXIT_SUCCESS;


}
