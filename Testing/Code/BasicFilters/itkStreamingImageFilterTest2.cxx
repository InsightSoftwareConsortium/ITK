/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkStreamingImageFilterTest2.cxx
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
#include <iostream>
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkShrinkImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkImageRegionMultidimensionalSplitter.h"
#include "itkXMLFileOutputWindow.h"

int main()
{
  itk::XMLFileOutputWindow::Pointer logger = itk::XMLFileOutputWindow::New();
  logger->SetInstance(logger);
  logger->Delete();
  
  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   ShortImage;

  // Test the creation of an image with native type
  ShortImage::Pointer if2 = ShortImage::New();

  // fill in an image
  ShortImage::IndexType  index = {{0, 0}};
  ShortImage::SizeType   size = {{42, 63}};
  ShortImage::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  if2->SetLargestPossibleRegion( region );
  if2->SetBufferedRegion( region );
  if2->Allocate();

  itk::ImageRegionIterator<ShortImage> iterator(if2, region);

  short i=0;
  short scalar;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
    scalar = i;
    iterator.Set( scalar );
    }
  
  // Create a filter
  itk::ShrinkImageFilter< ShortImage, ShortImage >::Pointer shrink;
  shrink = itk::ShrinkImageFilter< ShortImage, ShortImage >::New();
  shrink->SetInput( if2 );
  
  unsigned int factors[2] = { 2, 3 };
  shrink->SetShrinkFactors(factors);
  shrink->DebugOn();

  itk::ImageRegionMultidimensionalSplitter<2>::Pointer splitter;
  splitter = itk::ImageRegionMultidimensionalSplitter<2>::New();
  splitter->DebugOn();
  
  itk::StreamingImageFilter<ShortImage, ShortImage>::Pointer streamer;
  streamer = itk::StreamingImageFilter<ShortImage, ShortImage>::New();
  streamer->SetInput( shrink->GetOutput() );
  streamer->SetNumberOfStreamDivisions( 25 );
  streamer->SetRegionSplitter( splitter );
  streamer->Update();

  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << streamer->GetOutput()->GetSpacing()[0]
            << ", "
            << streamer->GetOutput()->GetSpacing()[1] << std::endl;


  //
  // The rest of this code determines whether the shrink code produced
  // the image we expected.
  //
  ShortImage::RegionType requestedRegion;
  requestedRegion = streamer->GetOutput()->GetRequestedRegion();
  
  itk::ImageRegionIterator<ShortImage>
    iterator2(streamer->GetOutput(), requestedRegion);

  bool passed = true;
  for (; !iterator2.IsAtEnd(); ++iterator2)
    {
    short trueValue = (short) (shrink->GetShrinkFactors()[0] * iterator2.GetIndex()[0])
              + (region.GetSize()[0]
                * shrink->GetShrinkFactors()[1] * iterator2.GetIndex()[1]);

    if ( iterator2.Get() != trueValue )
      {
      passed = false;
      std::cout << "Pixel " << iterator2.GetIndex() << " is incorrect" << std::endl;
      }
    }

  if (passed)
    {
    std::cout << "ImageStreamingFilter test passed." << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "ImageStreaming Filter test failed." << std::endl;
    return EXIT_FAILURE;
    }

}
