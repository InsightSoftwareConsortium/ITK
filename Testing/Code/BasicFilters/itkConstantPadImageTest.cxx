/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkConstantPadImageTest.cxx
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
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkConstantPadImageFilter.h"
#include "itkFileOutputWindow.h"
#include "itkStreamingImageFilter.h"

int main()
{
  itk::FileOutputWindow::Pointer fow = itk::FileOutputWindow::New();
  fow->SetInstance(fow);

  int nextVal;

  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   SimpleImage;
  SimpleImage::Pointer simpleImage = SimpleImage::New();
  std::cout << "Simple image spacing: " << simpleImage->GetSpacing()[0] << ", "
            << simpleImage->GetSpacing()[1] << std::endl;
  
  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   ShortImage;
  
  // Test the creation of an image with native type
  ShortImage::Pointer if2 = ShortImage::New();
  
  // fill in an image
  ShortImage::IndexType  index = {{0, 0}};
  ShortImage::SizeType   size = {{8, 12}};
  ShortImage::RegionType region;
  int row, column;
  region.SetSize( size );
  region.SetIndex( index );
  if2->SetLargestPossibleRegion( region );
  if2->SetBufferedRegion( region );
  if2->Allocate();
  
  itk::ImageRegionIterator<ShortImage> iterator(if2, region);
  
  short i=0;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
      iterator.Set( i );
    }
  
  // Create a filter
  itk::ConstantPadImageFilter< ShortImage, ShortImage >::Pointer constantPad;
  constantPad = itk::ConstantPadImageFilter< ShortImage, ShortImage >::New();
  constantPad->SetInput( if2 );
  
  unsigned int upperfactors[2] = { 0, 0};
  unsigned int lowerfactors[2] = { 0, 0};
  constantPad->SetConstant(13);
  constantPad->SetPadLowerBound(lowerfactors);
  constantPad->SetPadUpperBound(upperfactors);
  constantPad->UpdateLargestPossibleRegion();

  std::cout << constantPad << std::endl;
  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << constantPad->GetOutput()->GetSpacing()[0]
            << ", "
            << constantPad->GetOutput()->GetSpacing()[1] << std::endl;
  
  
  ShortImage::RegionType requestedRegion;
  bool passed;
  
  // CASE 1
  lowerfactors[0] = 1; lowerfactors[1] = 2; 
  upperfactors[0] = 3; upperfactors[1] = 4;
  constantPad->SetPadLowerBound(lowerfactors);
  constantPad->SetPadUpperBound(upperfactors);
  constantPad->UpdateLargestPossibleRegion();
  requestedRegion = constantPad->GetOutput()->GetRequestedRegion();
  
  itk::ImageRegionIterator<ShortImage>
    iteratorIn1(constantPad->GetOutput(), requestedRegion);
  
  nextVal = 0;
  passed = true; 
  size = requestedRegion.GetSize();
  index = requestedRegion.GetIndex();
  if ((index[0] != (0 - (long) lowerfactors[0])) 
      || (index[1] != (0 - (long) lowerfactors[1]))
      || (size[0] != (8 + lowerfactors[0] + upperfactors[0])) 
      || (size[1] != (12 + lowerfactors[1] + upperfactors[1]))) {
    passed = false;
  } else {
    
    for (; !iteratorIn1.IsAtEnd(); ++iteratorIn1)
      {
	row = iteratorIn1.GetIndex()[0];
	column = iteratorIn1.GetIndex()[1];
	if ((row < 0) || (row>7) || (column < 0) || (column > 11)) {
	  if ( iteratorIn1.Get() != 13 )
	    {
	      passed = false;
	    }
	} else {
	  nextVal = 8*column+row;
	  if (iteratorIn1.Get() != nextVal)
	    {
	      std::cout << "Error: (" << row << ", " << column 
			<< "), expected " << nextVal << " got " 
			<< iteratorIn1.Get() << std::endl;
	      passed = false;
	    }
	}
      }
  }
  
  if (passed)
    {
      std::cout << "constantPadImageFilter case 1 passed." << std::endl;
    }
  else
    {
      std::cout << "constantPadImageFilter case 1 failed." << std::endl;
      return EXIT_FAILURE;
    }
  
  
  // CASE 2
  lowerfactors[0] = 10; 
  upperfactors[1] = 15;
  constantPad->SetPadLowerBound(lowerfactors);
  constantPad->SetPadUpperBound(upperfactors);

  // Create a stream
  itk::StreamingImageFilter< ShortImage, ShortImage >::Pointer stream;
  stream = itk::StreamingImageFilter< ShortImage, ShortImage >::New();
  stream->SetInput( constantPad->GetOutput() );
  stream->SetNumberOfStreamDivisions(2);
  
  
  if ((constantPad->GetPadUpperBound()[0] != upperfactors[0]) 
      || (constantPad->GetPadUpperBound()[1] != upperfactors[1])
      || (constantPad->GetPadLowerBound()[0] != lowerfactors[0])
      || (constantPad->GetPadLowerBound()[1] != lowerfactors[1]))
    {
      passed = false;
    } 
  else 
    {
      stream->UpdateLargestPossibleRegion();
      requestedRegion = stream->GetOutput()->GetRequestedRegion();
      
      itk::ImageRegionIterator<ShortImage>
	iteratorIn2(stream->GetOutput(), requestedRegion);
      
      nextVal = 0;
      passed = true; 
      size = requestedRegion.GetSize();
      index = requestedRegion.GetIndex();
      if ((index[0] != (0 - (long) lowerfactors[0])) 
	  || (index[1] != (0 - (long) lowerfactors[1]))
	  || (size[0] != (8 + lowerfactors[0] + upperfactors[0])) 
	  || (size[1] != (12 + lowerfactors[1] + upperfactors[1]))) {
	passed = false;
      } else {
	for (; !iteratorIn2.IsAtEnd(); ++iteratorIn2)
	  {
	    row = iteratorIn2.GetIndex()[0];
	    column = iteratorIn2.GetIndex()[1];
	    if ((row < 0) || (row>7) || (column < 0) || (column > 11)) {
	      if ( iteratorIn2.Get() != 13 )
		{
		  passed = false;
		}
	    } else {
	      nextVal = 8*column+row;
	      if (iteratorIn2.Get() != nextVal)
		{ 
		  std::cout << "Error: (" << row << ", " << column 
			    << "), expected " << nextVal << " got " 
			    << iteratorIn2.Get() << std::endl;
		  passed = false;
		}
	    }
	  }
      }
    }
  
  if (passed)
    {
      std::cout << "constantPadImageFilter case 2 passed." << std::endl;
    }
  else
    {
      std::cout << "constantPadImageFilter case 2 failed." << std::endl;
      return EXIT_FAILURE;
    }
  
  return EXIT_SUCCESS;
}
