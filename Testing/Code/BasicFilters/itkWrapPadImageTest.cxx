/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkWrapPadImageTest.cxx
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
#include "itkWrapPadImageFilter.h"
#include "itkFileOutputWindow.h"

//
// Check that val represents the correct pixel value.  This routine
// allows the pad region to extend to twice the size of the input.
//
int VerifyPixel(int row, int col, int val) 
{
  int nextVal;

  if (row < 0) row += 8;
  if (row < 0) row += 8;
  if (row > 7) row -= 8;
  if (row > 7) row -= 8;
  if (col < 0) col += 12;
  if (col < 0) col += 12;
  if (col > 11) col -= 12;
  if (col > 11) col -= 12;
  nextVal = 8*col+row;
  return (val == nextVal);
}


int main()
{
  itk::FileOutputWindow::Pointer fow = itk::FileOutputWindow::New();
  fow->SetInstance(fow);
  
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
  itk::WrapPadImageFilter< ShortImage, ShortImage >::Pointer wrapPad;
  wrapPad = itk::WrapPadImageFilter< ShortImage, ShortImage >::New();
  wrapPad->SetInput( if2 );
  
  unsigned int upperfactors[2] = { 0, 0};
  unsigned int lowerfactors[2] = { 0, 0};
  wrapPad->SetPadLowerBound(lowerfactors);
  wrapPad->SetPadUpperBound(upperfactors);
  wrapPad->UpdateLargestPossibleRegion();
 
  std::cout << wrapPad << std::endl;

  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << wrapPad->GetOutput()->GetSpacing()[0]
            << ", "
            << wrapPad->GetOutput()->GetSpacing()[1] << std::endl;
  
  
  ShortImage::RegionType requestedRegion;
  bool passed;
  
  // CASE 1
  lowerfactors[0] = 1; lowerfactors[1] = 3; 
  upperfactors[0] = 2; upperfactors[1] = 4;
  wrapPad->SetPadLowerBound(lowerfactors);
  wrapPad->SetPadUpperBound(upperfactors);
  wrapPad->UpdateLargestPossibleRegion();
  requestedRegion = wrapPad->GetOutput()->GetRequestedRegion();
  
  itk::ImageRegionIterator<ShortImage>
    iteratorIn1(wrapPad->GetOutput(), requestedRegion);
  
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
	if (!VerifyPixel(row, column, iteratorIn1.Get()))
	  {
	    std::cout << "Error: (" << row << ", " << column 
		      << "), got " 
		      << iteratorIn1.Get() << std::endl;
	    passed = false;
	  }
      }
  }
  
  if (passed)
    {
      std::cout << "wrapPadImageFilter case 1 passed." << std::endl;
    }
  else
    {
      std::cout << "wrapPadImageFilter case 1 failed." << std::endl;
      return EXIT_FAILURE;
    }
  
  
  // CASE 2
  lowerfactors[0] = 10; 
  upperfactors[1] = 15;
  wrapPad->SetPadLowerBound(lowerfactors);
  wrapPad->SetPadUpperBound(upperfactors);
  
  if ((wrapPad->GetPadUpperBound()[0] != upperfactors[0]) 
      || (wrapPad->GetPadUpperBound()[1] != upperfactors[1])
      || (wrapPad->GetPadLowerBound()[0] != lowerfactors[0])
      || (wrapPad->GetPadLowerBound()[1] != lowerfactors[1]))
    {
      passed = false;
    } 
  else 
    {
      wrapPad->UpdateLargestPossibleRegion();
      requestedRegion = wrapPad->GetOutput()->GetRequestedRegion();
      
      itk::ImageRegionIterator<ShortImage>
	iteratorIn2(wrapPad->GetOutput(), requestedRegion);
      
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
	    if (!VerifyPixel(row, column, iteratorIn2.Get()))
	      { 
		std::cout << "Error: (" << row << ", " << column 
			  << "), got " 
			  << iteratorIn2.Get() << std::endl;
		passed = false;
	      }
	  }
      }
    }
  
  if (passed)
    {
      std::cout << "wrapPadImageFilter case 2 passed." << std::endl;
    }
  else
    {
      std::cout << "wrapPadImageFilter case 2 failed." << std::endl;
      return EXIT_FAILURE;
    }
  
  
  // CASE 3
  lowerfactors[1] = 16; 
  upperfactors[0] = 9;
  wrapPad->SetPadLowerBound(lowerfactors);
  wrapPad->SetPadUpperBound(upperfactors);
  
  if ((wrapPad->GetPadUpperBound()[0] != upperfactors[0]) 
      || (wrapPad->GetPadUpperBound()[1] != upperfactors[1])
      || (wrapPad->GetPadLowerBound()[0] != lowerfactors[0])
      || (wrapPad->GetPadLowerBound()[1] != lowerfactors[1]))
    {
      passed = false;
    } 
  else 
    {
      wrapPad->UpdateLargestPossibleRegion();
      requestedRegion = wrapPad->GetOutput()->GetRequestedRegion();
      
      itk::ImageRegionIterator<ShortImage>
	iteratorIn3(wrapPad->GetOutput(), requestedRegion);
      
      passed = true; 
      size = requestedRegion.GetSize();
      index = requestedRegion.GetIndex();
      if ((index[0] != (0 - (long) lowerfactors[0])) 
	  || (index[1] != (0 - (long) lowerfactors[1]))
	  || (size[0] != (8 + lowerfactors[0] + upperfactors[0])) 
	  || (size[1] != (12 + lowerfactors[1] + upperfactors[1]))) {
	passed = false;
      } else {
	for (; !iteratorIn3.IsAtEnd(); ++iteratorIn3)
	  {
	    row = iteratorIn3.GetIndex()[0];
	    column = iteratorIn3.GetIndex()[1];
	    if (!VerifyPixel(row, column, iteratorIn3.Get()))
	      { 
		std::cout << "Error: (" << row << ", " << column 
			  << "), got " 
			  << iteratorIn3.Get() << std::endl;
		passed = false;
	      }
	  }
      }
    }
  
  if (passed)
    {
      std::cout << "wrapPadImageFilter case 3 passed." << std::endl;
    }
  else
    {
      std::cout << "wrapPadImageFilter case 3 failed." << std::endl;
      return EXIT_FAILURE;
    }
  
  return EXIT_SUCCESS;
}
