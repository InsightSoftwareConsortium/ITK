/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkResampleImageTest.cxx
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
#include "itkPhysicalImage.h"
#include "itkImageRegionIterator.h"
#include "itkScalar.h"
#include "itkResampleImageFilter.h"


int main()
{
  // typedefs to simplify the syntax
  typedef itk::PhysicalImage<itk::Scalar<short>, 2>   ShortImage;

  // Test the creation of an image with native type
  ShortImage::Pointer if2 = ShortImage::New();

  // fill in an image
  ShortImage::IndexType  index = {{0, 0}};
  ShortImage::SizeType   size = {{8, 12}};
  ShortImage::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  if2->SetLargestPossibleRegion( region );
  if2->SetBufferedRegion( region );
  if2->Allocate();

  itk::ImageRegionIterator<ShortImage> iterator(if2, region);

  short i=0;
  itk::Scalar<short> scalar;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
    scalar = i;
    iterator.Set( scalar );
    }

  // FIXME: Create an affine transformation

  // FIXME: Create a linear interpolation image function
  
  // Create and configure a resampling filter
  itk::ResampleImageFilter< ShortImage, ShortImage >::Pointer resample;
  resample = itk::ResampleImageFilter< ShortImage, ShortImage >::New();
  resample->SetInput( if2 );

  // FIXME: Must finish configuration code before running rest of program
  return EXIT_FAILURE;

#if 0             // FIXME: Disable rest of program for now

  // Run the resampling filter
  resample->Update();

  // Check if we got the results we expected
  // FIXME: Modify for image resampling
  ShortImage::RegionType requestedRegion;
  requestedRegion = resample->GetOutput()->GetRequestedRegion();
  
  itk::ImageRegionIterator<ShortImage>
    iterator2(resample->GetOutput(), requestedRegion);

  bool passed = true;
  for (; !iterator2.IsAtEnd(); ++iterator2)
    {
    std::cout << "Pixel " << iterator2.GetIndex() << " = " << iterator2.Get()
              << std::endl;
    if ( iterator2.Get() != ((shrink->GetShrinkFactor() * iterator2.GetIndex()[0])
                        + (region.GetSize()[0]
                           * shrink->GetShrinkFactor() * iterator2.GetIndex()[1])))
      {
      passed = false;
      }
    }

  if (passed)
    {
    std::cout << "ResampleImageFilter test passed." << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "ResampleImageFilter test failed." << std::endl;
    return EXIT_FAILURE;
    }

#endif

}
