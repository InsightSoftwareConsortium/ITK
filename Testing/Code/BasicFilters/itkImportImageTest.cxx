/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImportImageTest.cxx
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
#include "itkShrinkImageFilter.h"
#include "itkImportPhysicalImageFilter.h"

int main()
{
  // Create a C-array to hold an image
  short *rawImage = new short[8*12];
  for (int i=0; i < 8*12; i++)
    {
    rawImage[i] = i;
    }

  // typdefs to simplify the syntax
  typedef itk::ImportPhysicalImageFilter<short, 2>          ImportImageFilter;
  typedef itk::PhysicalImage<short, 2>   ShortImage;
    
  // Create an ImportImageFilter filter
  ImportImageFilter::Pointer import;
  import = ImportImageFilter::New();

  itk::ImageRegion<2>         region;
  itk::ImageRegion<2>::IndexType  index = {{0, 0}};
  itk::ImageRegion<2>::SizeType   size = {{8, 12}};

  region.SetSize( size );
  region.SetIndex( index );

  import->SetRegion( region );
  import->SetImportPointer( rawImage, 8*12, true);
  
  // Create another filter
  itk::ShrinkImageFilter<ImportImageFilter::OutputImageType, ShortImage >::Pointer shrink;
  shrink = itk::ShrinkImageFilter<ImportImageFilter::OutputImageType, ShortImage>::New();
  shrink->SetInput( import->GetOutput() );
  shrink->SetShrinkFactors(2);
  shrink->Update();

  //
  // The rest of this code determines whether the shrink code produced
  // the image we expected.
  //
  ShortImage::RegionType requestedRegion;
  requestedRegion = shrink->GetOutput()->GetRequestedRegion();
  
  itk::ImageRegionIterator<ShortImage>
    iterator2(shrink->GetOutput(), requestedRegion);

  bool passed = true;
  for (; !iterator2.IsAtEnd(); ++iterator2)
    {
    std::cout << "Pixel " << iterator2.GetIndex() << " = " << iterator2.Get() << std::endl;
    if ( iterator2.Get() != ((shrink->GetShrinkFactors()[0] * iterator2.GetIndex()[0])
                        + (region.GetSize()[0]
                           * shrink->GetShrinkFactors()[0] * iterator2.GetIndex()[1])))
      {
      passed = false;
      }
    }

  if (passed)
    {
    std::cout << "ImportImageFilter test passed." << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "ImportImageFilter test failed." << std::endl;
    return EXIT_FAILURE;
    }

}
