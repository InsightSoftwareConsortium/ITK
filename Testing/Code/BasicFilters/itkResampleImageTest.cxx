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

#include "itkAffineTransform.h"
#include "itkPhysicalImage.h"
#include "itkImageRegionIterator.h"
#include "itkResampleImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"


enum {NDimensions = 2};

typedef short                  PixelType;
typedef itk::PhysicalImage<PixelType, NDimensions>     ImageType;
typedef ImageType::IndexType                ImageIndexType;
typedef ImageType::Pointer                  ImagePointerType;
typedef ImageType::RegionType               ImageRegionType;
typedef ImageType::SizeType                 ImageSizeType;
typedef ImageType::AffineTransformType      AffineTransformType;
typedef itk::LinearInterpolateImageFunction<ImageType>  InterpolatorType;


int main()
{
  // Create and configure an image
  ImagePointerType image = ImageType::New();
  ImageIndexType  index = {{0,  0}};
  ImageSizeType   size  = {{8, 12}};
  ImageRegionType region;
  region.SetSize ( size );
  region.SetIndex( index );
  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->Allocate();

  // Fill image with a ramp
  itk::ImageRegionIteratorWithIndex<ImageType> iter(image, region);
  PixelType value;
  for (iter.Begin(); !iter.IsAtEnd(); ++iter) {
    index = iter.GetIndex();
    value = index[0] + index[1];
    iter.Set(value);
  }

  // Create an affine transformation
  AffineTransformType aff;
  aff.Scale(0.5);

  // Create a linear interpolation image function
  InterpolatorType::Pointer interp = InterpolatorType::New();
  interp->SetInputImage(image);
  
  // Create and configure a resampling filter
  itk::ResampleImageFilter< ImageType, ImageType >::Pointer resample;
  resample = itk::ResampleImageFilter< ImageType, ImageType >::New();
  resample->SetInput(image);
  resample->SetSize(size);
  resample->SetTransform(&aff);
  resample->SetInterpolator(interp);

  // Run the resampling filter
  resample->Update();

  // Check if desired results were obtained
  bool passed = true;
  ImageType::RegionType region2;
  region2 = resample->GetOutput()->GetRequestedRegion();
  itk::ImageRegionIteratorWithIndex<ImageType>
      iter2(resample->GetOutput(), region2);
  PixelType pixval;
  for (iter2.Begin(); !iter2.IsAtEnd(); ++iter2) {
    index  = iter2.GetIndex();
    value  = iter2.Get();
    pixval = value;
    if ( (index[0] + index[1]) / 2 != pixval ) {
      std::cout << "Error in resampled image: Pixel " << index
                << " = " << value << std::endl;
      passed = false;
    }
  }

  // Report success or failure
  if (passed) {
    std::cout << "Resampling test was successful" << std::endl;
    return EXIT_SUCCESS;
  }
  else {
    std::cout << "Resampling test failed" << std::endl;
    return EXIT_FAILURE;
  }

}
