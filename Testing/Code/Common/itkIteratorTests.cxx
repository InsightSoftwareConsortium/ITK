/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkIteratorTests.cxx
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
#include "itkVector.h"
#include "itkImageRegionIterator.h"
#include "itkImageBufferIterator.h"
#include "itkSimpleImageRegionIterator.h"
#include <time.h>

int main()
{
  std::cout << "Creating an image" << std::endl;
  typedef itk::PhysicalImage<unsigned short, 3> ScalarImage;
  ScalarImage::Pointer  o3 = ScalarImage::New();

  double origin3D[3] = { 5, 2.1, 8.1};
  double spacing3D[3] = { 1.5, 2.1, 1};

  ScalarImage::SizeType imageSize3D = {{ 200, 200, 200 }};
  ScalarImage::SizeType bufferSize3D = {{ 200, 200, 200 }};
  ScalarImage::SizeType regionSize3D = {{ 190, 190, 190 }};

  ScalarImage::IndexType startIndex3D = {{0, 0, 0}};
  ScalarImage::IndexType bufferStartIndex3D = {{0, 0, 0}};
  ScalarImage::IndexType regionStartIndex3D = {{5,5, 5}};


  ScalarImage::RegionType region;
  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);
  o3->SetLargestPossibleRegion( region );
  region.SetSize(bufferSize3D);
  region.SetIndex(bufferStartIndex3D);
  o3->SetBufferedRegion( region );
  region.SetSize(regionSize3D);
  region.SetIndex(regionStartIndex3D);
  o3->SetRequestedRegion( region );
  
  o3->SetOrigin(origin3D);
  o3->SetSpacing(spacing3D);

  o3->Allocate();

  // extra variables
  double elapsedTime;
  clock_t start, end;
  unsigned long num = 190*190*190;
  unsigned long i = 0;
  bool passed = true;
  
  // memset
  start = clock();
  unsigned short *ptr = o3->GetBufferPointer();
  memset(ptr, 0, num*sizeof(unsigned short));
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "Raw pointer using memset" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << num << std::endl;

  // 1D array
  start = clock();
  i = 0;
  ptr = o3->GetBufferPointer();
  for (i=0; i < num; ++i)
    {
    ++ptr;
    }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "Raw pointer as a 1D array" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << i << std::endl;

  if (i != num)
    {
    passed = false;
    }
  
  // 3 nested loops
  unsigned long ii, jj, kk, len=190;
  start = clock();
  i = 0;
  ptr = o3->GetBufferPointer();
  for (ii=0; ii < len; ++ii)
    for (jj=0; jj < len; ++jj)
      for (kk=0; kk < len; ++kk)
        {
        *ptr = 5;
        ++ptr;
        ++i;
        }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "Raw pointer in 3 nested loops" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << i << std::endl;
  
  if (i != num)
    {
    passed = false;
    }

  // ImageRegionIterator
  start = clock();
  itk::ImageRegionIterator<ScalarImage> it(o3, region);

  unsigned short scalar;
  scalar = 5;
  
  i = 0;
  for ( ; !it.IsAtEnd(); ++it)
    {
    it.Set( scalar );
    ++i;
    }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "ImageRegionIterator" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << i << std::endl;

  if (i != num)
    {
    passed = false;
    }

  // SimpleImageRegionIterator
  start = clock();
  itk::SimpleImageRegionIterator<ScalarImage> it2(o3, region);

  i = 0;
  for ( it2.Begin(); !it2.IsAtEnd(); ++it2)
    {
    it2.Set( scalar );
    ++i;
    }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "SimpleImageRegionIterator" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << i << std::endl;

  if (i != num)
    {
    passed = false;
    }
  
  if (passed)
    {
    std::cout << "Iterator tests passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Iterator tests failed" << std::endl;
    return EXIT_FAILURE;
    }
}
