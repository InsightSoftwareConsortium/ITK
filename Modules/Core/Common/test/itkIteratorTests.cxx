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

#include "itkImage.h"
#include "itkVector.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include <time.h>

int itkIteratorTests(int, char* [] )
{
  std::cout << "Creating an image" << std::endl;
  typedef itk::Image<unsigned short, 3> ScalarImage;
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
  unsigned long i;
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
  ptr = o3->GetBufferPointer();
  for (i=0; i < num; ++i)
    {
    ++ptr;
    }
  end = clock();
  *ptr = 0;
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
  for (; !it.IsAtEnd(); ++it)
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

  // ImageRegionIteratorWithIndex
  start = clock();
  itk::ImageRegionIteratorWithIndex<ScalarImage> it2(o3, region);

  i = 0;
  for (; !it2.IsAtEnd(); ++it2)
    {
    it2.Set( scalar );
    ++i;
    }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "ImageRegionIteratorWithIndex" << std::endl;
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
