/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include <ctime>

int
itkIteratorTests(int, char *[])
{
  std::cout << "Creating an image" << std::endl;
  using ScalarImage = itk::Image<unsigned short, 3>;
  auto o3 = ScalarImage::New();

  double origin3D[3] = { 5, 2.1, 8.1 };
  double spacing3D[3] = { 1.5, 2.1, 1 };

  constexpr ScalarImage::SizeType imageSize3D = { { 200, 200, 200 } };
  constexpr ScalarImage::SizeType bufferSize3D = { { 200, 200, 200 } };
  constexpr ScalarImage::SizeType regionSize3D = { { 190, 190, 190 } };

  constexpr ScalarImage::IndexType startIndex3D = { { 0, 0, 0 } };
  constexpr ScalarImage::IndexType bufferStartIndex3D = { { 0, 0, 0 } };
  constexpr ScalarImage::IndexType regionStartIndex3D = { { 5, 5, 5 } };


  ScalarImage::RegionType region{ startIndex3D, imageSize3D };
  o3->SetLargestPossibleRegion(region);
  region.SetSize(bufferSize3D);
  region.SetIndex(bufferStartIndex3D);
  o3->SetBufferedRegion(region);
  region.SetSize(regionSize3D);
  region.SetIndex(regionStartIndex3D);
  o3->SetRequestedRegion(region);

  o3->SetOrigin(origin3D);
  o3->SetSpacing(spacing3D);

  o3->Allocate();

  // extra variables
  constexpr unsigned long num = 190 * 190 * 190;

  bool passed = true;
  // memset
  clock_t          start = clock();
  unsigned short * ptr = o3->GetBufferPointer();
  memset(ptr, 0, num * sizeof(unsigned short));
  clock_t end = clock();
  double  elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);

  std::cout << "Raw pointer using memset" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << num << std::endl;

  // 1D array
  start = clock();
  ptr = o3->GetBufferPointer();
  {
    unsigned int i = 0;
    for (; i < num; ++i)
    {
      ++ptr;
    }
    end = clock();
    *ptr = 0;
    elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);

    std::cout << "Raw pointer as a 1D array" << std::endl;
    std::cout << "\tTime   = " << elapsedTime << std::endl;
    std::cout << "\tPixels = " << i << std::endl;

    if (i != num)
    {
      passed = false;
    }
  }
  // 3 nested loops
  const unsigned long len = 190;
  start = clock();
  {
    unsigned int i = 0;
    ptr = o3->GetBufferPointer();
    for (unsigned long ii = 0; ii < len; ++ii)
      for (unsigned long jj = 0; jj < len; ++jj)
        for (unsigned long kk = 0; kk < len; ++kk)
        {
          *ptr = 5;
          ++ptr;
          ++i;
        }
    end = clock();
    elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);

    std::cout << "Raw pointer in 3 nested loops" << std::endl;
    std::cout << "\tTime   = " << elapsedTime << std::endl;
    std::cout << "\tPixels = " << i << std::endl;

    if (i != num)
    {
      passed = false;
    }
  }
  // ImageRegionIterator
  start = clock();
  itk::ImageRegionIterator<ScalarImage> it(o3, region);

  constexpr unsigned short scalar = 5;

  {
    unsigned int i = 0;
    for (; !it.IsAtEnd(); ++it)
    {
      it.Set(scalar);
      ++i;
    }
    end = clock();
    elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);

    std::cout << "ImageRegionIterator" << std::endl;
    std::cout << "\tTime   = " << elapsedTime << std::endl;
    std::cout << "\tPixels = " << i << std::endl;

    if (i != num)
    {
      passed = false;
    }
  }
  // ImageRegionIteratorWithIndex
  start = clock();
  itk::ImageRegionIteratorWithIndex<ScalarImage> it2(o3, region);
  {
    unsigned int i = 0;
    for (; !it2.IsAtEnd(); ++it2)
    {
      it2.Set(scalar);
      ++i;
    }
    end = clock();
    elapsedTime = (end - start) / static_cast<double>(CLOCKS_PER_SEC);

    std::cout << "ImageRegionIteratorWithIndex" << std::endl;
    std::cout << "\tTime   = " << elapsedTime << std::endl;
    std::cout << "\tPixels = " << i << std::endl;

    if (i != num)
    {
      passed = false;
    }
  }
  if (passed)
  {
    std::cout << "Iterator tests passed" << std::endl;
    return EXIT_SUCCESS;
  }

  std::cout << "Iterator tests failed" << std::endl;
  return EXIT_FAILURE;
}
