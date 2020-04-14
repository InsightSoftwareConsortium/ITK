/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkRLEImage.h"
#include "itkVector.h"
#include <ctime>

int
itkIteratorTestsForRLEImage(int, char *[])
{
  std::cout << "Creating an image" << std::endl;
  using ScalarImage = itk::RLEImage<unsigned short>;
  ScalarImage::Pointer o3 = ScalarImage::New();

  double origin3D[3] = { 5, 2.1, 8.1 };
  double spacing3D[3] = { 1.5, 2.1, 1 };

  ScalarImage::SizeType imageSize3D = { { 100, 100, 100 } };
  ScalarImage::SizeType bufferSize3D = { { 100, 100, 100 } };
  ScalarImage::SizeType regionSize3D = { { 91, 93, 87 } };

  ScalarImage::IndexType startIndex3D = { { 0, 0, 0 } };
  ScalarImage::IndexType bufferStartIndex3D = { { 0, 0, 0 } };
  ScalarImage::IndexType regionStartIndex3D = { { 5, 5, 5 } };


  ScalarImage::RegionType region;
  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);
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
  double        elapsedTime;
  clock_t       start, end;
  unsigned long num = regionSize3D[0] * regionSize3D[1] * regionSize3D[2];
  unsigned long i;
  bool          passed = true;

  // ImageRegionIterator
  start = clock();
  itk::ImageRegionIterator<ScalarImage> it(o3, region);

  unsigned short scalar;
  scalar = 5;

  i = 0;
  for (; !it.IsAtEnd(); ++it)
  {
    it.Set(scalar);
    ++i;
  }
  end = clock();
  elapsedTime = (end - start) / (double)CLOCKS_PER_SEC;

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
    it2.Set(scalar);
    ++i;
  }
  end = clock();
  elapsedTime = (end - start) / (double)CLOCKS_PER_SEC;

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
