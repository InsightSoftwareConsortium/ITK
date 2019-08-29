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
#include "itkImageRegionIterator.h"
#include "itkPolyLineParametricPath.h"
#include "itkPathFunctions.h"
#include "itkPathIterator.h"
#include "itkTestingMacros.h"

int
itkPathFunctionsTest(int, char *[])
{

  constexpr unsigned int Dimension = 2;
  using PixelType = double;

  using ImageType = itk::Image<PixelType, Dimension>;
  using PolyLineParametricPathType = itk::PolyLineParametricPath<Dimension>;
  using ChainPathType = itk::ChainCodePath<Dimension>;
  using FourierSeriesPathType = itk::FourierSeriesPath<Dimension>;
  using PathIteratorType = itk::PathIterator<ImageType, FourierSeriesPathType>;

  using IndexType = ImageType::IndexType;
  using VertexType = PolyLineParametricPathType::VertexType;

  bool passed = true;


  // Set up the image
  std::cout << "Making a 64x64 white square centered in a 128x128 black image" << std::endl;
  ImageType::Pointer image = ImageType::New();

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  ImageType::SizeType size;
  size[0] = 128;
  size[1] = 128;
  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(start);
  image->SetRegions(region);
  double spacing[ImageType::ImageDimension];
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  image->SetSpacing(spacing);

  image->Allocate();

  using ImageRegionIteratorType = itk::ImageRegionIterator<ImageType>;
  ImageRegionIteratorType it(image, image->GetRequestedRegion());
  it.GoToBegin();
  ImageType::PixelType storedValue;
  IndexType            pixelIndex;
  while (!it.IsAtEnd())
  {
    pixelIndex = it.GetIndex();
    if (pixelIndex[0] >= int(size[0] / 4) && pixelIndex[0] < int(size[0] * 3 / 4) &&
        pixelIndex[1] >= int(size[1] / 4) && pixelIndex[1] < int(size[1] * 3 / 4))
    {
      it.Set(1.0);
    }
    else
    {
      it.Set(0.0);
    }
    ++it;
  }

  // Retrieve and print the value stored at pixel index (32,32)
  pixelIndex[0] = 32;
  pixelIndex[1] = 32;
  storedValue = image->GetPixel(pixelIndex);
  std::cout << "The pixel at index (" << pixelIndex[0] << "," << pixelIndex[1] << ") has the value " << storedValue
            << ".\n"
            << std::endl;

  // Set up the path
  std::cout << "Making a square Path with v0 at (30,30) and v2 at (33,33)" << std::endl;
  VertexType                          v;
  PolyLineParametricPathType::Pointer inPath = PolyLineParametricPathType::New();
  ChainPathType::Pointer              chainPath = ChainPathType::New();
  FourierSeriesPathType::Pointer      path = FourierSeriesPathType::New();
  v.Fill(30);
  inPath->AddVertex(v);
  v[0] = 33;
  v[1] = 30;
  inPath->AddVertex(v);
  v.Fill(33);
  inPath->AddVertex(v);
  v[0] = 30;
  v[1] = 33;
  inPath->AddVertex(v);
  v.Fill(30);
  inPath->AddVertex(v);

  itk::MakeChainCodeTracePath(*chainPath, *inPath);
  std::cout << "New ChainCodePath has " << chainPath->NumberOfSteps() << " steps." << std::endl;

  itk::MakeFourierSeriesPathTraceChainCode(*path, *chainPath, 2);

  // Test the iterator
  std::cout << "Creating an iterator to trace the FourierSeriesPath" << std::endl;
  PathIteratorType iter(image, path);
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
  {
    std::cout << "Path(" << iter.GetPathPosition() << ") @ " << iter.GetIndex() << " = " << iter.Get()
              << "; Now inverting." << std::endl;
    iter.Set(1.0 - iter.Get());
  }
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
  {
    std::cout << "Path(" << iter.GetPathPosition() << ") @ " << iter.GetIndex() << " = " << iter.Get() << std::endl;
  }
  std::cout << "Should still be at end: ";
  std::cout << "Path(" << iter.GetPathPosition() << ") @ " << iter.GetIndex() << " = " << iter.Get() << std::endl;

  if ((iter.GetIndex())[0] != 30 || (iter.GetIndex())[1] != 30)
  {
    std::cout << "PathFunctionsTest: Failed to maintain a closed path" << std::endl;
    passed = false;
  }

  if (passed)
  {
    std::cout << "Test passed" << std::endl;
    return EXIT_SUCCESS;
  }
  else
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }
}
