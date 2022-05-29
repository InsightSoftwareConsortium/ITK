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

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"


template <typename TRegion>
static bool
RunTest(const TRegion & region, const TRegion & exclusionRegion)
{
  const unsigned int ImageDimension = TRegion::ImageDimension;

  using IndexPixelType = itk::Index<ImageDimension>;
  using ValuePixelType = unsigned char;

  using IndexImageType = itk::Image<IndexPixelType, ImageDimension>;
  using ValueImageType = itk::Image<ValuePixelType, ImageDimension>;

  auto myIndexImage = IndexImageType::New();

  myIndexImage->SetRegions(region);
  myIndexImage->Allocate();

  auto myValueImage = ValueImageType::New();

  myValueImage->SetRegions(region);
  myValueImage->Allocate();

  using ValueIteratorType = itk::ImageRegionIteratorWithIndex<ValueImageType>;
  using IndexIteratorType = itk::ImageRegionIteratorWithIndex<IndexImageType>;

  constexpr unsigned char normalRegionValue = 100;
  constexpr unsigned char exclusionRegionValue = 200;

  // Initialize the Image
  IndexIteratorType ii(myIndexImage, region);
  ValueIteratorType iv(myValueImage, region);

  ii.GoToBegin();
  iv.GoToBegin();

  while (!ii.IsAtEnd())
  {
    ii.Set(ii.GetIndex());
    iv.Set(normalRegionValue);
    ++ii;
    ++iv;
  }

  // Set a different value inside the exclusion region
  TRegion croppedExclusionRegion(exclusionRegion);
  if (!croppedExclusionRegion.Crop(region))
  {
    // Exclusion region is completely outside the region. Set it to
    // have size 0.
    typename TRegion::IndexType exclusionStart = region.GetIndex();
    croppedExclusionRegion.SetIndex(exclusionStart);

    typename TRegion::SizeType exclusionSize = croppedExclusionRegion.GetSize();
    exclusionSize.Fill(0);
    croppedExclusionRegion.SetSize(exclusionSize);
  }

  ValueIteratorType ive(myValueImage, croppedExclusionRegion);

  ive.GoToBegin();
  while (!ive.IsAtEnd())
  {
    ive.Set(exclusionRegionValue);
    ++ive;
  }

  using ExclusionIndexIteratorType = itk::ImageRegionExclusionIteratorWithIndex<IndexImageType>;
  using ExclusionValueIteratorType = itk::ImageRegionExclusionIteratorWithIndex<ValueImageType>;

  ExclusionValueIteratorType ev(myValueImage, region);
  ExclusionIndexIteratorType ei(myIndexImage, region);

  ev.SetExclusionRegion(exclusionRegion);
  ei.SetExclusionRegion(exclusionRegion);

  unsigned int       numberOfPixelsVisited = 0;
  const unsigned int pixelsToVisit = region.GetNumberOfPixels() - croppedExclusionRegion.GetNumberOfPixels();

  ev.GoToBegin();
  ei.GoToBegin();
  while (!ev.IsAtEnd())
  {
    if (ei.Get() != ei.GetIndex())
    {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It should be at " << ei.GetIndex();
      std::cout << " but it is at   " << ei.Get() << std::endl;
      return false;
    }

    if (ev.Get() != normalRegionValue)
    {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << ev.GetIndex() << std::endl;
      return false;
    }
    ++numberOfPixelsVisited;
    ++ei;
    ++ev;
  }

  if (numberOfPixelsVisited != pixelsToVisit)
  {
    std::cout << "Error in exclusion iterator " << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return false;
  }

  numberOfPixelsVisited = 0;
  ev.GoToReverseBegin();
  ei.GoToReverseBegin();
  while (!ev.IsAtReverseEnd())
  {
    if (ei.Get() != ei.GetIndex())
    {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It should be at " << ei.GetIndex();
      std::cout << " but it is at   " << ei.Get() << std::endl;
      return false;
    }

    if (ev.Get() != normalRegionValue)
    {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << ev.GetIndex() << std::endl;
      return false;
    }
    ++numberOfPixelsVisited;
    --ei;
    --ev;
  }

  if (numberOfPixelsVisited != pixelsToVisit)
  {
    std::cout << "Error in exclusion iterator" << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return false;
  }

  using ExclusionIndexConstIteratorType = itk::ImageRegionExclusionConstIteratorWithIndex<IndexImageType>;
  using ExclusionValueConstIteratorType = itk::ImageRegionExclusionConstIteratorWithIndex<ValueImageType>;

  ExclusionValueConstIteratorType cev(myValueImage, region);
  ExclusionIndexConstIteratorType cei(myIndexImage, region);

  cev.SetExclusionRegion(exclusionRegion);
  cei.SetExclusionRegion(exclusionRegion);

  numberOfPixelsVisited = 0;

  cev.GoToBegin();
  cei.GoToBegin();
  while (!cev.IsAtEnd())
  {
    if (cei.Get() != cei.GetIndex())
    {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It should be at " << cei.GetIndex();
      std::cout << " but it is at   " << cei.Get() << std::endl;
      return false;
    }

    if (cev.Get() != normalRegionValue)
    {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << ev.GetIndex() << std::endl;
      return false;
    }
    ++numberOfPixelsVisited;
    ++cei;
    ++cev;
  }

  if (numberOfPixelsVisited != pixelsToVisit)
  {
    std::cout << "Error in exclusion const iterator " << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return false;
  }

  numberOfPixelsVisited = 0;
  cev.GoToReverseBegin();
  cei.GoToReverseBegin();
  while (!cev.IsAtReverseEnd())
  {
    if (cei.Get() != cei.GetIndex())
    {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It should be at " << cei.GetIndex();
      std::cout << " but it is at   " << cei.Get() << std::endl;
      return false;
    }

    if (cev.Get() != normalRegionValue)
    {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << cev.GetIndex() << std::endl;
      return false;
    }
    ++numberOfPixelsVisited;
    --cei;
    --cev;
  }

  if (numberOfPixelsVisited != pixelsToVisit)
  {
    std::cout << "Error in exclusion const iterator " << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return false;
  }

  return true;
}


int
itkImageRegionExclusionIteratorWithIndexTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;
  using SizeType = itk::Size<Dimension>;
  using IndexType = itk::Index<Dimension>;
  using RegionType = itk::ImageRegion<Dimension>;

  SizeType   regionSize;
  IndexType  regionStart;
  RegionType region;

  regionStart.Fill(0);
  regionSize.Fill(7);

  region.SetIndex(regionStart);
  region.SetSize(regionSize);

  SizeType::SizeValueType size[2] = { 4, 7 };

  unsigned int count = 0;
  for (SizeType::SizeValueType s : size)
  {
    for (IndexType::IndexValueType k = -2; k < 6; ++k)
    {
      for (IndexType::IndexValueType j = -2; j < 6; ++j)
      {
        for (IndexType::IndexValueType i = -2; i < 6; ++i)
        {
          IndexType exclusionStart;
          exclusionStart[0] = i;
          exclusionStart[1] = j;
          exclusionStart[2] = k;

          SizeType exclusionSize;
          exclusionSize.Fill(s);

          RegionType exclusionRegion(exclusionStart, exclusionSize);

          count++;

          if (!RunTest(region, exclusionRegion))
          {
            std::cerr << "Test failed for exclusion region: " << exclusionRegion;
            return EXIT_FAILURE;
          }
        }
      }
    }
  }

  // Test exclusion region completely outside the region.
  IndexType exclusionStart;
  exclusionStart.Fill(-3);
  SizeType exclusionSize;
  exclusionSize.Fill(2);
  RegionType exclusionRegion(exclusionStart, exclusionSize);

  if (!RunTest(region, exclusionRegion))
  {
    std::cerr << "Test failed for exclusion region: " << exclusionRegion;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
