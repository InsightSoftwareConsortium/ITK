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

#include "itkNeighborhoodIteratorTestCommon.hxx"
#include "itkConstShapedNeighborhoodIterator.h"
#include <array>

void
PrintShapedNeighborhood(const itk::ConstShapedNeighborhoodIterator<TestImageType> & n)
{
  std::cout << n.GetIndex() << "->[";
  for (itk::ConstShapedNeighborhoodIterator<TestImageType>::ConstIterator it = n.Begin(); !it.IsAtEnd(); ++it)
  {
    std::cout << it.Get();
  }
  std::cout << ']' << std::endl;
}

int
itkConstShapedNeighborhoodIteratorTest(int, char *[])
{
  const TestImageType::Pointer                                   img = GetTestImage(10, 10, 5, 3);
  itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexType loc;
  loc[0] = 4;
  loc[1] = 4;
  loc[2] = 2;
  loc[3] = 1;

  // radius of the iterator
  itk::ConstShapedNeighborhoodIterator<TestImageType>::RadiusType radius{ 1, 1, 1, 1 };
  // region over which the iterator is defined

  constexpr itk::ConstShapedNeighborhoodIterator<TestImageType>::SizeType  sz{ 10, 10, 5, 1 };
  constexpr itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexType idx{ 0, 0, 0, 1 };
  itk::ConstShapedNeighborhoodIterator<TestImageType>::RegionType          reg = { idx, sz };

  // initialize an iterator
  std::cout << "Creating ConstShapedNeighborhoodIterator" << std::endl;
  itk::ConstShapedNeighborhoodIterator<TestImageType> it(radius, img, reg);
  it.Print(std::cout);

  std::cout << "Moving iterator using SetLocation()" << std::endl;
  it.SetLocation(loc);
  it.Print(std::cout);

  std::cout << "Testing GoToBegin()" << std::endl;
  it.GoToBegin();
  it.Print(std::cout);

  std::cout << "Testing IsAtBegin()" << std::endl;
  std::cout << it.IsAtBegin() << std::endl;

  std::cout << "Testing GoToEnd()" << std::endl;
  it.GoToEnd();
  it.Print(std::cout);

  std::cout << "Testing IsAtEnd()" << std::endl;
  std::cout << it.IsAtEnd() << std::endl;

  std::cout << "Testing forward iteration" << std::endl;
  it.GoToBegin();
  itk::ConstShapedNeighborhoodIterator<TestImageType>::OffsetType off;
  off[0] = 0;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  while (!it.IsAtEnd())
  {
    PrintShapedNeighborhood(it);
    ++it;
  }

  std::cout << "Testing reverse iteration" << std::endl;
  it.GoToEnd();
  while (!it.IsAtBegin())
  {
    PrintShapedNeighborhood(it);
    --it;
  }

  std::cout << "Moving iterator: it.GoToBegin(); it += (1, 1, 1, 1)" << std::endl;
  it.GoToBegin();
  off[0] = 1;
  off[1] = 1;
  off[2] = 1;
  off[3] = 1;
  it += off;
  PrintShapedNeighborhood(it);

  std::cout << "Moving iterator: it -= (1, 1, 1, 1)" << std::endl;
  it -= off;
  PrintShapedNeighborhood(it);

  std::cout << "Moving iterator using SetLocation()" << std::endl;
  it.SetLocation(loc);
  it.Print(std::cout);

  std::cout << "Initializing ConstShapedNeighborhoodIterator" << std::endl;
  std::cout << "...turn on [0,0,0,0], the center pixel" << std::endl;
  off[0] = 0;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  for (unsigned int r = 0; r < 1; ++r)
  {
    std::cout << "...turn on [1,0,0,0]" << std::endl;
    off[0] = 1;
    off[1] = 0;
    off[2] = 0;
    off[3] = 0;
    it.ActivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn on [1,0,0,0] again" << std::endl;
    off[0] = 1;
    off[1] = 0;
    off[2] = 0;
    off[3] = 0;
    it.ActivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn on [-1,0,0,0]" << std::endl;
    off[0] = -1;
    off[1] = 0;
    off[2] = 0;
    off[3] = 0;
    it.ActivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn on [0,-1,0,0]" << std::endl;
    off[0] = 0;
    off[1] = -1;
    off[2] = 0;
    off[3] = 0;
    it.ActivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn on [0,1,0,0]" << std::endl;
    off[0] = 0;
    off[1] = 1;
    off[2] = 0;
    off[3] = 0;
    it.ActivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn off [-1,0,0,0]" << std::endl;
    off[0] = -1;
    off[1] = 0;
    off[2] = 0;
    off[3] = 0;
    it.DeactivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn off [1,0,0,0]" << std::endl;
    off[0] = 1;
    off[1] = 0;
    off[2] = 0;
    off[3] = 0;
    it.DeactivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn off [0,1,0,0]" << std::endl;
    off[0] = 0;
    off[1] = 1;
    off[2] = 0;
    off[3] = 0;
    it.DeactivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn off [0,-1,0,0]" << std::endl;
    off[0] = 0;
    off[1] = -1;
    off[2] = 0;
    off[3] = 0;
    it.DeactivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn off [0,-1,0,0] again" << std::endl;
    off[0] = 0;
    off[1] = -1;
    off[2] = 0;
    off[3] = 0;
    it.DeactivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn off [0,0 ,0,0]" << std::endl;
    off[0] = 0;
    off[1] = 0;
    off[2] = 0;
    off[3] = 0;
    it.DeactivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn on [1,0,0,0]" << std::endl;
    off[0] = 1;
    off[1] = 0;
    off[2] = 0;
    off[3] = 0;
    it.ActivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn on [1,0,0,0] again" << std::endl;
    off[0] = 1;
    off[1] = 0;
    off[2] = 0;
    off[3] = 0;
    it.ActivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn on [-1,0,0,0]" << std::endl;
    off[0] = -1;
    off[1] = 0;
    off[2] = 0;
    off[3] = 0;
    it.ActivateOffset(off);
    it.Print(std::cout);

    std::cout << "...turn on [0,-1,0,0]" << std::endl;
    off[0] = 0;
    off[1] = -1;
    off[2] = 0;
    off[3] = 0;
    it.ActivateOffset(off);
    it.Print(std::cout);

    std::cout << " Testing it.ClearActiveList() " << std::endl;
    it.ClearActiveList();
    it.Print(std::cout);

    std::cout << " NOW REPEAT " << std::endl;
  }

  std::cout << "...turn on [1,0,0,0]" << std::endl;
  off[0] = 1;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "...turn on [1,0,0,0] again" << std::endl;
  off[0] = 1;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "...turn on [-1,0,0,0]" << std::endl;
  off[0] = -1;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "...turn on [0,-1,0,0]" << std::endl;
  off[0] = 0;
  off[1] = -1;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "...turn on [0,1,0,0]" << std::endl;
  off[0] = 0;
  off[1] = 1;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "it.GetActiveIndexListSize()=" << it.GetActiveIndexListSize();

  std::cout << "Testing GetActiveIndexList()" << std::endl;
  itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexListType l = it.GetActiveIndexList();
  auto                                                               ali = l.begin();
  while (ali != l.end())
  {
    std::cout << *ali << ' ';
    ++ali;
  }
  std::cout << std::endl;

  std::cout << "Testing const iteration through the neighborhood." << std::endl;
  {
    std::cout << "Testing using IsAtEnd()" << std::endl;
    itk::ConstShapedNeighborhoodIterator<TestImageType>::ConstIterator ci = it.Begin();
    while (!ci.IsAtEnd())
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
      ++ci;
    }
  }

  std::cout << "Testing using != it.End()" << std::endl;
  for (auto ci = it.Begin(); ci != it.End(); ++ci)
  {
    std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
  }

  std::cout << "Testing reverse iteration using != it.Begin()" << std::endl;
  auto ci = it.End();
  --ci;
  while (ci != it.Begin())
  {
    std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
    --ci;
  }
  std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;

  std::cout << std::endl;
  std::cout << "------------------------------" << std::endl;
  std::cout << std::endl;
  std::cout << "Testing activating and deactivating pixels on-the-fly" << std::endl;
  std::cout << "it.GoToBegin(); it.ClearActiveList();  Activate 1 0 0 0 and -1 0 0 0 and 0 0 0 0" << std::endl;
  it.GoToBegin();
  it.ClearActiveList();
  off[0] = -1;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);

  off[0] = 1;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);

  off[0] = 0;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);

  PrintShapedNeighborhood(it);

  std::cout << "Move the neighborhood two pixels using operator ++" << std::endl;
  ++it;
  ++it;
  PrintShapedNeighborhood(it);

  std::cout << "Clear the active list" << std::endl;
  it.ClearActiveList();
  PrintShapedNeighborhood(it);

  std::cout << "Move the neighborhood one pixel using operator ++" << std::endl;
  ++it;

  std::cout << "Reactivate the same indices" << std::endl;
  off[0] = -1;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  off[0] = 1;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  off[0] = 0;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);

  PrintShapedNeighborhood(it);

  std::cout << "Activate 0 1 0 0" << std::endl;
  off[0] = 0;
  off[1] = 1;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  PrintShapedNeighborhood(it);

  std::cout << "Testing operator=" << std::endl;
  itk::ConstShapedNeighborhoodIterator<TestImageType> oeIt;
  oeIt = it;
  PrintShapedNeighborhood(it);
  PrintShapedNeighborhood(oeIt);

  it.Print(std::cout);
  oeIt.Print(std::cout);

  int result = EXIT_SUCCESS;

  // Iterate over a region, then change the region and iterate over the new region
  {
    // Create an image
    using ChangeRegionTestImageType = itk::Image<int, 2>;
    auto                                        imageSize = ChangeRegionTestImageType::SizeType::Filled(4);
    const ChangeRegionTestImageType::RegionType imageRegion{ imageSize };

    auto image = ChangeRegionTestImageType::New();
    image->SetRegions(imageRegion);
    image->Allocate();

    itk::ImageRegionIterator<ChangeRegionTestImageType> createImageIterator(image, imageRegion);

    // Set all pixels with first index == 0 to 0, and set the rest of the image to 255
    while (!createImageIterator.IsAtEnd())
    {
      if (createImageIterator.ComputeIndex()[0] == 0)
      {
        createImageIterator.Set(0);
      }
      else
      {
        createImageIterator.Set(255);
      }

      ++createImageIterator;
    }

    // Setup and iterate over the first region
    auto region1Start = ChangeRegionTestImageType::IndexType::Filled(1);

    auto regionSize = ChangeRegionTestImageType::SizeType::Filled(1);

    const ChangeRegionTestImageType::RegionType region1(region1Start, regionSize);

    // Create the radius (a 3x3 region)
    auto neighborhoodRadius = ChangeRegionTestImageType::SizeType::Filled(1);

    // Use the first two offsets
    std::vector<itk::Offset<2>>           offsets;
    ChangeRegionTestImageType::OffsetType offset = { { -1, -1 } };
    offsets.push_back(offset);
    offset[0] = 0;
    offset[1] = 0;
    offsets.push_back(offset);

    using ShapedNeighborhoodIteratorType = itk::ConstShapedNeighborhoodIterator<ChangeRegionTestImageType>;
    ShapedNeighborhoodIteratorType shapedNeighborhoodIterator(neighborhoodRadius, image, region1);

    // Activate all of the offsets
    for (const auto & offset_item : offsets)
    {
      shapedNeighborhoodIterator.ActivateOffset(offset_item);
    }

    static constexpr std::array<int, 2> expectedValuesRegion1{ 0, 255 };

    unsigned int counter = 0;
    // while(!shapedNeighborhoodIterator.IsAtEnd()) // no need for this loop as we are only iterating over a 1x1 region
    //{
    ShapedNeighborhoodIteratorType::ConstIterator pixelIterator = shapedNeighborhoodIterator.Begin();

    while (!pixelIterator.IsAtEnd())
    {
      if (pixelIterator.Get() != expectedValuesRegion1[counter])
      {
        result = EXIT_FAILURE;
      }
      counter++;
      ++pixelIterator;
    }

    //++imageIterator;
    //}

    // Change iteration region
    auto region2start = ChangeRegionTestImageType::IndexType::Filled(2);

    const ChangeRegionTestImageType::RegionType region2(region2start, regionSize);

    shapedNeighborhoodIterator.SetRegion(region2);
    shapedNeighborhoodIterator.GoToBegin();

    static constexpr std::array<int, 2> expectedValuesRegion2{ 255, 255 };

    counter = 0;
    // while(!shapedNeighborhoodIterator.IsAtEnd()) // no need for this loop as we are only iterating over a 1x1 region
    //{
    pixelIterator = shapedNeighborhoodIterator.Begin();
    while (!pixelIterator.IsAtEnd())
    {
      if (pixelIterator.Get() != expectedValuesRegion2[counter])
      {
        result = EXIT_FAILURE;
      }
      counter++;
      ++pixelIterator;
    }
    //++imageIterator;
    //}

  } // end "Change Region" test

  return result;
}

//
// this is kind of a duff test, in that it doesn't fail w/the old code
// at runtime, it won't compile at all.  But it does at least do
// coverage of the newly exposed methods.
template <typename ImageType>
class MyDerivedCSNI : public itk::ConstShapedNeighborhoodIterator<ImageType>
{
public:
  using Superclass = itk::ConstShapedNeighborhoodIterator<ImageType>;
  using typename Superclass::SizeType;
  using typename Superclass::IndexType;
  using typename Superclass::RadiusType;
  using typename Superclass::RegionType;

  void
  TestNewExposedProtectedMembers();
  MyDerivedCSNI(const SizeType & radius, const ImageType * ptr, const RegionType & region)
    : Superclass(radius, const_cast<ImageType *>(ptr), region)
  {}
};

template <typename ImageType>
void
MyDerivedCSNI<ImageType>::TestNewExposedProtectedMembers()
{
  bool needToUseBoundaryCondition(this->GetNeedToUseBoundaryCondition());
  this->NeedToUseBoundaryConditionOn();
  this->NeedToUseBoundaryConditionOff();
  this->SetNeedToUseBoundaryCondition(needToUseBoundaryCondition);
}
