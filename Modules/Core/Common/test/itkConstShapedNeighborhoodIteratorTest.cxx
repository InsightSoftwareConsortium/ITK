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

#include "itkNeighborhoodIteratorTestCommon.hxx"
#include "itkConstShapedNeighborhoodIterator.h"

void PrintShapedNeighborhood(const itk::ConstShapedNeighborhoodIterator<TestImageType> &n)
{
  itk::ConstShapedNeighborhoodIterator<TestImageType>::ConstIterator it;
  std::cout << n.GetIndex() <<  "->[";
  for (it = n.Begin(); ! it.IsAtEnd(); ++it)
    {      std::cout << it.Get();    }
  std::cout << "]" << std::endl;
}

int itkConstShapedNeighborhoodIteratorTest(int, char* [] )
{
  TestImageType::Pointer img = GetTestImage(10, 10, 5, 3);
  itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexType loc;
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  // radius of the iterator
  itk::ConstShapedNeighborhoodIterator<TestImageType>::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  // region over which the iterator is defined
  itk::ConstShapedNeighborhoodIterator<TestImageType>::RegionType reg;
  itk::ConstShapedNeighborhoodIterator<TestImageType>::SizeType sz;
  itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexType idx;
  idx[0] = idx[1] = idx[2] = 0;  idx[3] = 1;
  sz[0] = sz[1] = 10; sz[2] = 5; sz[3] = 1;
  reg.SetIndex(idx); reg.SetSize(sz);

  // initialize an iterator
  println("Creating ConstShapedNeighborhoodIterator");
  itk::ConstShapedNeighborhoodIterator<TestImageType>
    it(radius, img, reg);
  it.Print(std::cout);

  println("Moving iterator using SetLocation()");
  it.SetLocation(loc);
  it.Print(std::cout);

  println("Testing GoToBegin()");
  it.GoToBegin();
  it.Print(std::cout);

  println("Testing IsAtBegin()");
  std::cout << it.IsAtBegin() << std::endl;

  println("Testing GoToEnd()");
  it.GoToEnd();
  it.Print(std::cout);

  println("Testing IsAtEnd()");
  std::cout << it.IsAtEnd() << std::endl;

  println("Testing forward iteration");
  it.GoToBegin();
  itk::ConstShapedNeighborhoodIterator<TestImageType>::OffsetType off;
  off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  while (! it.IsAtEnd())
    {
      PrintShapedNeighborhood(it);
      ++it;
    }

  println("Testing reverse iteration");
  it.GoToEnd();
  while (! it.IsAtBegin())
    {
      PrintShapedNeighborhood(it);
      --it;
    }

  println ("Moving iterator: it.GoToBegin(); it += (1, 1, 1, 1)");
  it.GoToBegin();
  off[0] = 1; off[1] = 1; off[2] = 1; off[3] = 1;
  it += off;
  PrintShapedNeighborhood(it);

  println ("Moving iterator: it -= (1, 1, 1, 1)");
  it -= off;
  PrintShapedNeighborhood(it);

  println("Moving iterator using SetLocation()");
  it.SetLocation(loc);
  it.Print(std::cout);

  println("Initializing ConstShapedNeighborhoodIterator");
  println("...turn on [0,0,0,0], the center pixel");
  off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  for (unsigned int r = 0; r < 1; r++)
    {
      println("...turn on [1,0,0,0]");
      off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);

      println("...turn on [1,0,0,0] again");
      off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);

      println("...turn on [-1,0,0,0]");
      off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);

      println("...turn on [0,-1,0,0]");
      off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);

      println("...turn on [0,1,0,0]");
      off[0] = 0; off[1] = 1; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);

      println("...turn off [-1,0,0,0]");
      off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);

      println("...turn off [1,0,0,0]");
      off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);

      println("...turn off [0,1,0,0]");
      off[0] = 0; off[1] = 1; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);

      println("...turn off [0,-1,0,0]");
      off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);

      println("...turn off [0,-1,0,0] again");
      off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);

      println("...turn off [0,0 ,0,0]");
      off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);

      println("...turn on [1,0,0,0]");
      off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);

      println("...turn on [1,0,0,0] again");
      off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);

      println("...turn on [-1,0,0,0]");
      off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);

      println("...turn on [0,-1,0,0]");
      off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);

      println(" Testing it.ClearActiveList() ");
      it.ClearActiveList();
      it.Print(std::cout);

      println(" NOW REPEAT " );
    }

  println("...turn on [1,0,0,0]");
  off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  println("...turn on [1,0,0,0] again");
  off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  println("...turn on [-1,0,0,0]");
  off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  println("...turn on [0,-1,0,0]");
  off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  println("...turn on [0,1,0,0]");
  off[0] = 0; off[1] = 1; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "it.GetActiveIndexListSize()="
            << it.GetActiveIndexListSize();

  println("Testing GetActiveIndexList()");
  itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexListType l
    = it.GetActiveIndexList();
  itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexListType
    ::const_iterator ali = l.begin();
  while (ali != l.end())
    {
      std::cout << *ali << " ";
      ++ali;
    }
  std::cout << std::endl;

  println("Testing const iteration through the neighborhood.");
  itk::ConstShapedNeighborhoodIterator<TestImageType>::ConstIterator
    ci = it.Begin();

  println("Testing using IsAtEnd()");
  while (! ci.IsAtEnd())
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
      ci++;
    }


  println("Testing using != it.End()");
  for (ci = it.Begin(); ci != it.End(); ++ci)
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
    }

  println("Testing reverse iteration using != it.Begin()");
  ci = it.End();
  --ci;
  while (ci != it.Begin())
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
      ci--;
    }
  std::cout << ci.GetNeighborhoodIndex() << " -> "
            << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;

  std::cout << std::endl;
  std::cout << "------------------------------" << std::endl;
  std::cout << std::endl;
  println("Testing activating and deactivating pixels on-the-fly");
  println("it.GoToBegin(); it.ClearActiveList();  Activate 1 0 0 0 and -1 0 0 0 and 0 0 0 0");
  it.GoToBegin();
  it.ClearActiveList();
  off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);

  off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);

  off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);

  PrintShapedNeighborhood(it);

  println("Move the neighborhood two pixels using operator ++");
  ++it;
  ++it;
  PrintShapedNeighborhood(it);

  println("Clear the active list");
  it.ClearActiveList();
  PrintShapedNeighborhood(it);

  println("Move the neighborhood one pixel using operator ++");
  ++it;

  println("Reactivate the same indices");
  off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);

  PrintShapedNeighborhood(it);

  println("Activate 0 1 0 0");
  off[0] = 0; off[1] = 1; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  PrintShapedNeighborhood(it);

  println("Testing operator=");
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
    typedef itk::Image<int, 2> ChangeRegionTestImageType;
    ChangeRegionTestImageType::IndexType imageCorner;
    imageCorner.Fill(0);

    ChangeRegionTestImageType::SizeType imageSize;
    imageSize.Fill(4);

    ChangeRegionTestImageType::RegionType imageRegion(imageCorner, imageSize);

    ChangeRegionTestImageType::Pointer image = ChangeRegionTestImageType::New();
    image->SetRegions(imageRegion);
    image->Allocate();

    itk::ImageRegionIterator<ChangeRegionTestImageType> createImageIterator(image, imageRegion);

    // Set all pixels with first index == 0 to 0, and set the rest of the image to 255
    while(!createImageIterator.IsAtEnd())
      {
      if(createImageIterator.GetIndex()[0] == 0)
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
    ChangeRegionTestImageType::IndexType region1Start;
    region1Start.Fill(1);

    ChangeRegionTestImageType::SizeType regionSize;
    regionSize.Fill(1);

    ChangeRegionTestImageType::RegionType region1(region1Start, regionSize);

    // Create the radius (a 3x3 region)
    ChangeRegionTestImageType::SizeType neighborhoodRadius;
    neighborhoodRadius.Fill(1);

    // Use the first two offsets
    std::vector<itk::Offset<2> > offsets;
    ChangeRegionTestImageType::OffsetType offset = {{-1,-1}};
    offsets.push_back(offset);
    offset[0] = 0;
    offset[1] = 0;
    offsets.push_back(offset);

    typedef itk::ConstShapedNeighborhoodIterator<ChangeRegionTestImageType> ShapedNeighborhoodIteratorType;
    ShapedNeighborhoodIteratorType shapedNeighborhoodIterator(neighborhoodRadius, image, region1);

    // Activate all of the offsets
    for(size_t i = 0; i < offsets.size(); ++i)
    {
      shapedNeighborhoodIterator.ActivateOffset(offsets[i]);
    }

    std::vector<int> expectedValuesRegion1(2);
    expectedValuesRegion1[0] = 0;
    expectedValuesRegion1[1] = 255;

    unsigned int counter = 0;
    //while(!shapedNeighborhoodIterator.IsAtEnd()) // no need for this loop as we are only iterating over a 1x1 region
      //{
      ShapedNeighborhoodIteratorType::ConstIterator pixelIterator = shapedNeighborhoodIterator.Begin();

      while (!pixelIterator.IsAtEnd())
        {
        if(pixelIterator.Get() != expectedValuesRegion1[counter])
          {
          result = EXIT_FAILURE;
          }
        counter++;
        ++pixelIterator;
        }

      //++imageIterator;
      //}

    // Change iteration region
    ChangeRegionTestImageType::IndexType region2start;
    region2start.Fill(2);

    ChangeRegionTestImageType::RegionType region2(region2start, regionSize);

    shapedNeighborhoodIterator.SetRegion(region2);
    shapedNeighborhoodIterator.GoToBegin();

    std::vector<int> expectedValuesRegion2(2);
    expectedValuesRegion2[0] = 255;
    expectedValuesRegion2[1] = 255;

    counter = 0;
    //while(!shapedNeighborhoodIterator.IsAtEnd()) // no need for this loop as we are only iterating over a 1x1 region
      //{
    pixelIterator = shapedNeighborhoodIterator.Begin();
    while (!pixelIterator.IsAtEnd())
      {
      if(pixelIterator.Get() != expectedValuesRegion2[counter])
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
  typedef typename itk::ConstShapedNeighborhoodIterator<ImageType> Superclass;
  typedef typename Superclass::SizeType                            SizeType;
  typedef typename Superclass::IndexType                           IndexType;
  typedef typename Superclass::RadiusType                          RadiusType;
  typedef typename Superclass::RegionType                          RegionType;

  void TestNewExposedProtectedMembers();
  MyDerivedCSNI(const SizeType & radius,
                const ImageType *ptr,
                const RegionType & region):
    Superclass (radius, const_cast< ImageType * >( ptr ), region)
    {
    }
};

template <typename ImageType>
void
MyDerivedCSNI<ImageType>
::TestNewExposedProtectedMembers()
{
  bool needToUseBoundaryCondition(this->GetNeedToUseBoundaryCondition());
  this->NeedToUseBoundaryConditionOn();
  this->NeedToUseBoundaryConditionOff();
  this->SetNeedToUseBoundaryCondition(needToUseBoundaryCondition);
}
