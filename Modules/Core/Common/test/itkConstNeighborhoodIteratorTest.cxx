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
#include "itkConstNeighborhoodIterator.h"

void println(const char *s)
{
  std::cout << s << std::endl;
}

TestImageType::Pointer GetTestImage(int d1, int d2, int d3, int d4)
{
  itk::Size<4>  sizeND;
   sizeND[0] = d1;
   sizeND[1] = d2;
   sizeND[2] = d3;
   sizeND[3] = d4;

  itk::Index<4> origND;
   origND.Fill(0);

  itk::ImageRegion<4> RegionND;
   RegionND.SetSize(sizeND);
   RegionND.SetIndex(origND);

  TestImageType::Pointer imageND = TestImageType::New();
   imageND->SetLargestPossibleRegion(RegionND);
   imageND->SetBufferedRegion(RegionND);
   imageND->SetRequestedRegion(RegionND);
   imageND->Allocate();

  FillImage<4>(imageND.GetPointer());

  return  imageND;
}

int itkConstNeighborhoodIteratorTest(int, char* [] )
{
  TestImageType::Pointer img = GetTestImage(10, 10, 5, 3);
  itk::ConstNeighborhoodIterator<TestImageType>::IndexType loc;
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  itk::ConstNeighborhoodIterator<TestImageType>::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  itk::ConstNeighborhoodIterator<TestImageType>::RegionType reg;
  itk::ConstNeighborhoodIterator<TestImageType>::SizeType sz;
  itk::ConstNeighborhoodIterator<TestImageType>::IndexType idx;
  idx[0] = idx[1] = idx[2] = 0;  idx[3] = 1;
  sz[0] = sz[1] = 10; sz[2] = 5; sz[3] = 1;
  reg.SetIndex(idx); reg.SetSize(sz);

  println("Creating ConstNeighborhoodIterator");
  itk::ConstNeighborhoodIterator<TestImageType>
     it(radius, img, reg);

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
  while (! it.IsAtEnd())
    {
      printnb<itk::ConstNeighborhoodIterator<TestImageType> >(it, false);
      ++it;
    }

  println("Testing reverse iteration");
  it.GoToEnd();
  while (! it.IsAtBegin())
    {
      --it;
      printnb<itk::ConstNeighborhoodIterator<TestImageType> >(it, false);
    }

  println("Moving iterator using SetLocation()");
  it.SetLocation(loc);
  it.Print(std::cout);

  println("Testing GetNeighborhood()");
  it.GetNeighborhood().Print(std::cout);

  println("Printing neighborhood using GetPixel(i), GetPixel(offset) and GetIndex(i), and GetIndex(offset).");
  for (unsigned int j = 0; j < it.Size(); ++j)
    {
      std::cout << "GetOffset(" << j << ")=" << it.GetOffset(j);
      std::cout << " GetPixel(" << j << ")=" << it.GetPixel(j);
      std::cout << " GetPixel(" << it.GetOffset(j) << ")=" << it.GetPixel(it.GetOffset(j));
      std::cout << " GetIndex(" << j << ")=" << it.GetIndex(j);
      std::cout << " GetIndex(" << it.GetOffset(j) << ")=" << it.GetIndex(it.GetOffset(j));
      std::cout << std::endl;
    }

  println("Testing GetCenterPixel()");
  std::cout << it.GetCenterPixel() << std::endl;

  println("Testing GetCenterPointer()");
  std::cout << it.GetCenterPointer() << " = "
            << *(it.GetCenterPointer()) << std::endl;

  println("Testing GetIndex()");
  std::cout << it.GetIndex() << std::endl;

  println("Testing GetNext(3)");
  std::cout << it.GetNext(3) << std::endl;

  println("Testing GetNext(2)");
  std::cout << it.GetNext(2) << std::endl;

  println("Testing GetNext(1)");
  std::cout << it.GetNext(1) << std::endl;

  println("Testing GetNext(0) = GetNext(0,1)");
  std::cout << it.GetNext(0) << "=" << it.GetNext(0,1) <<  std::endl;

  println("Testing GetNext(0, 1)");
  std::cout << it.GetNext(0,1) << std::endl;

  println("Testing GetNext(1, 1)");
  std::cout << it.GetNext(1,1) << std::endl;

  println("Testing GetPrevious(3)");
  std::cout << it.GetPrevious(3) << std::endl;

  println("Testing GetPrevious(2)");
  std::cout << it.GetPrevious(2) << std::endl;

  println("Testing GetPrevious(1)");
  std::cout << it.GetPrevious(1) << std::endl;

  println("Testing GetPrevious(0) = GetPrevious(0,1)");
  std::cout << it.GetPrevious(0) << "=" << it.GetPrevious(0,1) <<  std::endl;

  println("Testing GetPrevious(0, 1)");
  std::cout << it.GetPrevious(0,1) << std::endl;

  println("Testing GetPrevious(1, 1)");
  std::cout << it.GetPrevious(1,1) << std::endl;

  println("Testing GetBoundingBoxAsImageRegion");
  std::cout << it.GetBoundingBoxAsImageRegion() << std::endl;

  println("Testing random access iteration");

  TestImageType::Pointer ra_img = GetTestImage(10, 10, 5, 3);
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  println("Creating ConstNeighborhoodIterator");
  itk::ConstNeighborhoodIterator<TestImageType>
     ra_it(radius, ra_img, ra_img->GetRequestedRegion());

  println("Testing random access");
  ra_it.Begin();
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  println("Adding [1, 1, 1, 1]");
  OffsetType a_off;
  a_off.Fill(1);
  ra_it += a_off;
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  println("Subtracting [1, 1, 1, 1]");
  ra_it -= a_off;
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  println("Adding [0 0 0 2]");
  a_off.Fill(0);
  a_off[3] = 2;
  ra_it += a_off;
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  println("Adding [0 8 0 0]");
  a_off.Fill(0);
  a_off[1] = 8;
  ra_it += a_off;
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  println("Adding [5 -3 2 -1]");
  a_off[0] = 5;
  a_off[1] = -3;
  a_off[2] = 2;
  a_off[3] = -1;
  ra_it += a_off;
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  //
  // Test IndexInBounds
  //
  println("Testing IndexInBounds");
  int dims[4] = {13,11,9,7};
  TestImageType::Pointer iib_img = GetTestImage(dims[0], dims[1], dims[2], dims[3]);
  radius[0] = 4;
  radius[1] = 3;
  radius[2] = 2;
  radius[3] = 1;

  println("Creating ConstNeighborhoodIterator");
  typedef itk::ConstNeighborhoodIterator<TestImageType> IteratorType;
  IteratorType iib_it(radius, iib_img, iib_img->GetRequestedRegion());
  IteratorType::OffsetType resultOffset;
  IteratorType::OffsetType internalIndex;

  IteratorType::IndexType centerLoc;
  centerLoc[0] = static_cast<IteratorType::IndexType::IndexValueType>(dims[0] / 2);
  centerLoc[1] = static_cast<IteratorType::IndexType::IndexValueType>(dims[1] / 2);
  centerLoc[2] = static_cast<IteratorType::IndexType::IndexValueType>(dims[2] / 2);
  centerLoc[3] = static_cast<IteratorType::IndexType::IndexValueType>(dims[3] / 2);

  iib_it.GoToBegin();
  bool inside = iib_it.IndexInBounds( 0, internalIndex, resultOffset );
  if( inside )
    {
    std::cerr << "IndexInBounds failed for index 0, expected false." << std::endl;
    return EXIT_FAILURE;
    }
  for( unsigned int n = 0; n < 4; n++ )
    {
    if( resultOffset[n] != static_cast<itk::OffsetValueType>( radius[n] ) )
      {
      std::cerr << "IndexInBounds failed. Expected resultOffset of " << radius << ", but got " << resultOffset << std::endl;
      return EXIT_FAILURE;
      }
    }
  inside = iib_it.IndexInBounds( iib_it.Size()-1, internalIndex, resultOffset );
  if( ! inside )
    {
    std::cerr << "IndexInBounds failed for index size-1, expected true." << std::endl;
    return EXIT_FAILURE;
    }
  for( unsigned int n = 0; n < 4; n++ )
    {
    if( resultOffset[n] != 0 )
      {
      std::cerr << "IndexInBounds failed. Expected resultOffset of 0, but got " << resultOffset << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Test min boundary by dimension
  int result = EXIT_SUCCESS;
  for( unsigned int n = 0; n < 4; n++ )
    {
    IteratorType::IndexType boundaryLoc = centerLoc;
    boundaryLoc[n] = 0;
    iib_it.SetLocation( boundaryLoc );
    if( iib_it.IndexInBounds( 0, internalIndex, resultOffset ) )
      {
      std::cerr << "IndexInBounds failed for min boundaryLoc: " << boundaryLoc << " and dimension: " << n << ". Expected false."
                << std::endl;
      result = EXIT_FAILURE;
      }
    }

  // Test max boundary by dimension
  for( unsigned int n = 0; n < 4; n++ )
    {
    IteratorType::IndexType boundaryLoc = centerLoc;
    boundaryLoc[n] = dims[n]-1;
    iib_it.SetLocation( boundaryLoc );
    if( iib_it.IndexInBounds( iib_it.Size()-1, internalIndex, resultOffset ) )
      {
      std::cerr << "IndexInBounds failed for max boundaryLoc: " << boundaryLoc << " and dimension: " << n << ". Expected false."
                << std::endl;
      result = EXIT_FAILURE;
      }
    }

  // Test center
  iib_it.SetLocation( centerLoc );
  inside = iib_it.IndexInBounds( 0, internalIndex, resultOffset );
  if( ! inside )
    {
    std::cerr << "IndexInBounds failed for index 0, expected true." << std::endl;
    result = EXIT_FAILURE;
    }

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

    typedef itk::ConstNeighborhoodIterator<ChangeRegionTestImageType> NeighborhoodIteratorType;
    NeighborhoodIteratorType neighborhoodIterator(neighborhoodRadius, image, region1);

    std::vector<int> expectedValuesRegion1(9);
    expectedValuesRegion1[0] = 0;
    expectedValuesRegion1[1] = 255;
    expectedValuesRegion1[2] = 255;
    expectedValuesRegion1[3] = 0;
    expectedValuesRegion1[4] = 255;
    expectedValuesRegion1[5] = 255;
    expectedValuesRegion1[6] = 0;
    expectedValuesRegion1[7] = 255;
    expectedValuesRegion1[8] = 255;
    unsigned int counter = 0;

    for(NeighborhoodIteratorType::ConstIterator pixelIterator = neighborhoodIterator.Begin();
        pixelIterator < neighborhoodIterator.End();
        ++pixelIterator)
      {
      if(**pixelIterator != expectedValuesRegion1[counter])
        {
        result = EXIT_FAILURE;
        }
      counter++;
      }

    // Change iteration region
    ChangeRegionTestImageType::IndexType region2start;
    region2start.Fill(2);

    ChangeRegionTestImageType::RegionType region2(region2start, regionSize);

    neighborhoodIterator.SetRegion(region2);
    neighborhoodIterator.GoToBegin();

    std::vector<int> expectedValuesRegion2(9);
    expectedValuesRegion2[0] = 255;
    expectedValuesRegion2[1] = 255;
    expectedValuesRegion2[2] = 255;
    expectedValuesRegion2[3] = 255;
    expectedValuesRegion2[4] = 255;
    expectedValuesRegion2[5] = 255;
    expectedValuesRegion2[6] = 255;
    expectedValuesRegion2[7] = 255;
    expectedValuesRegion2[8] = 255;
    counter = 0;
    for(NeighborhoodIteratorType::ConstIterator pixelIterator = neighborhoodIterator.Begin();
        pixelIterator < neighborhoodIterator.End();
        ++pixelIterator)
      {
      if(**pixelIterator != expectedValuesRegion2[counter])
        {
        result = EXIT_FAILURE;
        }
      counter++;
      }

  } // end "Change Region" test

  return result;

}
