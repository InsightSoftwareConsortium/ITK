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


// This routine is used to make sure that we call the "const" version
// of GetPixel() (via the operator[])
template <typename T, unsigned int VImageDimension>
void TestConstPixelAccess(const itk::Image<T, VImageDimension> &in,
                          itk::Image<T, VImageDimension> &out)
{
  typename itk::Image<T, VImageDimension>::IndexType regionStartIndex3D = {{5, 10, 15}};
  typename itk::Image<T, VImageDimension>::IndexType regionEndIndex3D = {{8, 15, 17}};

  T vec;

  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  out[regionStartIndex3D] = vec;
  out[regionEndIndex3D] = in[regionStartIndex3D];
}


int itkImageRegionIteratorTest(int, char* [] )
{
  std::cout << "Creating an image" << std::endl;
  itk::Image<itk::Vector<unsigned short, 5>, 3>::Pointer
    o3 = itk::Image<itk::Vector<unsigned short, 5>, 3>::New();

  int status = 0;

  float origin3D[3] = { 5.0f, 2.1f, 8.1f};
  float spacing3D[3] = { 1.5f, 2.1f, 1.0f};

  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType imageSize3D = {{ 20, 40, 60 }};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType bufferSize3D = {{ 8, 20, 14 }};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType regionSize3D = {{ 4,  6,  6 }};

  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType startIndex3D = {{5, 4, 1}};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType bufferStartIndex3D = {{2, 3, 5}};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType regionStartIndex3D = {{5, 10, 12}};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType regionEndIndex3D = {{8, 15, 17}};


  itk::Image<itk::Vector<unsigned short, 5>, 3>::RegionType region;
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

  std::cout << "Setting/Getting a pixel" << std::endl;
  itk::Vector<unsigned short, 5> vec;

  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  (*o3)[regionStartIndex3D] = vec;
  (*o3)[regionEndIndex3D] = (*o3)[regionStartIndex3D];
  TestConstPixelAccess(*o3, *o3);


  itk::ImageIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > standardIt(o3, region);

  // Iterate over a region using a simple for loop
  itk::ImageRegionIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > it(o3, region);

  std::cout << "Simple iterator loop: ";
  for (; !it.IsAtEnd(); ++it)
    {
    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType index = it.GetIndex();
    std::cout << index << std::endl;
    }

  itk::ImageRegionConstIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > standardCIt(o3, region);

  // Iterate over a region using a simple for loop and a const iterator
  itk::ImageRegionConstIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > cit(o3, region);

  std::cout << "Simple const iterator loop: ";
  for (; !cit.IsAtEnd(); ++cit)
    {
    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType index = cit.GetIndex();
    std::cout << index << std::endl;
    }


  // Iterator over the region backwards using a simple for loop
  itk::ImageRegionIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > backIt(o3, region);

  backIt.GoToEnd(); // one pixel past the end of the region
  do
    {
    --backIt;

    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType index = backIt.GetIndex();
    std::cout << "Simple iterator backwards loop: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;
    }
  while (!backIt.IsAtBegin()); // stop when we reach the beginning

  // Iterate over a region, then change the region and iterate over the new region
  {
    // Create an image
    typedef itk::Image<int, 2> TestImageType;
    TestImageType::IndexType imageCorner;
    imageCorner.Fill(0);

    TestImageType::SizeType imageSize;
    imageSize.Fill(3);

    TestImageType::RegionType imageRegion(imageCorner, imageSize);

    TestImageType::Pointer image = TestImageType::New();
    image->SetRegions(imageRegion);
    image->Allocate();

    itk::ImageRegionIterator<TestImageType> createImageIterator(image,imageRegion);

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
    TestImageType::IndexType region1Start;
    region1Start.Fill(0);

    TestImageType::SizeType regionSize;
    regionSize.Fill(2);

    TestImageType::RegionType region1(region1Start, regionSize);

    itk::ImageRegionConstIterator<TestImageType> imageIterator(image,region1);

    std::vector<int> expectedValuesRegion1(4);
    expectedValuesRegion1[0] = 0;
    expectedValuesRegion1[1] = 255;
    expectedValuesRegion1[2] = 0;
    expectedValuesRegion1[3] = 255;
    unsigned int counter = 0;
    while(!imageIterator.IsAtEnd())
      {
      if(imageIterator.Get() != expectedValuesRegion1[counter])
        {
        status = 1; // Fail
        }
      counter++;
      ++imageIterator;
      }

    // Change iteration region
    TestImageType::IndexType region2start;
    region2start.Fill(1);

    TestImageType::RegionType region2(region2start, regionSize);

    imageIterator.SetRegion(region2);
    imageIterator.GoToBegin();

    std::vector<int> expectedValuesRegion2(4);
    expectedValuesRegion2[0] = 255;
    expectedValuesRegion2[1] = 255;
    expectedValuesRegion2[2] = 255;
    expectedValuesRegion2[3] = 255;
    counter = 0;
    while(!imageIterator.IsAtEnd())
      {
      if(imageIterator.Get() != expectedValuesRegion2[counter])
        {
        status = 1; // Fail
        }
      counter++;
      ++imageIterator;
      }

  } // end "Change Region" test

  if (status == 0)
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    }
  return status;
}
