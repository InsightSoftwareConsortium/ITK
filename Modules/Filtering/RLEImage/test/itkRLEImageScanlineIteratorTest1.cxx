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

#include "itkRLEImage.h"

// This routine is used to make sure that we call the "const" version
// of GetPixel() (via the operator[])
template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
TestConstPixelAccess(const itk::RLEImage<TPixel, VImageDimension, CounterType> & in,
                     itk::RLEImage<TPixel, VImageDimension, CounterType> &       out)
{
  typename itk::RLEImage<TPixel, VImageDimension, CounterType>::IndexType regionStartIndex3D = { { 5, 10, 15 } };
  typename itk::RLEImage<TPixel, VImageDimension, CounterType>::IndexType regionEndIndex3D = { { 8, 15, 17 } };

  TPixel vec;

  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  out.SetPixel(regionStartIndex3D, vec);
  out.SetPixel(regionEndIndex3D, in[regionStartIndex3D]);
  // out[regionStartIndex3D] = vec;
  // out[regionEndIndex3D] = in[regionStartIndex3D];
}


int
itkRLEImageScanlineIteratorTest1(int, char *[])
{
  itk::RLEImage<itk::Vector<unsigned short, 5>>::Pointer o3 = itk::RLEImage<itk::Vector<unsigned short, 5>>::New();

  int status = EXIT_SUCCESS;

  float origin3D[3] = { 5.f, 2.1f, 8.1f };
  float spacing3D[3] = { 1.5f, 2.1f, 1.f };

  using ImageType = itk::RLEImage<itk::Vector<unsigned short, 5>>;

  ImageType::SizeType imageSize3D = { { 20, 40, 60 } };
  ImageType::SizeType bufferSize3D = { { 20, 20, 14 } };
  ImageType::SizeType regionSize3D = { { 4, 6, 6 } };

  ImageType::IndexType startIndex3D = { { 5, 4, 1 } };
  ImageType::IndexType bufferStartIndex3D = { { 5, 3, 5 } };
  ImageType::IndexType regionStartIndex3D = { { 5, 10, 12 } };
  ImageType::IndexType regionEndIndex3D = { { 8, 15, 17 } };


  ImageType::RegionType region;
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

  std::cout << "Setting/Getting a pixel" << std::endl;
  itk::Vector<unsigned short, 5> vec;

  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  (*o3).SetPixel(regionStartIndex3D, vec);
  (*o3).SetPixel(regionEndIndex3D, (*o3)[regionStartIndex3D]);
  TestConstPixelAccess(*o3, *o3);


  itk::ImageIterator<ImageType> standardIt(o3, region);

  // Iterate over a region using a simple for loop
  itk::ImageScanlineIterator<ImageType> it(standardIt);

  std::cout << "Simple iterator loop 1\n";
  while (!it.IsAtEnd())
  {
    while (!it.IsAtEndOfLine())
    {
      ++it;
    }
    it.NextLine();
  }

  itk::ImageScanlineIterator<ImageType> it2(o3, o3->GetBufferedRegion());
  std::cout << "Simple iterator loop 1bis\n";
  while (!it2.IsAtEnd())
  {
    while (!it2.IsAtEndOfLine())
    {
      ++it2;
    }
    it2.NextLine();
  }

  itk::ImageScanlineConstIterator<ImageType> testBeginEnd(o3, region);
  testBeginEnd.GoToBeginOfLine();
  testBeginEnd.GoToEndOfLine();

  itk::ImageScanlineConstIterator<ImageType> standardCIt(o3, region);

  // Iterate over a region using a simple loop and a const iterator
  itk::ImageScanlineConstIterator<ImageType> cit(standardIt);

  std::cout << "Simple const iterator loop 2\n";
  while (!cit.IsAtEnd())
  {
    while (!cit.IsAtEndOfLine())
    {
      ++cit;
    }
    cit.NextLine();
  }

  while (!cit.IsAtEnd())
  {
    cit.NextLine();
  }


  // Iterate over a region, then change the region and iterate over the new region
  {
    // Create an image
    using TestImageType = itk::Image<int, 2>;
    TestImageType::IndexType imageCorner;
    imageCorner.Fill(0);

    TestImageType::SizeType imageSize;
    imageSize.Fill(3);

    TestImageType::RegionType imageRegion(imageCorner, imageSize);

    TestImageType::Pointer image = TestImageType::New();
    image->SetRegions(imageRegion);
    image->Allocate();

    itk::ImageScanlineIterator<TestImageType> createImageIterator(image, imageRegion);

    // Set all pixels with first index == 0 to 0, and set the rest of the image to 255
    while (!createImageIterator.IsAtEnd())
    {
      while (!createImageIterator.IsAtEndOfLine())
      {
        if (createImageIterator.GetIndex()[0] == 0)
        {
          createImageIterator.Set(0);
        }
        else
        {
          createImageIterator.Set(255);
        }
        ++createImageIterator;
      }
      createImageIterator.NextLine();
    }

    // Setup and iterate over the first region
    TestImageType::IndexType region1Start;
    region1Start.Fill(0);

    TestImageType::SizeType regionSize;
    regionSize.Fill(2);

    TestImageType::RegionType region1(region1Start, regionSize);

    itk::ImageScanlineConstIterator<TestImageType> imageIterator(image, region1);

    std::vector<int> expectedValuesRegion1(4);
    expectedValuesRegion1[0] = 0;
    expectedValuesRegion1[1] = 255;
    expectedValuesRegion1[2] = 0;
    expectedValuesRegion1[3] = 255;
    unsigned int counter = 0;
    while (!imageIterator.IsAtEnd())
    {
      while (!imageIterator.IsAtEndOfLine())
      {
        if (imageIterator.Get() != expectedValuesRegion1[counter])
        {
          status = EXIT_FAILURE; // Fail
        }
        counter++;
        ++imageIterator;
      }
      imageIterator.NextLine();
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
    while (!imageIterator.IsAtEnd())
    {
      while (!imageIterator.IsAtEndOfLine())
      {
        if (imageIterator.Get() != expectedValuesRegion2[counter])
        {
          status = EXIT_FAILURE; // Fail
        }
        counter++;
        ++imageIterator;
      }
      imageIterator.NextLine();
    }

  } // end "Change Region" test
  if (status == EXIT_SUCCESS)
    std::cout << "All scanline iterator tests successful!\n";
  else
    std::cout << "Some scanline iterator tests failed!\n";
  return status;
}
