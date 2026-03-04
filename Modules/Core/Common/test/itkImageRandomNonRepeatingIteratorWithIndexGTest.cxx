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

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRandomNonRepeatingIteratorWithIndex.h"
#include "itkGTest.h"


TEST(ImageRandomNonRepeatingIteratorWithIndex, SupportsSubregions)
{
  constexpr unsigned int ImageDimension{ 3 };
  using PixelType = itk::Index<ImageDimension>;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using PriorityPixelType = itk::SizeValueType;
  using PriorityImageType = itk::Image<PriorityPixelType, ImageDimension>;
  using IteratorType = itk::ImageRegionIteratorWithIndex<ImageType>;
  using PriorityIteratorType = itk::ImageRegionIteratorWithIndex<PriorityImageType>;
  using RandomIteratorType = itk::ImageRandomNonRepeatingIteratorWithIndex<ImageType>;
  using RandomConstIteratorType = itk::ImageRandomNonRepeatingConstIteratorWithIndex<ImageType>;
  std::cout << "Creating images" << std::endl;


  constexpr ImageType::SizeType prioritySize{ 50, 50, 50 };
  constexpr auto                numberOfPixelsSize0 = prioritySize.CalculateProductOfElements();
  constexpr unsigned long       numberOfSamples{ 10 };
  const ImageType::RegionType   priorityRegion{ prioritySize };

  // Make the priority image
  constexpr ImageType::IndexType        start{ 10, 12, 14 };
  constexpr ImageType::SizeType         size{ 11, 12, 13 };
  constexpr PriorityImageType::SizeType subsize{ 3, 4, 5 };
  constexpr auto                        numberOfPixelsSub = subsize.CalculateProductOfElements();

  auto priorityImage = PriorityImageType::New();
  {
    priorityImage->SetRegions(priorityRegion);
    priorityImage->Allocate();
    // we will make most of this image of ones, with a small region of
    // zeros.  Then pixels from the zero region should be selected
    // preferentially.
    std::cout << "Building Priority image" << std::endl;
    priorityImage->FillBuffer(1);
  }

  {
    {
      constexpr PriorityImageType::IndexType substart{ 15, 16, 17 };
      const PriorityImageType::RegionType    subRegionOfZeros{ substart, subsize };
      PriorityIteratorType                   subit(priorityImage, subRegionOfZeros);
      subit.GoToBegin();
      while (!subit.IsAtEnd())
      {
        subit.Set(0);
        ++subit;
      }
    }
  }
  auto myImage = ImageType::New();
  myImage->SetRegions(priorityRegion);
  myImage->Allocate();
  const ImageType::ConstPointer myConstImage = myImage;
  {
    //********
    std::cout << "Filling image with indices" << std::endl;

    RandomIteratorType it(myImage, priorityRegion);
    it.SetNumberOfSamples(numberOfPixelsSize0);
    it.GoToBegin();
    {
      ImageType::IndexType indexFill0;
      // Because the random iterator does not repeat, this should
      // fill the image with indices
      while (!it.IsAtEnd())
      {
        indexFill0 = it.GetIndex();
        it.Set(indexFill0);
        ++it;
      }
    }
  }
  {
    // Sample the image
    IteratorType ot(myImage, priorityRegion);
    ot.GoToBegin();
    // if it repeated it is going to have missed a few.
    std::cout << "Verifying iterators... ";
    while (!ot.IsAtEnd())
    {
      ImageType::IndexType indexMatch = ot.GetIndex();
      EXPECT_EQ(ot.Get(), indexMatch) << "Values don't correspond to what was stored "
                                      << "Test failed at index " << indexMatch;

      ++ot;
    }
    std::cout << std::endl << "   Done ! " << std::endl;
  }
  {
    // Verification
    RandomConstIteratorType cot(myConstImage, priorityRegion);
    cot.SetNumberOfSamples(numberOfSamples);
    cot.GoToBegin();

    std::cout << "Verifying const iterator... ";
    std::cout << "Random walk of the Iterator over the image " << std::endl;
    ImageType::IndexType indexConstMatch;
    while (!cot.IsAtEnd())
    {
      indexConstMatch = cot.GetIndex();
      EXPECT_EQ(cot.Get(), indexConstMatch)
        << "Values don't correspond to what was stored " << std::endl
        << "Test failed at index " << indexConstMatch << " value is " << cot.Get() << std::endl;
      std::cout << indexConstMatch << std::endl;
      ++cot;
    }
  }
  {
    // Verification
    std::cout << "Verifying iterator in reverse direction... " << std::endl;
    std::cout << "Should be a random walk too (a different one)" << std::endl;
    RandomIteratorType ior(myImage, priorityRegion);
    ior.SetNumberOfSamples(numberOfSamples);
    ior.GoToEnd();
    --ior;
    ImageType::IndexType indexReverse;
    while (!ior.IsAtBegin())
    {
      indexReverse = ior.GetIndex();
      EXPECT_EQ(ior.Get(), indexReverse) << "Values don't correspond to what was stored " << std::endl
                                         << "Test failed at index " << indexReverse << " value is " << ior.Get()
                                         << std::endl;
      std::cout << indexReverse << std::endl;
      --ior;
    }
    std::cout << indexReverse << std::endl; // print the value at the beginning index
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Verification
    std::cout << "Verifying const iterator in reverse direction... ";
    RandomConstIteratorType cor(myImage, priorityRegion);
    cor.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
    cor.GoToEnd();
    --cor; // start at the end position
    ImageType::IndexType indexConstReverse;
    while (!cor.IsAtBegin())
    {
      indexConstReverse = cor.GetIndex();
      EXPECT_EQ(cor.Get(), indexConstReverse)
        << "Values don't correspond to what was stored " << std::endl
        << "Test failed at index " << indexConstReverse << " value is " << cor.Get() << std::endl;
      std::cout << indexConstReverse << std::endl;
      --cor;
    }
    std::cout << indexConstReverse << std::endl; // print the value at the beginning index
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Verification
    std::cout << "Verifying const iterator in both directions... ";
    RandomConstIteratorType dor(myImage, priorityRegion);
    dor.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
    dor.GoToEnd();
    --dor; // start at the last valid pixel position
    ImageType::IndexType indexBiDirectional;
    for (unsigned int counter = 0; !dor.IsAtEnd(); ++counter)
    {
      indexBiDirectional = dor.GetIndex();
      EXPECT_EQ(dor.Get(), indexBiDirectional)
        << "Values don't correspond to what was stored " << std::endl
        << "Test failed at index " << indexBiDirectional << " value is " << dor.Get() << std::endl;
      std::cout << indexBiDirectional << std::endl;
      if (counter < 6)
      {
        --dor;
      }
      else
      {
        ++dor;
      }
    }
    std::cout << indexBiDirectional << std::endl; // print the value at the beginning index
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Verification of the Iterator in a subregion of the image
    std::cout << "Verifying Iterator in a Region smaller than the whole image... " << std::endl;
    const ImageType::RegionType region{ start, size };
    RandomIteratorType          cbot(myImage, region);
    cbot.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
    cbot.GoToBegin();
    while (!cbot.IsAtEnd())
    {
      const ImageType::IndexType index = cbot.GetIndex();
      const ImageType::PixelType pixel = cbot.Get();
      EXPECT_EQ(index, pixel) << "Values don't correspond to what was stored " << std::endl
                              << "Test failed at index " << index << " value is " << cbot.Get() << std::endl;
      EXPECT_TRUE(region.IsInside(index)) << "Iterator in region test failed" << std::endl
                                          << index << " is outside the region " << region << std::endl;
      std::cout << index << std::endl;
      ++cbot;
    }
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Verification of the Const Iterator in a subregion of the image
    std::cout << "Verifying Const Iterator in a Region smaller than the whole image... " << std::endl;
    const ImageType::RegionType region{ start, size };
    RandomConstIteratorType     cbot(myImage, region);
    cbot.SetNumberOfSamples(numberOfSamples);
    cbot.GoToBegin();
    while (!cbot.IsAtEnd())
    {
      const ImageType::IndexType index = cbot.GetIndex();
      const ImageType::PixelType pixel = cbot.Get();
      EXPECT_EQ(index, pixel) << "Values don't correspond to what was stored " << std::endl
                              << "Test failed at index " << index << " value is " << cbot.Get() << std::endl;
      EXPECT_TRUE(region.IsInside(index)) << "Iterator in region test failed" << std::endl
                                          << index << " is outside the region " << region << std::endl;
      std::cout << index << std::endl;
      ++cbot;
    }
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Verifying iterator works with the priority image
    std::cout << "Verifying Iterator with respect to priority image... " << std::endl;

    RandomIteratorType cbot(myImage, priorityRegion);
    cbot.SetPriorityImage(priorityImage);
    cbot.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
    cbot.GoToBegin();
    unsigned int count = 0;
    while (!cbot.IsAtEnd() && count < numberOfPixelsSub)
    {
      const ImageType::IndexType index = cbot.GetIndex();
      EXPECT_TRUE(priorityRegion.IsInside(index)) << "Iterator in priority region test failed" << std::endl
                                                  << index << " is outside the region " << priorityRegion << std::endl;
      std::cout << index << std::endl;
      ++cbot;
      ++count;
    }
    std::cout << "   Done ! " << std::endl;
  }
  {
    std::cout << "Verifying const Iterator with respect to priority image... " << std::endl;

    RandomConstIteratorType cbot(myImage, priorityRegion);
    cbot.SetPriorityImage(priorityImage);
    cbot.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
    cbot.GoToBegin();
    unsigned int count = 0;
    while (!cbot.IsAtEnd() && count < numberOfPixelsSub)
    {
      const ImageType::IndexType index = cbot.GetIndex();
      EXPECT_TRUE(priorityRegion.IsInside(index)) << "Iterator in priority region test failed" << std::endl
                                                  << index << " is outside the region " << priorityRegion << std::endl;
      std::cout << index << std::endl;
      ++cbot;
      ++count;
    }
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Exercise assignment operator
    std::cout << "Exercising assignment operator... " << std::endl;
    RandomConstIteratorType iteratorAssignment = RandomConstIteratorType(myImage, myImage->GetLargestPossibleRegion());
    iteratorAssignment.SetNumberOfSamples(myImage->GetLargestPossibleRegion().GetNumberOfPixels());
    iteratorAssignment.GoToBegin();
    size_t counter = 0;
    while (!iteratorAssignment.IsAtEnd())
    {
      ++counter;
      ++iteratorAssignment;
    }
    EXPECT_EQ(counter, myImage->GetLargestPossibleRegion().GetNumberOfPixels())
      << "Assignment operator failed for the number of requested samples" << std::endl;
    std::cout << "Finished exercising assignment operator!" << std::endl;
  }
}

TEST(ImageRandomNonRepeatingIteratorWithIndex, SupportsRandomWalks)
{
  /*
  This tests the classes ImageRandomNonRepeatingIteratorWithIndex and
  ImageRandomNonRepeatingConstIteratorWithIndex.  This was contributed
  by Rupert Brooks, McGill Centre for Intelligent
  Machines, Montreal, Canada.  It is heavily based on the
  ImageRandomIterator test program.
*/

  constexpr unsigned int ImageDimension{ 2 };
  using PixelType = itk::Index<ImageDimension>;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  using RandomConstIteratorType = itk::ImageRandomNonRepeatingConstIteratorWithIndex<ImageType>;
  constexpr unsigned long     N{ 10 };
  constexpr int               Seed{ 42 };
  auto                        size = ImageType::SizeType::Filled(N);
  const ImageType::RegionType region{ size };
  auto                        myImage = ImageType::New();
  myImage->SetRegions(region);
  myImage->Allocate();
  using WalkType = std::vector<ImageType::IndexType>;
  using WalkIteratorType = WalkType::iterator;
  WalkType firstWalk(N);
  {
    RandomConstIteratorType firstIt(myImage, region);
    firstIt.ReinitializeSeed(Seed);
    firstIt.SetNumberOfSamples(region.GetNumberOfPixels());
    for (firstIt.GoToBegin(); !firstIt.IsAtEnd(); ++firstIt)
    {
      firstWalk.push_back(firstIt.GetIndex());
    }
  }
  WalkType secondWalk(N);
  {
    RandomConstIteratorType secondIt(myImage, region);
    secondIt.ReinitializeSeed(Seed);
    secondIt.SetNumberOfSamples(region.GetNumberOfPixels());
    for (secondIt.GoToBegin(); !secondIt.IsAtEnd(); ++secondIt)
    {
      secondWalk.push_back(secondIt.GetIndex());
    }
  }
  const std::pair<WalkIteratorType, WalkIteratorType> mismatchTest =
    std::mismatch(firstWalk.begin(), firstWalk.end(), secondWalk.begin());
  const bool at_end = mismatchTest.first == firstWalk.end();
  EXPECT_TRUE(at_end) << "Two iterations with the same seed do not"
                      << " walk over the same pixels" << std::endl
                      << "First mismatch found after " << mismatchTest.first - firstWalk.begin() << " iterations."
                      << std::endl
                      << "First walk index  : " << *(mismatchTest.first) << std::endl
                      << "Second walk index : " << *(mismatchTest.second) << std::endl;
}
