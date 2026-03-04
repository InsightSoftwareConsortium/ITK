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
/*
  This tests the classes ImageRandomNonRepeatingIteratorWithIndex and
  ImageRandomNonRepeatingConstIteratorWithIndex.  This was contributed
  by Rupert Brooks, McGill Centre for Intelligent
  Machines, Montreal, Canada.  It is heavily based on the
  ImageRandomIterator test program.
*/

#include <iostream>
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRandomNonRepeatingIteratorWithIndex.h"
int
itkImageRandomNonRepeatingIteratorWithIndexTest(int, char *[])
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

  auto                          myImage = ImageType::New();
  const ImageType::ConstPointer myConstImage = myImage;
  constexpr ImageType::SizeType size0{ 50, 50, 50 };
  constexpr auto                numberOfPixelsSize0 = size0.CalculateProductOfElements();
  constexpr unsigned long       numberOfSamples{ 10 };
  const ImageType::RegionType   region0{ size0 };
  myImage->SetRegions(region0);
  myImage->Allocate();
  // Make the priority image
  constexpr PriorityImageType::SizeType  prioritySize{ 50, 50, 50 };
  constexpr ImageType::IndexType         start{ 10, 12, 14 };
  constexpr ImageType::SizeType          size{ 11, 12, 13 };
  constexpr PriorityImageType::IndexType substart{ 15, 16, 17 };
  constexpr PriorityImageType::SizeType  subsize{ 3, 4, 5 };
  constexpr auto                         numberOfPixelsSub = subsize.CalculateProductOfElements();

  auto priorityImage = PriorityImageType::New();
  {
    const PriorityImageType::RegionType priorityRegion{ prioritySize };
    priorityImage->SetRegions(priorityRegion);
    priorityImage->Allocate();
    // we will make most of this image of ones, with a small region of
    // zeros.  Then pixels from the zero region should be selected
    // preferentially.
    std::cout << "Building Priority image" << std::endl;
    priorityImage->FillBuffer(1);
  }

  const PriorityImageType::RegionType subregion{ substart, subsize };
  {
    PriorityIteratorType subit(priorityImage, subregion);
    subit.GoToBegin();
    while (!subit.IsAtEnd())
    {
      subit.Set(0);
      ++subit;
    }
  }

  {
    //********
    std::cout << "Filling image with indices" << std::endl;

    RandomIteratorType it(myImage, region0);
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
    IteratorType ot(myImage, region0);
    ot.GoToBegin();
    // if it repeated it is going to have missed a few.
    std::cout << "Verifying iterators... ";
    ImageType::IndexType indexMatch;
    while (!ot.IsAtEnd())
    {
      indexMatch = ot.GetIndex();
      if (ot.Get() != indexMatch)
      {
        std::cerr << "Values don't correspond to what was stored " << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << indexMatch << std::endl;
        return EXIT_FAILURE;
      }
      // std::cout <<".";
      // std::cout << indexMatch << std::endl;
      ++ot;
    }
    // Now we have walked through all the pixels of low priority,
    // the next one should be outside the region.
    if (subregion.IsInside(indexMatch))
    {
      std::cerr << "Iterator in priority region test failed" << std::endl;
      std::cerr << indexMatch << " is outside the region (should be in)" << region0 << std::endl;
      return EXIT_FAILURE;
    }
    std::cout << std::endl << "   Done ! " << std::endl;
  }
  {
    // Verification
    RandomConstIteratorType cot(myConstImage, region0);
    cot.SetNumberOfSamples(numberOfSamples);
    cot.GoToBegin();

    std::cout << "Verifying const iterator... ";
    std::cout << "Random walk of the Iterator over the image " << std::endl;
    ImageType::IndexType indexConstMatch;
    while (!cot.IsAtEnd())
    {
      indexConstMatch = cot.GetIndex();
      if (cot.Get() != indexConstMatch)
      {
        std::cerr << "Values don't correspond to what was stored " << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << indexConstMatch << " value is " << cot.Get() << std::endl;
        return EXIT_FAILURE;
      }
      std::cout << indexConstMatch << std::endl;
      ++cot;
    }
    // Now we have walked through all the pixels of low priority,
    // the next one should be outside the region.
    if (subregion.IsInside(indexConstMatch))
    {
      std::cerr << "Iterator in priority region test failed" << std::endl;
      std::cerr << indexConstMatch << " is outside the region (should be in)" << region0 << std::endl;
      return EXIT_FAILURE;
    }
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Verification
    std::cout << "Verifying iterator in reverse direction... " << std::endl;
    std::cout << "Should be a random walk too (a different one)" << std::endl;
    RandomIteratorType ior(myImage, region0);
    ior.SetNumberOfSamples(numberOfSamples);
    ior.GoToEnd();
    --ior;
    ImageType::IndexType indexReverse;
    while (!ior.IsAtBegin())
    {
      indexReverse = ior.GetIndex();
      if (ior.Get() != indexReverse)
      {
        std::cerr << "Values don't correspond to what was stored " << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << indexReverse << " value is " << ior.Get() << std::endl;
        return EXIT_FAILURE;
      }
      std::cout << indexReverse << std::endl;
      --ior;
    }
    std::cout << indexReverse << std::endl; // print the value at the beginning index
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Verification
    std::cout << "Verifying const iterator in reverse direction... ";
    RandomConstIteratorType cor(myImage, region0);
    cor.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
    cor.GoToEnd();
    --cor; // start at the end position
    ImageType::IndexType indexConstReverse;
    while (!cor.IsAtBegin())
    {
      indexConstReverse = cor.GetIndex();
      if (cor.Get() != indexConstReverse)
      {
        std::cerr << "Values don't correspond to what was stored " << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << indexConstReverse << " value is " << cor.Get() << std::endl;
        return EXIT_FAILURE;
      }
      std::cout << indexConstReverse << std::endl;
      --cor;
    }
    std::cout << indexConstReverse << std::endl; // print the value at the beginning index
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Verification
    std::cout << "Verifying const iterator in both directions... ";
    RandomConstIteratorType dor(myImage, region0);
    dor.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
    dor.GoToEnd();
    --dor; // start at the last valid pixel position
    ImageType::IndexType indexBiDirectional;
    for (unsigned int counter = 0; !dor.IsAtEnd(); ++counter)
    {
      indexBiDirectional = dor.GetIndex();
      if (dor.Get() != indexBiDirectional)
      {
        std::cerr << "Values don't correspond to what was stored " << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << indexBiDirectional << " value is " << dor.Get() << std::endl;
        return EXIT_FAILURE;
      }
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
      if (index != pixel)
      {
        std::cerr << "Iterator in region test failed" << std::endl;
        std::cerr << pixel << " should be" << index << std::endl;
        return EXIT_FAILURE;
      }
      if (!region.IsInside(index))
      {
        std::cerr << "Iterator in region test failed" << std::endl;
        std::cerr << index << " is outside the region " << region << std::endl;
        return EXIT_FAILURE;
      }
      std::cout << index << std::endl;
      ++cbot;
    }
    {
      // Now we have walked through all the pixels of low priority,
      // the next one should be outside the region.
      ImageType::IndexType indexOnePast = cbot.GetIndex();
      if (subregion.IsInside(indexOnePast))
      {
        std::cerr << "Iterator in priority region test failed" << std::endl;
        std::cerr << indexOnePast << " is outside the region (should be in)" << region0 << "\n"
                  << __FILE__ << ":" << __LINE__ << std::endl;
        return EXIT_FAILURE;
      }
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
      if (index != pixel)
      {
        std::cerr << "Iterator in region test failed" << std::endl;
        std::cerr << pixel << " should be" << index << std::endl;
        return EXIT_FAILURE;
      }
      if (!region.IsInside(index))
      {
        std::cerr << "Iterator in region test failed" << std::endl;
        std::cerr << index << " is outside the region " << region << std::endl;
        return EXIT_FAILURE;
      }
      std::cout << index << std::endl;
      ++cbot;
    }
    {
      ImageType::IndexType indexConstOnePast = cbot.GetIndex();
      // Now we have walked through all the pixels of low priority,
      // the next one should be outside the region.
      if (subregion.IsInside(indexConstOnePast))
      {
        std::cerr << "Iterator in priority region test failed" << std::endl;
        std::cerr << indexConstOnePast << " is outside the region (should be in)" << region0 << "\n"
                  << __FILE__ << ":" << __LINE__ << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Verifying iterator works with the priority image
    std::cout << "Verifying Iterator with respect to priority image... " << std::endl;

    RandomIteratorType cbot(myImage, region0);
    cbot.SetPriorityImage(priorityImage);
    cbot.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
    cbot.GoToBegin();
    unsigned int count = 0;
    while (!cbot.IsAtEnd() && count < numberOfPixelsSub)
    {
      const ImageType::IndexType index = cbot.GetIndex();
      if (!subregion.IsInside(index))
      {
        std::cerr << "Iterator in priority region test failed" << std::endl;
        std::cerr << index << " is outside the region " << region0 << std::endl;
        return EXIT_FAILURE;
      }
      std::cout << index << std::endl;
      ++cbot;
      ++count;
    }
    std::cout << "   Done ! " << std::endl;
  }
  {
    std::cout << "Verifying const Iterator with respect to priority image... " << std::endl;

    RandomConstIteratorType cbot(myImage, region0);
    cbot.SetPriorityImage(priorityImage);
    cbot.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
    cbot.GoToBegin();
    unsigned int count = 0;
    while (!cbot.IsAtEnd() && count < numberOfPixelsSub)
    {
      const ImageType::IndexType index = cbot.GetIndex();
      if (!subregion.IsInside(index))
      {
        std::cerr << "Iterator in priority region test failed" << std::endl;
        std::cerr << index << " is outside the region " << region0 << std::endl;
        return EXIT_FAILURE;
      }
      std::cout << index << std::endl;
      ++cbot;
      ++count;
    }
    std::cout << "   Done ! " << std::endl;
  }
  {
    // Exercise assignment operator
    std::cout << "Exercising assignment operator... " << std::endl;
    RandomConstIteratorType iteratorAssignment;
    iteratorAssignment = RandomConstIteratorType(myImage, myImage->GetLargestPossibleRegion());
    iteratorAssignment.SetNumberOfSamples(myImage->GetLargestPossibleRegion().GetNumberOfPixels());
    iteratorAssignment.GoToBegin();
    std::cout << "Finished exercising assignment operator!" << std::endl;
  }
  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
