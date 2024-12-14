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
#include "itkImageRandomConstIteratorWithOnlyIndex.h"

int
itkImageRandomConstIteratorWithOnlyIndexTest(int, char *[])
{
  std::cout << "Creating an image of indices" << '\n';

  constexpr unsigned int ImageDimension = 3;

  using PixelType = itk::Index<ImageDimension>;

  using ImageType = itk::Image<PixelType, ImageDimension>;

  auto                          myImage = ImageType::New();
  const ImageType::ConstPointer myConstImage = myImage;

  ImageType::SizeType size0;

  size0[0] = 100;
  size0[1] = 100;
  size0[2] = 100;

  const unsigned long numberOfSamples = 10;

  const ImageType::IndexType start0{};

  const ImageType::RegionType region0{ start0, size0 };

  myImage->SetRegions(region0);
  myImage->Allocate();

  using IteratorType = itk::ImageRegionIteratorWithIndex<ImageType>;
  using RandomConstIteratorType = itk::ImageRandomConstIteratorWithOnlyIndex<ImageType>;

  IteratorType it(myImage, region0);

  it.GoToBegin();
  ImageType::IndexType index0;

  // Fill an image with indices
  while (!it.IsAtEnd())
  {
    index0 = it.GetIndex();
    it.Set(index0);
    ++it;
  }

  // Sample the image
  RandomConstIteratorType cot(myConstImage, region0);
  cot.SetNumberOfSamples(numberOfSamples);
  cot.GoToBegin();

  std::cout << "Verifying const iterator... ";
  std::cout << "Random walk of the Iterator over the image " << '\n';

  while (!cot.IsAtEnd())
  {
    index0 = cot.GetIndex();
    it.SetIndex(index0);
    if (it.Get() != index0)
    {
      std::cerr << "Values don't correspond to what was stored " << '\n';
      std::cerr << "Test failed at index ";
      std::cerr << index0 << " value is " << it.Get() << '\n';
      return EXIT_FAILURE;
    }
    std::cout << index0 << '\n';
    ++cot;
  }
  std::cout << "   Done ! " << '\n';

  // Verification of reverse iteration
  std::cout << "Verifying const iterator in reverse direction... ";

  RandomConstIteratorType cor(myImage, region0);
  cor.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
  cor.GoToEnd();

  --cor; // start at the end position

  while (!cor.IsAtBegin())
  {
    index0 = cor.GetIndex();
    it.SetIndex(index0);
    if (it.Get() != index0)
    {
      std::cerr << "Values don't correspond to what was stored " << '\n';
      std::cerr << "Test failed at index ";
      std::cerr << index0 << " value is " << it.Get() << '\n';
      return EXIT_FAILURE;
    }
    std::cout << index0 << '\n';
    --cor;
  }
  std::cout << index0 << '\n'; // print the value at the beginning index
  std::cout << "   Done ! " << '\n';

  // Verification with ImageBase type and both directions of iteration
  std::cout << "Verifying const iterator with ImageBase type and in both directions... ";

  using ImageBaseType = itk::ImageBase<ImageDimension>;
  using ImageBaseRandomConstIteratorType = itk::ImageRandomConstIteratorWithOnlyIndex<ImageBaseType>;

  auto myImageBase = ImageBaseType::New();
  myImageBase->CopyInformation(myImage);
  ImageBaseRandomConstIteratorType dor(myImageBase, region0);
  dor.SetNumberOfSamples(numberOfSamples); // 0=x, 1=y, 2=z
  dor.GoToEnd();

  --dor; // start at the last valid pixel position

  for (unsigned int counter = 0; !dor.IsAtEnd(); ++counter)
  {
    index0 = dor.GetIndex();
    it.SetIndex(index0);
    if (it.Get() != index0)
    {
      std::cerr << "Values don't correspond to what was stored " << '\n';
      std::cerr << "Test failed at index " << index0 << " value is " << it.Get() << '\n';
      return EXIT_FAILURE;
    }
    std::cout << index0 << '\n';
    if (counter < 6)
    {
      --dor;
    }
    else
    {
      ++dor;
    }
  }
  std::cout << index0 << '\n'; // print the value at the beginning index
  std::cout << "   Done ! " << '\n';

  // Verification of the Const Iterator in a subregion of the image
  {
    std::cout << "Verifying Const Iterator in a Region smaller than the whole image... " << '\n';

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 12;
    start[2] = 14;

    ImageType::SizeType size;
    size[0] = 11;
    size[1] = 12;
    size[2] = 13;

    const ImageType::RegionType region{ start, size };

    RandomConstIteratorType cbot(myImage, region);

    cbot.SetNumberOfSamples(numberOfSamples);
    cbot.GoToBegin();

    while (!cbot.IsAtEnd())
    {
      const ImageType::IndexType index = cbot.GetIndex();
      it.SetIndex(index);
      const ImageType::PixelType pixel = it.Get();

      if (index != pixel)
      {
        std::cerr << "Iterator in region test failed" << '\n';
        std::cerr << pixel << " should be" << index << '\n';
        return EXIT_FAILURE;
      }
      if (!region.IsInside(index))
      {
        std::cerr << "Iterator in region test failed" << '\n';
        std::cerr << index << " is outside the region " << region << '\n';
        return EXIT_FAILURE;
      }
      std::cout << index << '\n';

      ++cbot;
    }

    std::cout << "   Done ! " << '\n';
  }

  std::cout << "Test passed" << '\n';

  return EXIT_SUCCESS;
}
