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

int
itkImageIteratorsForwardBackwardTest(int, char *[])
{

  std::cout << "Creating an image" << '\n';
  using ImageType = itk::Image<unsigned short, 3>;

  auto myImage = ImageType::New();

  ImageType::SizeType size;

  size[0] = 4;
  size[1] = 4;
  size[2] = 4;

  const ImageType::IndexType start{};

  const ImageType::RegionType region{ start, size };

  myImage->SetRegions(region);
  myImage->Allocate();

  using IteratorType = itk::ImageRegionIteratorWithIndex<ImageType>;

  using ConstIteratorType = itk::ImageRegionConstIteratorWithIndex<ImageType>;

  IteratorType it(myImage, region);

  ImageType::PixelType value;

  value = ImageType::PixelType{};

  // Store information on the Image
  std::cout << "Storing data on the image ... " << '\n';

  while (!it.IsAtEnd())
  {
    value++;
    it.Set(value);
    ++it;
  }


  // Verification
  IteratorType ot(myImage, region);
  std::cout << "Verifying the data forwards... ";

  value = ImageType::PixelType{};

  while (!ot.IsAtEnd())
  {
    value++;

    if (ot.Get() != value)
    {
      std::cerr << "Error in forward pass" << '\n';
      std::cerr << "Values don't correspond to what was stored " << '\n';
      std::cerr << "Test failed at index ";
      std::cerr << ot.GetIndex() << '\n';
      std::cerr << "Value stored is = " << ot.Get() << '\n';
      std::cerr << "Value should be = " << value << '\n';
      return EXIT_FAILURE;
    }
    ++ot;
  }

  std::cout << "      PASSED !" << '\n';

  // Verification
  std::cout << "Verifying the data backwards... ";

  ot.GoToReverseBegin();
  --ot;
  --value;
  while (!ot.IsAtReverseEnd())
  {

    if (ot.Get() != value)
    {
      std::cerr << "Error in backwards pass" << '\n';
      std::cerr << "Values don't correspond to what was stored " << '\n';
      std::cerr << "Test failed at index ";
      std::cerr << ot.GetIndex() << '\n';
      std::cerr << "Value stored is = " << ot.Get() << '\n';
      std::cerr << "Value should be = " << value << '\n';
      return EXIT_FAILURE;
    }
    value--;
    --ot;
  }

  std::cout << "      PASSED !" << '\n';

  // Verification
  ConstIteratorType cot(myImage, region);
  std::cout << "Const Iterator: Verifying the data forwards... ";

  value = ImageType::PixelType{};

  while (!cot.IsAtEnd())
  {
    value++;

    if (cot.Get() != value)
    {
      std::cerr << "Error in forward pass" << '\n';
      std::cerr << "Values don't correspond to what was stored " << '\n';
      std::cerr << "Test failed at index ";
      std::cerr << cot.GetIndex() << '\n';
      std::cerr << "Value stored is = " << cot.Get() << '\n';
      std::cerr << "Value should be = " << value << '\n';
      return EXIT_FAILURE;
    }
    ++cot;
  }

  std::cout << "      PASSED !" << '\n';

  // Verification
  std::cout << "Const Iterator : Verifying the data backwards... ";

  cot.GoToReverseBegin();
  --cot;
  --value;
  while (!cot.IsAtReverseEnd())
  {

    if (cot.Get() != value)
    {
      std::cerr << "Error in backwards pass" << '\n';
      std::cerr << "Values don't correspond to what was stored " << '\n';
      std::cerr << "Test failed at index ";
      std::cerr << cot.GetIndex() << '\n';
      std::cerr << "Value stored is = " << cot.Get() << '\n';
      std::cerr << "Value should be = " << value << '\n';
      return EXIT_FAILURE;
    }
    value--;
    --cot;
  }

  std::cout << "      PASSED !" << '\n';

  std::cout << '\n' << "Test passed" << '\n';

  return EXIT_SUCCESS;
}
