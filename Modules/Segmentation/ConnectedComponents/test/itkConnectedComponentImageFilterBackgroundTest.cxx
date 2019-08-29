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

#include "itkConnectedComponentImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkConnectedComponentImageFilterBackgroundTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <background value>\n";
    return EXIT_FAILURE;
  }

  using PixelType = int;
  constexpr unsigned int Dimension = 2;
  using ImageType = itk::Image<PixelType, Dimension>;

  auto background = static_cast<PixelType>(std::stoi(argv[1]));

  // Create an image with an arbitrary background value and a number
  // of islands with pixel values above and below the background value
  ImageType::Pointer  image = ImageType::New();
  ImageType::SizeType size;
  size.Fill(512);
  image->SetRegions(size);
  image->Allocate();
  image->FillBuffer(0);

  // Set up islands
  ImageType::IndexType index1;
  index1[0] = 10;
  index1[1] = 11;
  image->SetPixel(index1, 1);

  ImageType::IndexType index2;
  index2[0] = 100;
  index2[1] = 101;
  image->SetPixel(index2, 2);

  // Instantiate and run the filter
  using FilterType = itk::ConnectedComponentImageFilter<ImageType, ImageType>;
  FilterType::Pointer filter = FilterType::New();
  filter->SetBackgroundValue(background);
  filter->SetInput(image);
  itk::SimpleFilterWatcher watcher(filter);

  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & excep)
  {
    std::cerr << "exception caught:" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
  }

  if (filter->GetObjectCount() != 2)
  {
    std::cerr << "Expected 2 objects, got " << filter->GetObjectCount() << " instead.\n";
  }

  ImageType * output = filter->GetOutput();

  using IteratorType = itk::ImageRegionConstIteratorWithIndex<ImageType>;
  IteratorType iterator(output, output->GetLargestPossibleRegion());
  while (!iterator.IsAtEnd())
  {
    PixelType            value = iterator.Get();
    ImageType::IndexType index = iterator.GetIndex();
    if (index == index1 || index == index2)
    {
      // Check that objects don't have the background value
      if (value == background)
      {
        std::cerr << "Pixel at index " << index << " has background value "
                  << "but should have an object label instead.\n";
        return EXIT_FAILURE;
      }
    }
    else
    {
      if (value != background)
      {
        std::cerr << "Pixel at index " << index << " has value " << value
                  << " but should have bacground value: " << background << "\n";
        return EXIT_FAILURE;
      }
    }
    ++iterator;
  }

  return EXIT_SUCCESS;
}
