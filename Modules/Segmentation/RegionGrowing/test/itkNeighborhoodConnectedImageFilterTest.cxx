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

#include <fstream>
#include "itkNeighborhoodConnectedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkNeighborhoodConnectedImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage OutputImage seed_x seed_y\n";
    return EXIT_FAILURE;
  }

  using PixelType = unsigned char;
  using myImage = itk::Image<PixelType, 2>;
  itk::ImageFileReader<myImage>::Pointer input = itk::ImageFileReader<myImage>::New();
  input->SetFileName(argv[1]);

  // Create a filter
  using FilterType = itk::NeighborhoodConnectedImageFilter<myImage, myImage>;

  auto                     filter = FilterType::New();
  itk::SimpleFilterWatcher watcher(filter);

  filter->SetInput(input->GetOutput());

  FilterType::IndexType seed;

  seed[0] = std::stoi(argv[3]);
  seed[1] = std::stoi(argv[4]);
  filter->SetSeed(seed);

  filter->SetLower(0);
  filter->SetUpper(210);
  using SizeType = FilterType::InputImageSizeType;
  SizeType radius;
  radius.Fill(5);

  filter->SetRadius(radius);
  filter->SetReplaceValue(255);

  // Test GetMacros
  PixelType lower = filter->GetLower();
  std::cout << "filter->GetLower(): " << itk::NumericTraits<PixelType>::PrintType(lower) << std::endl;
  PixelType upper = filter->GetUpper();
  std::cout << "filter->GetUpper(): " << itk::NumericTraits<PixelType>::PrintType(upper) << std::endl;
  PixelType replaceValue = filter->GetReplaceValue();
  std::cout << "filter->GetReplaceValue(): " << itk::NumericTraits<PixelType>::PrintType(replaceValue) << std::endl;

  // Test GetConstReferenceMacro
  const SizeType & radius2 = filter->GetRadius();
  std::cout << "filter->GetRadius(): " << radius2 << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(input->Update());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  return EXIT_SUCCESS;
}
