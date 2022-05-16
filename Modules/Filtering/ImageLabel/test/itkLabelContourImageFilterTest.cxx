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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkSimpleFilterWatcher.h"
#include "itkLabelContourImageFilter.h"
#include "itkTestingMacros.h"

int
itkLabelContourImageFilterTest(int argc, char * argv[])
{
  if (argc != 5)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " intputImage outputImage fullyConnected backgroundValue" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using FilterType = itk::LabelContourImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();

  // Test default values
  ITK_TEST_EXPECT_TRUE(!filter->GetFullyConnected());

  ITK_TEST_EXPECT_TRUE(filter->GetBackgroundValue() ==
                       itk::NumericTraits<FilterType::OutputImagePixelType>::NonpositiveMin());

  // Tests for raising code coverage
  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, LabelContourImageFilter, InPlaceImageFilter);
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());
  ITK_TEST_SET_GET_BOOLEAN(filter, FullyConnected, std::stoi(argv[3]));

  filter->SetInput(reader->GetOutput());

  auto backgroundValue = std::stoi(argv[4]);
  filter->SetBackgroundValue(backgroundValue);
  ITK_TEST_SET_GET_VALUE(backgroundValue, filter->GetBackgroundValue());


  itk::SimpleFilterWatcher watcher(filter, "filter");

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
