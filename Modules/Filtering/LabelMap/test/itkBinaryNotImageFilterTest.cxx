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
#include "itkBinaryNotImageFilter.h"
#include "itkTestingMacros.h"

int
itkBinaryNotImageFilterTest(int argc, char * argv[])
{
  if (argc != 6)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputFileName1 inputFileName2 outputFileName backgroundValue foregroundValue" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader1 = ReaderType::New();
  reader1->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader1->Update());


  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader2->Update());


  using FilterType = itk::BinaryNotImageFilter<ImageType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, BinaryNotImageFilter, UnaryFunctorImageFilter);


  auto foregroundValue = static_cast<PixelType>(std::stoi(argv[4]));
  filter->SetForegroundValue(foregroundValue);
  ITK_TEST_SET_GET_VALUE(foregroundValue, filter->GetForegroundValue());


  auto backgroundValue = static_cast<PixelType>(std::stoi(argv[5]));
  filter->SetBackgroundValue(backgroundValue);
  ITK_TEST_SET_GET_VALUE(backgroundValue, filter->GetBackgroundValue());


  filter->SetInput(reader1->GetOutput());
  filter->SetInput(reader2->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[3]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
