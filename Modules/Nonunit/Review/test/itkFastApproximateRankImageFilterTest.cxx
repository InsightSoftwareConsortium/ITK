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
#include "itkFastApproximateRankImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkFastApproximateRankImageFilterTest(int argc, char * argv[])
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage BaselineImage radius" << std::endl;
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<unsigned char, 2>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto input = ReaderType::New();
  input->SetFileName(argv[1]);

  // Create a filter
  using FilterType = itk::FastApproximateRankImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, FastApproximateRankImageFilter, MiniPipelineSeparableImageFilter);


  itk::SimpleFilterWatcher filterWatch(filter);

  using RadiusType = FilterType::RadiusType;

  // Test default values
  RadiusType r1;
  r1.Fill(1);
  ITK_TEST_SET_GET_VALUE(r1, filter->GetRadius());

  auto rank = 0.5;
  ITK_TEST_SET_GET_VALUE(rank, filter->GetRank());

  // Set radius with a radius type
  RadiusType r5;
  r5.Fill(5);
  filter->SetRadius(r5);
  ITK_TEST_SET_GET_VALUE(r5, filter->GetRadius());

  // Set radius with an integer
  auto radius = 1;
  filter->SetRadius(radius);
  ITK_TEST_SET_GET_VALUE(r1, filter->GetRadius());

  rank = 0.25;
  filter->SetRank(rank);
  ITK_TEST_SET_GET_VALUE(rank, filter->GetRank());

  int r = std::stoi(argv[3]);
  filter->SetRadius(r);

  rank = 0.5;
  filter->SetRank(rank);

  filter->SetInput(input->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Generate test image
  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
