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
#include "itkGrayscaleMorphologicalClosingImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkSimpleFilterWatcher.h"
#include "itkFlatStructuringElement.h"
#include "itkTestingMacros.h"

int
itkGrayscaleMorphologicalClosingImageFilterTest2(int argc, char * argv[])
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if (argc < 8)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " InputImage radius BASIC HISTO ANCHOR VHGW SafeBorder" << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int const dim = 2;
  using ImageType = itk::Image<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Create a filter
  using SRType = itk::FlatStructuringElement<dim>;
  using FilterType = itk::GrayscaleMorphologicalClosingImageFilter<ImageType, ImageType, SRType>;
  auto filter = FilterType::New();
  filter->SetInput(reader->GetOutput());

  itk::SimpleFilterWatcher watcher(filter, "filter");

  using RadiusType = FilterType::RadiusType;

  // Test default values
  RadiusType r1;
  r1.Fill(1);
  ITK_TEST_SET_GET_VALUE(r1, filter->GetRadius());

  ITK_TEST_SET_GET_VALUE(FilterType::AlgorithmEnum::HISTO, filter->GetAlgorithm());

  ITK_TEST_SET_GET_VALUE(true, filter->GetSafeBorder());

  itk::SizeValueType radiusValue{ static_cast<itk::SizeValueType>(std::stoi(argv[2])) };
  filter->SetRadius(radiusValue);
  RadiusType radius{};
  radius.Fill(radiusValue);
  ITK_TEST_SET_GET_VALUE(radius, filter->GetRadius());

  auto safeBorder = static_cast<bool>(std::stoi(argv[7]));
  ITK_TEST_SET_GET_BOOLEAN(filter, SafeBorder, safeBorder);

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());

  filter->SetAlgorithm(FilterType::AlgorithmEnum::BASIC);
  writer->SetFileName(argv[3]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  filter->SetAlgorithm(FilterType::AlgorithmEnum::HISTO);
  writer->SetFileName(argv[4]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  filter->SetAlgorithm(FilterType::AlgorithmEnum::ANCHOR);
  writer->SetFileName(argv[5]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  filter->SetAlgorithm(FilterType::AlgorithmEnum::VHGW);
  writer->SetFileName(argv[6]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Generate test image
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[3]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
