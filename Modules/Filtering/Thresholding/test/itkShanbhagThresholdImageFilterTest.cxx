/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkShanbhagThresholdImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkShanbhagThresholdImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImageFile outputImageFile";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  using InputPixelType = short;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, 2>;
  using OutputImageType = itk::Image<OutputPixelType, 2>;

  using FilterType = itk::ShanbhagThresholdImageFilter<InputImageType, OutputImageType>;

  using ReaderType = itk::ImageFileReader<InputImageType>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();
  WriterType::Pointer writer = WriterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  filter->SetInsideValue(255);
  ITK_TEST_SET_GET_VALUE(255, filter->GetInsideValue());

  filter->SetOutsideValue(0);
  ITK_TEST_SET_GET_VALUE(0, filter->GetOutsideValue());

  reader->SetFileName(argv[1]);
  filter->SetInput(reader->GetOutput());
  // filter->SetNumberOfHistogramBins (std::stoi(argv[3]));
  writer->SetInput(filter->GetOutput());

  filter->Update();
  std::cout << "Computed Threshold is: "
            << itk::NumericTraits<FilterType::InputPixelType>::PrintType(filter->GetThreshold()) << std::endl;
  writer->SetFileName(argv[2]);
  writer->Update();

  return EXIT_SUCCESS;
}
