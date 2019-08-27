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

#include "itkKittlerIllingworthThresholdImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkKittlerIllingworthMaskedThresholdImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImageFile maskImageFile outputImageFile";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  using InputPixelType = short;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, 2>;
  using OutputImageType = itk::Image<OutputPixelType, 2>;

  using FilterType = itk::KittlerIllingworthThresholdImageFilter<InputImageType, OutputImageType, OutputImageType>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using MaskReaderType = itk::ImageFileReader<OutputImageType>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();
  WriterType::Pointer writer = WriterType::New();

  MaskReaderType::Pointer maskreader = MaskReaderType::New();

  itk::SimpleFilterWatcher watcher(filter);

  filter->SetInsideValue(255);
  ITK_TEST_SET_GET_VALUE(255, filter->GetInsideValue());

  filter->SetOutsideValue(0);
  ITK_TEST_SET_GET_VALUE(0, filter->GetOutsideValue());

  reader->SetFileName(argv[1]);
  maskreader->SetFileName(argv[2]);
  filter->SetInput(reader->GetOutput());
  filter->SetMaskImage(maskreader->GetOutput());
  writer->SetInput(filter->GetOutput());

  filter->Update();
  std::cout << "Computed Threshold is: "
            << itk::NumericTraits<FilterType::InputPixelType>::PrintType(filter->GetThreshold()) << std::endl;
  writer->SetFileName(argv[3]);
  writer->Update();

  return EXIT_SUCCESS;
}
