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

#include "itkKappaSigmaThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


int
itkKappaSigmaThresholdImageFilterTest(int argc, char * argv[])
{
  if (argc != 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " outputImageFile"
              << " maskValue"
              << " sigmaFactor"
              << " numberOfIterations"
              << " expectedThreshold" << std::endl;
    return EXIT_FAILURE;
  }

  using InputPixelType = unsigned char;
  using MaskPixelType = unsigned char;
  using OutputPixelType = unsigned char;

  constexpr unsigned int Dimension = 2;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using MaskImageType = itk::Image<MaskPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  using FilterType = itk::KappaSigmaThresholdImageFilter<InputImageType, MaskImageType, OutputImageType>;
  FilterType::Pointer filter = FilterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, KappaSigmaThresholdImageFilter, ImageToImageFilter);


  auto insideValue = static_cast<FilterType::OutputPixelType>(255);
  filter->SetInsideValue(insideValue);
  ITK_TEST_SET_GET_VALUE(insideValue, filter->GetInsideValue());

  auto outsideValue = static_cast<FilterType::OutputPixelType>(0);
  filter->SetOutsideValue(outsideValue);
  ITK_TEST_SET_GET_VALUE(outsideValue, filter->GetOutsideValue());

  auto maskValue = static_cast<FilterType::MaskPixelType>(std::stod(argv[3]));
  filter->SetMaskValue(maskValue);
  ITK_TEST_SET_GET_VALUE(maskValue, filter->GetMaskValue());

  double sigmaFactor = std::stod(argv[4]);
  filter->SetSigmaFactor(sigmaFactor);
  ITK_TEST_SET_GET_VALUE(sigmaFactor, filter->GetSigmaFactor());

  auto numberOfIterations = static_cast<unsigned int>(std::stoi(argv[5]));
  filter->SetNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, filter->GetNumberOfIterations());


  filter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Regression test: compare computed threshold
  FilterType::InputPixelType expectedThreshold = std::stod(argv[6]);
  FilterType::InputPixelType resultThreshold = filter->GetThreshold();
  if (itk::Math::NotAlmostEquals(expectedThreshold, resultThreshold))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetThreshold()" << std::endl;
    std::cerr << "Expected: " << itk::NumericTraits<FilterType::InputPixelType>::PrintType(expectedThreshold)
              << ", but got: " << itk::NumericTraits<FilterType::InputPixelType>::PrintType(resultThreshold)
              << std::endl;
    return EXIT_FAILURE;
  }

  // Write output image
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
