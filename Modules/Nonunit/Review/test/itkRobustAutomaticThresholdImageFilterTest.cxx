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
#include "itkRobustAutomaticThresholdImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

int
itkRobustAutomaticThresholdImageFilterTest(int argc, char * argv[])
{

  if (argc != 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImage outputImage pow insideValue outsideValue expectedThreshold" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned short;
  using ImageType = itk::Image<PixelType, Dimension>;

  using RealPixelType = float;
  using RealImageType = itk::Image<RealPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using GradientType = itk::GradientMagnitudeRecursiveGaussianImageFilter<ImageType, RealImageType>;
  auto gradient = GradientType::New();
  gradient->SetInput(reader->GetOutput());
  gradient->SetSigma(10);

  ITK_TRY_EXPECT_NO_EXCEPTION(gradient->Update());


  using FilterType = itk::RobustAutomaticThresholdImageFilter<ImageType, RealImageType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, RobustAutomaticThresholdImageFilter, ImageToImageFilter);

  itk::SimpleFilterWatcher watcher(filter, "RobustAutomaticThresholdImageFilter");


  filter->SetGradientImage(gradient->GetOutput());

  double pow = std::stod(argv[3]);
  filter->SetPow(pow);
  ITK_TEST_SET_GET_VALUE(pow, filter->GetPow());

  auto insideValue = static_cast<FilterType::InputPixelType>(std::stod(argv[4]));
  filter->SetInsideValue(insideValue);
  ITK_TEST_SET_GET_VALUE(insideValue, filter->GetInsideValue());

  auto outsideValue = static_cast<FilterType::InputPixelType>(std::stod(argv[5]));
  filter->SetOutsideValue(outsideValue);
  ITK_TEST_SET_GET_VALUE(outsideValue, filter->GetOutsideValue());


  filter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Regression test
  auto                       expectedThreshold = static_cast<FilterType::InputPixelType>(std::stod(argv[6]));
  FilterType::InputPixelType computedThreshold = filter->GetThreshold();
  if (itk::Math::NotAlmostEquals(expectedThreshold, computedThreshold))
  {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Error in GetThreshold()" << std::endl;
    std::cout << "Expected: "
              << static_cast<itk::NumericTraits<FilterType::InputPixelType>::PrintType>(expectedThreshold)
              << ", but got: "
              << static_cast<itk::NumericTraits<FilterType::InputPixelType>::PrintType>(computedThreshold) << std::endl;
    return EXIT_FAILURE;
  }


  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);


  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
