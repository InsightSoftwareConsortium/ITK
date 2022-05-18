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

#include <iostream>
#include "itkZeroCrossingBasedEdgeDetectionImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkZeroCrossingBasedEdgeDetectionImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " varianceValue maximumErrorValue backgroundValue foregroundValue" << std::endl;
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<float, 2>;

  // Set up filter
  using FilterType = itk::ZeroCrossingBasedEdgeDetectionImageFilter<ImageType, ImageType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ZeroCrossingBasedEdgeDetectionImageFilter, ImageToImageFilter);


  itk::SimpleFilterWatcher watcher(filter);

  float varianceValue = std::stod(argv[1]);
  filter->SetVariance(varianceValue);
  for (auto i : filter->GetVariance())
  {
    if (i != varianceValue)
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in itk::ZeroCrossingBasedEdgeDetectionImageFilter::GetVariance" << std::endl;
      std::cerr << "Expected: " << varianceValue << ", but got: " << i << std::endl;
      return EXIT_FAILURE;
    }
  }

  FilterType::ArrayType variance;
  variance.Fill(varianceValue);
  filter->SetVariance(variance);
  ITK_TEST_SET_GET_VALUE(variance, filter->GetVariance());

  float maximumErrorValue = std::stod(argv[2]);
  filter->SetMaximumError(maximumErrorValue);
  for (auto i : filter->GetMaximumError())
  {
    if (i != maximumErrorValue)
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in itk::ZeroCrossingBasedEdgeDetectionImageFilter::GetMaximumError" << std::endl;
      std::cerr << "Expected: " << maximumErrorValue << ", but got: " << i << std::endl;
      return EXIT_FAILURE;
    }
  }

  FilterType::ArrayType maximumError;
  maximumError.Fill(maximumErrorValue);
  filter->SetMaximumError(maximumError);
  ITK_TEST_SET_GET_VALUE(maximumError, filter->GetMaximumError());

  auto backgroundValue = static_cast<typename FilterType::OutputImagePixelType>(std::stod(argv[3]));
  filter->SetBackgroundValue(backgroundValue);
  ITK_TEST_SET_GET_VALUE(backgroundValue, filter->GetBackgroundValue());

  auto foregroundValue = static_cast<typename FilterType::OutputImagePixelType>(std::stod(argv[4]));
  filter->SetForegroundValue(foregroundValue);
  ITK_TEST_SET_GET_VALUE(foregroundValue, filter->GetForegroundValue());

  // Run Test
  itk::Size<2> sz;
  sz[0] = 100;
  sz[1] = 100;
  itk::NullImageToImageFilterDriver<ImageType, ImageType> test1;
  test1.SetImageSize(sz);
  test1.SetFilter(filter);

  ITK_TRY_EXPECT_NO_EXCEPTION(test1.Execute());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
