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
#include <iostream>

#include "itkConstantBoundaryCondition.h"
#include "itkFFTDiscreteGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

int
itkFFTDiscreteGaussianImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv)
              << " inputFilename sigma kernelWidth outputFilename filterDimensionality" << std::endl;
    return EXIT_FAILURE;
  }

  using ScalarPixelType = float;
  const size_t ImageDimension = 2;
  using ImageType = itk::Image<ScalarPixelType, ImageDimension>;

  double       sigma = std::stod(argv[2]);
  unsigned int kernelWidth = std::stoi(argv[3]);
  unsigned int filterDimensionality = (argc < 6 ? ImageDimension : std::stoi(argv[5]));

  typename ImageType::Pointer inputImage = itk::ReadImage<ImageType>(argv[1]);

  using FilterType = itk::FFTDiscreteGaussianImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, FFTDiscreteGaussianImageFilter, DiscreteGaussianImageFilter);

  // Test setting inputs

  filter->SetInput(inputImage);

  filter->SetVariance(sigma);
  for (auto & param : filter->GetVariance())
  {
    ITK_TEST_EXPECT_EQUAL(sigma, param);
  }

  filter->SetUseImageSpacingOff();
  for (auto & param : filter->GetImageSpacingVarianceArray())
  {
    ITK_TEST_EXPECT_EQUAL(sigma, param);
  }

  using SpacingType = typename ImageType::SpacingType;
  SpacingType spacing;
  spacing[0] = 0.5;
  spacing[1] = 0.25;
  inputImage->SetSpacing(spacing);
  filter->SetUseImageSpacingOn();
  for (size_t dim = 0; dim < ImageDimension; ++dim)
  {
    ITK_TEST_EXPECT_EQUAL(sigma, filter->GetVariance()[dim])
    ITK_TEST_EXPECT_EQUAL(sigma / (spacing[dim] * spacing[dim]), filter->GetImageSpacingVarianceArray()[dim]);
  }

  spacing[0] = 1.0;
  spacing[1] = 1.0;
  inputImage->SetSpacing(spacing);

  const float kernelError = 0.05;
  filter->SetMaximumError(kernelError);
  for (size_t dim = 0; dim < ImageDimension; ++dim)
  {
    ITK_TEST_SET_GET_VALUE(kernelError, filter->GetMaximumError()[dim]);
  }

  filter->SetMaximumKernelWidth(kernelWidth);
  ITK_TEST_SET_GET_VALUE(kernelWidth, filter->GetMaximumKernelWidth());

  filter->SetFilterDimensionality(filterDimensionality);
  ITK_TEST_SET_GET_VALUE(filterDimensionality, filter->GetFilterDimensionality());

  itk::ZeroFluxNeumannBoundaryCondition<ImageType> zfnBoundaryCondition;
  filter->SetRealBoundaryCondition(&zfnBoundaryCondition);
  ITK_TEST_SET_GET_VALUE(&zfnBoundaryCondition, filter->GetRealBoundaryCondition());

  // Test that attempting to set unused interface parameters throws warnings
  // Ignore values from "Get" methods as these are unused

  itk::ConstantBoundaryCondition<ImageType> constantBoundaryCondition;
  filter->SetInputBoundaryCondition(&constantBoundaryCondition);

  // Run convolution

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  itk::WriteImage(filter->GetOutput(), argv[4], true);

  return EXIT_SUCCESS;
}
