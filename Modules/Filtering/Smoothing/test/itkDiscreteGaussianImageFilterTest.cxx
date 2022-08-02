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
#include "itkDiscreteGaussianImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include "itkConstantBoundaryCondition.h"

/** Check basic image filter parameters and operations */

int
itkDiscreteGaussianImageFilterTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " useImageSpacing" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;

  using PixelType = float;

  using ImageType = itk::Image<PixelType, Dimension>;

  // Set up filter
  using FilterType = itk::DiscreteGaussianImageFilter<ImageType, ImageType>;
  using ArrayType = FilterType::ArrayType;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, DiscreteGaussianImageFilter, ImageToImageFilter);


  itk::SimpleFilterWatcher watcher(filter);

  // Test other set/get functions
  ArrayType array;
  array[0] = 0.05;
  array[1] = 0.06;
  array[2] = 0.07;
  filter->SetMaximumError(array);

  array.Fill(0.04);
  filter->SetMaximumError(array.GetDataPointer());

  // Set the value of the standard deviation of the Gaussian used for smoothing
  FilterType::SigmaArrayType::ValueType sigmaValue = 1.0;
  FilterType::SigmaArrayType            sigma;
  sigma.Fill(sigmaValue);

  filter->SetSigma(sigmaValue);
  ITK_TEST_SET_GET_VALUE(sigmaValue, filter->GetSigma());

  filter->SetSigma(sigma);
  ITK_TEST_SET_GET_VALUE(sigma, filter->GetSigmaArray());

  filter->SetSigmaArray(sigma);
  ITK_TEST_SET_GET_VALUE(sigma, filter->GetSigmaArray());

  // Set the boundary condition used by the filter
  itk::ConstantBoundaryCondition<ImageType> constantBoundaryCondition;
  filter->SetInputBoundaryCondition(&constantBoundaryCondition);
  ITK_TEST_SET_GET_VALUE(&constantBoundaryCondition, filter->GetInputBoundaryCondition());

  filter->SetRealBoundaryCondition(&constantBoundaryCondition);
  ITK_TEST_SET_GET_VALUE(&constantBoundaryCondition, filter->GetRealBoundaryCondition());

  // Set other filter properties
  FilterType::ArrayType::ValueType varianceValue = 1.0;
  FilterType::ArrayType            variance;
  variance.Fill(varianceValue);
  filter->SetVariance(variance);
  ITK_TEST_SET_GET_VALUE(variance, filter->GetVariance());

  FilterType::ArrayType::ValueType maximumErrorValue = 0.01;
  FilterType::ArrayType            maximumError;
  maximumError.Fill(maximumErrorValue);
  filter->SetMaximumError(maximumError);
  ITK_TEST_SET_GET_VALUE(maximumError, filter->GetMaximumError());

  unsigned int maximumKernelWidth = 32;
  filter->SetMaximumKernelWidth(maximumKernelWidth);
  ITK_TEST_SET_GET_VALUE(maximumKernelWidth, filter->GetMaximumKernelWidth());

  filter->SetFilterDimensionality(Dimension);
  ITK_TEST_SET_GET_VALUE(Dimension, filter->GetFilterDimensionality());

  // Verify kernel radius matches expectations for test parameters
  filter->UseImageSpacingOff();
  constexpr unsigned int EXPECTED_RADIUS = 3;
  auto                   radius = filter->GetKernelRadius();
  auto                   kernelSize = filter->GetKernelSize();
  for (unsigned int idx = 0; idx < Dimension; ++idx)
  {
    ITK_TEST_EXPECT_EQUAL(radius[idx], EXPECTED_RADIUS);
    ITK_TEST_EXPECT_EQUAL(filter->GetKernelRadius(idx), EXPECTED_RADIUS);
    ITK_TEST_EXPECT_EQUAL(kernelSize[idx], EXPECTED_RADIUS * 2 + 1);
  }

  // Verify filter throws exception when trying to get kernel information
  // if UseImageSpacing is ON and an input image is not set
  filter->UseImageSpacingOn();
  ITK_TRY_EXPECT_EXCEPTION(filter->GetKernelRadius());
  ITK_TRY_EXPECT_EXCEPTION(filter->GetKernelRadius(0));
  ITK_TRY_EXPECT_EXCEPTION(filter->GetKernelSize());

  auto useImageSpacing = static_cast<bool>(std::stoi(argv[1]));
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  if (useImageSpacing)
  {
    filter->SetUseImageSpacingOn();
  }
  else
  {
    filter->SetUseImageSpacingOff();
  }
#endif
  ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);

  // Run test
  itk::Size<3> sz;
  sz[0] = 100; // std::stoi(argv[1]);
  sz[1] = 100; // std::stoi(argv[2]);
  sz[2] = 40;

  itk::NullImageToImageFilterDriver<ImageType, ImageType> test1;
  test1.SetImageSize(sz);
  test1.SetFilter(filter);

  ITK_TRY_EXPECT_NO_EXCEPTION(test1.Execute());

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
