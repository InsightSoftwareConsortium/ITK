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
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkTestingMacros.h"

int
itkGradientAnisotropicDiffusionImageFilterTest(int itkNotUsed(argc), char * itkNotUsed(argv)[])
{
  constexpr unsigned int Dimension = 2;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  using FilterType = itk::GradientAnisotropicDiffusionImageFilter<ImageType, ImageType>;

  // Set up filter
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, GradientAnisotropicDiffusionImageFilter, AnisotropicDiffusionImageFilter);


  itk::IdentifierType numberOfIterations = 1;
  filter->SetNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, filter->GetNumberOfIterations());

  FilterType::TimeStepType timeStep = 0.125;
  filter->SetTimeStep(timeStep);
  ITK_TEST_SET_GET_VALUE(timeStep, filter->GetTimeStep());

  auto conductanceParameter = 3.0;
  filter->SetConductanceParameter(conductanceParameter);
  ITK_TEST_SET_GET_VALUE(conductanceParameter, filter->GetConductanceParameter());

  unsigned int conductanceScalingUpdateInterval = 1;
  filter->SetConductanceScalingUpdateInterval(conductanceScalingUpdateInterval);
  ITK_TEST_SET_GET_VALUE(conductanceScalingUpdateInterval, filter->GetConductanceScalingUpdateInterval());

  auto conductanceScalingParameter = 1.0;
  filter->SetConductanceScalingParameter(conductanceScalingParameter);
  ITK_TEST_SET_GET_VALUE(conductanceScalingParameter, filter->GetConductanceScalingParameter());

  auto fixedAverageGradientMagnitude = 1.0;
  filter->SetFixedAverageGradientMagnitude(fixedAverageGradientMagnitude);
  ITK_TEST_SET_GET_VALUE(fixedAverageGradientMagnitude, filter->GetFixedAverageGradientMagnitude());

  // Run test
  itk::Size<Dimension> sz;
  sz[0] = 250;
  sz[1] = 250;
  itk::NullImageToImageFilterDriver<ImageType, ImageType> test1;
  test1.SetImageSize(sz);
  test1.SetFilter(filter);

  ITK_TRY_EXPECT_NO_EXCEPTION(test1.Execute());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
