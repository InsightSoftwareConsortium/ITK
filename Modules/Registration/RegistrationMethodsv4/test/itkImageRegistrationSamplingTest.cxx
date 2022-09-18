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

#include "itkImageRegistrationMethodv4.h"
#include "itkTestingMacros.h"

/*
 * Test the SetMetricSamplingPercentage and SetMetricSamplingPercentagePerLevel.
 * We only need to explicitly run the SetMetricSamplingPercentage method because it
 * invokes the SetMetricSamplingPercentagePerLevel method.
 */
int
itkImageRegistrationSamplingTest(int, char *[])
{
  using PixelType = double;
  using FixedImageType = itk::Image<PixelType, 2>;
  using MovingImageType = itk::Image<PixelType, 2>;

  using RegistrationType = itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType>;
  auto registrationMethod = RegistrationType::New();

  ITK_TRY_EXPECT_NO_EXCEPTION(registrationMethod->SetMetricSamplingPercentage(0.1));


  constexpr unsigned int     NUM_ERRORS = 3;
  RegistrationType::RealType errorValues[NUM_ERRORS] = { -0.1, 0.0, 1.1 };
  for (double errorValue : errorValues)
  {
    ITK_TRY_EXPECT_EXCEPTION(registrationMethod->SetMetricSamplingPercentage(errorValue));
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
