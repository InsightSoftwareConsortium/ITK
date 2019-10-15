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

#include "itkKrcahEigenToScalarPreprocessingImageToImageFilter.h"
#include "itkTestingMacros.h"

int
itkKrcahEigenToScalarPreprocessingImageToImageFilterTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;
  using InputPixelType = char;
  using InputImageType = itk::Image<InputPixelType, Dimension>;

  using FilterType = itk::KrcahEigenToScalarPreprocessingImageToImageFilter<InputImageType>;
  FilterType::Pointer filter = FilterType::New();

  /* Basic test */
  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, KrcahEigenToScalarPreprocessingImageToImageFilter, ImageToImageFilter);

  /* Default parameters */
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(filter->GetScalingConstant(), 10.0, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(filter->GetSigma(), 1.0, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(filter->GetReleaseInternalFilterData());

  /* TODO: Regression test */

  return EXIT_SUCCESS;
}
