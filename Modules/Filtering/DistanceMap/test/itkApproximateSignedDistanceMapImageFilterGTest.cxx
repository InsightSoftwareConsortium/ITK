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

#include "itkApproximateSignedDistanceMapImageFilter.h"

#include "itkGTest.h"

TEST(ApproximateSignedDistanceMapImageFilter, DefaultInsideValueIsNegative)
{
  using ImageType = itk::Image<float, 2>;
  using FilterType = itk::ApproximateSignedDistanceMapImageFilter<ImageType, ImageType>;

  const auto filter = FilterType::New();

  // NumericTraits<float>::min() is the smallest positive value, not the most negative one.
  EXPECT_LT(filter->GetInsideValue(), 0.0f);
}
