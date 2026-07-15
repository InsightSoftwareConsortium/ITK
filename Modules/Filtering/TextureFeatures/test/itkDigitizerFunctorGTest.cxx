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

#include "itkDigitizerFunctor.h"

#include "itkGTest.h"

#include <cmath>

TEST(DigitizerFunctor, DefaultConstructorUsesFullNegativeToPositiveRange)
{
  const itk::Statistics::Digitizer<float> digitizer;

  // NumericTraits<float>::min() is the smallest positive value, not the most negative one.
  EXPECT_LT(digitizer.m_Min, 0.0f);

  constexpr float maskValue = 1.0f;
  constexpr float negativePixel = -1.0f;

  EXPECT_NE(digitizer(maskValue, negativePixel), -1.0f);
}

// PixelType == double has no headroom above RealType (also double): the
// unhalved m_Max - m_Min would overflow to +inf, silently mapping every
// pixel to bin 0 (issue #6575).
TEST(DigitizerFunctor, DoublePixelTypeBinsAcrossFullRangeWithoutOverflow)
{
  const itk::Statistics::Digitizer<double> digitizer;

  EXPECT_LT(digitizer.m_Min, 0.0);
  EXPECT_GT(digitizer.m_Max, 0.0);
  ASSERT_TRUE(std::isfinite(digitizer.m_Max - digitizer.m_Min));

  constexpr double maskValue = 1.0;

  // Before the fix, m_Max - m_Min overflowed to +inf, so
  // (inputPixel - m_Min) / ((m_Max - m_Min) / N) was 0 for every in-range
  // pixel: every value silently mapped to bin 0. A representative central
  // value must instead land near the middle of the default 256 bins.
  const auto midBin = digitizer(maskValue, 0.0);
  EXPECT_NE(midBin, 0);
  EXPECT_NEAR(midBin, 128, 2);
}
