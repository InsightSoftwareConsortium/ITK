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
#include "itkRealTimeInterval.h"
#include "itkGTest.h"

#include "itkMath.h"
#include "itkNumericTraits.h"

namespace
{
void
CheckForValue(double a, double b)
{
  double eps = 4.0 * itk::NumericTraits<double>::epsilon();
  ITK_GCC_PRAGMA_PUSH
  ITK_GCC_SUPPRESS_Wfloat_equal
  eps = (b == 0.0) ? eps : itk::Math::Absolute(b * eps);
  ITK_GCC_PRAGMA_POP
  EXPECT_LE(itk::Math::Absolute(a - b), eps);
}
} // namespace

TEST(RealTimeInterval, DefaultConstructor)
{
  const itk::RealTimeInterval interval0;

  CheckForValue(interval0.GetTimeInMicroSeconds(), 0.0);
  CheckForValue(interval0.GetTimeInMilliSeconds(), 0.0);
  CheckForValue(interval0.GetTimeInSeconds(), 0.0);
  CheckForValue(interval0.GetTimeInMinutes(), 0.0);
  CheckForValue(interval0.GetTimeInHours(), 0.0);
  CheckForValue(interval0.GetTimeInDays(), 0.0);
}

TEST(RealTimeInterval, AccumulationAndSubtraction)
{
  const itk::RealTimeInterval interval0;
  itk::RealTimeInterval       intervalX = interval0;

  const itk::RealTimeInterval oneSecond(1, 0);
  for (unsigned int i = 0; i < 1000000L; ++i)
  {
    intervalX += oneSecond;
  }

  itk::RealTimeInterval manySeconds = intervalX - interval0;
  CheckForValue(manySeconds.GetTimeInSeconds(), 1000000.0);

  itk::RealTimeInterval fiveMicroseconds;
  fiveMicroseconds.Set(0, 5);

  itk::RealTimeInterval interval3 = interval0;

  for (unsigned int i = 0; i < 1000000L; ++i)
  {
    interval3 += fiveMicroseconds;
  }

  manySeconds = interval3 - interval0;
  CheckForValue(manySeconds.GetTimeInSeconds(), 5.0);

  for (unsigned int i = 0; i < 1000000L; ++i)
  {
    interval3 -= fiveMicroseconds;
  }

  manySeconds = interval3 - interval0;
  CheckForValue(manySeconds.GetTimeInSeconds(), 0.0);
}

TEST(RealTimeInterval, SetWithNormalization)
{
  itk::RealTimeInterval timeSpan;

  timeSpan.Set(19, -5000000L);
  CheckForValue(timeSpan.GetTimeInSeconds(), 14.0);

  timeSpan.Set(-19, 5000000L);
  CheckForValue(timeSpan.GetTimeInSeconds(), -14.0);

  timeSpan.Set(-19, -5000000L);
  CheckForValue(timeSpan.GetTimeInSeconds(), -24.0);

  timeSpan.Set(19, 5000000L);
  CheckForValue(timeSpan.GetTimeInSeconds(), 24.0);
}

TEST(RealTimeInterval, Addition)
{
  const itk::RealTimeInterval timeSpan1(19, 300000L);
  const itk::RealTimeInterval timeSpan2(13, 500000L);

  const itk::RealTimeInterval timeSpan3 = timeSpan1 + timeSpan2;
  CheckForValue(timeSpan3.GetTimeInSeconds(), 32.8);
}

TEST(RealTimeInterval, ComparisonOperators)
{
  // Test comparison operations
  const itk::RealTimeInterval dt1(15, 13);
  const itk::RealTimeInterval dt2(19, 11);
  const itk::RealTimeInterval dt3(15, 25);

  EXPECT_TRUE(dt1 == dt1);
  EXPECT_TRUE(dt1 != dt2);
  EXPECT_FALSE(dt1 != dt1);
  EXPECT_TRUE(dt2 >= dt1);
  EXPECT_TRUE(dt1 >= dt1);
  EXPECT_TRUE(dt2 > dt1);
  EXPECT_TRUE(dt1 <= dt2);
  EXPECT_TRUE(dt1 <= dt1);
  EXPECT_TRUE(dt1 < dt2);

  EXPECT_TRUE(dt3 == dt3);
  EXPECT_TRUE(dt1 != dt3);
  EXPECT_TRUE(dt3 >= dt1);
  EXPECT_TRUE(dt3 > dt1);
  EXPECT_FALSE(dt3 <= dt1);
  EXPECT_FALSE(dt3 < dt1);
  EXPECT_TRUE(dt1 <= dt3);
  EXPECT_TRUE(dt1 < dt3);
  EXPECT_FALSE(dt1 >= dt3);
  EXPECT_FALSE(dt1 > dt3);
}
