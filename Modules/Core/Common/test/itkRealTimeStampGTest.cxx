// SPDX-FileCopyrightText: Copyright NumFOCUS
// SPDX-License-Identifier: Apache-2.0
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
#include "itkRealTimeStamp.h"
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

TEST(RealTimeStamp, DefaultConstructor)
{
  const itk::RealTimeStamp stamp0;

  CheckForValue(stamp0.GetTimeInMicroSeconds(), 0.0);
  CheckForValue(stamp0.GetTimeInMilliSeconds(), 0.0);
  CheckForValue(stamp0.GetTimeInSeconds(), 0.0);
  CheckForValue(stamp0.GetTimeInHours(), 0.0);
  CheckForValue(stamp0.GetTimeInDays(), 0.0);
}

TEST(RealTimeStamp, NegativeIntervalThrows)
{
  itk::RealTimeStamp          stamp;
  const itk::RealTimeInterval minusOneSecond(-1, 0);

  EXPECT_THROW(stamp += minusOneSecond, itk::ExceptionObject);
}

TEST(RealTimeStamp, AccumulationAndSubtraction)
{
  const itk::RealTimeStamp stamp0;
  itk::RealTimeStamp       stamp2 = stamp0;

  const itk::RealTimeInterval oneSecond(1, 0);
  for (unsigned int i = 0; i < 1000000L; ++i)
  {
    stamp2 += oneSecond;
  }

  itk::RealTimeInterval manySeconds = stamp2 - stamp0;
  CheckForValue(manySeconds.GetTimeInSeconds(), 1000000.0);

  itk::RealTimeInterval fiveMicroseconds;
  fiveMicroseconds.Set(0, 5);

  itk::RealTimeStamp stamp3 = stamp0;

  for (unsigned int i = 0; i < 1000000L; ++i)
  {
    stamp3 += fiveMicroseconds;
  }

  manySeconds = stamp3 - stamp0;
  CheckForValue(manySeconds.GetTimeInSeconds(), 5.0);

  for (unsigned int i = 0; i < 1000000L; ++i)
  {
    stamp3 -= fiveMicroseconds;
  }

  manySeconds = stamp3 - stamp0;
  CheckForValue(manySeconds.GetTimeInSeconds(), 0.0);

  const itk::RealTimeInterval minusOneSecond(-1, 0);
  EXPECT_THROW(stamp3 += minusOneSecond, itk::ExceptionObject);
}

TEST(RealTimeStamp, IntervalSetWithNormalization)
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

TEST(RealTimeStamp, IntervalAddition)
{
  const itk::RealTimeInterval timeSpan1(19, 300000L);
  const itk::RealTimeInterval timeSpan2(13, 500000L);

  const itk::RealTimeInterval timeSpan3 = timeSpan1 + timeSpan2;
  CheckForValue(timeSpan3.GetTimeInSeconds(), 32.8);
}

TEST(RealTimeStamp, ComparisonOperators)
{
  // Test comparison operations
  const itk::RealTimeInterval dt1(15, 13);
  const itk::RealTimeInterval dt2(19, 11);
  const itk::RealTimeInterval dt3(15, 25);

  itk::RealTimeInterval t1;
  t1 += dt1;

  itk::RealTimeInterval t2;
  t2 += dt2;

  itk::RealTimeInterval t3;
  t3 += dt3;

  EXPECT_TRUE(t1 == t1);
  EXPECT_TRUE(t1 != t2);
  EXPECT_FALSE(t1 != t1);
  EXPECT_TRUE(t2 >= t1);
  EXPECT_TRUE(t1 >= t1);
  EXPECT_TRUE(t2 > t1);
  EXPECT_TRUE(t1 <= t2);
  EXPECT_TRUE(t1 <= t1);
  EXPECT_TRUE(t1 < t2);

  EXPECT_TRUE(t3 == t3);
  EXPECT_TRUE(t1 != t3);
  EXPECT_TRUE(t3 >= t1);
  EXPECT_TRUE(t3 > t1);
  EXPECT_FALSE(t3 <= t1);
  EXPECT_FALSE(t3 < t1);
  EXPECT_TRUE(t1 <= t3);
  EXPECT_TRUE(t1 < t3);
  EXPECT_FALSE(t1 >= t3);
  EXPECT_FALSE(t1 > t3);
}
