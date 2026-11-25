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

#include "itkFFTWCommon.h"
#include "gtest/gtest.h"

TEST(FFTWCommon, LimitThreadsByProblemSize)
{
  using itk::fftw::LimitThreadsByProblemSize;
  constexpr int perThread = 262144;

  const int below[] = { perThread - 1 };
  EXPECT_EQ(LimitThreadsByProblemSize(1, below, 24), 1);

  const int exact[] = { perThread };
  EXPECT_EQ(LimitThreadsByProblemSize(1, exact, 24), 1);

  const int twice[] = { 2 * perThread };
  EXPECT_EQ(LimitThreadsByProblemSize(1, twice, 24), 2);
  EXPECT_EQ(LimitThreadsByProblemSize(1, twice, 1), 1);

  // All dimensions count: 1024^2 and 128^3 hold 4 and 8 blocks of 2^18 elements.
  const int n2D[] = { 1024, 1024 };
  EXPECT_EQ(LimitThreadsByProblemSize(2, n2D, 24), 4);
  const int n3D[] = { 128, 128, 128 };
  EXPECT_EQ(LimitThreadsByProblemSize(3, n3D, 24), 8);
  EXPECT_EQ(LimitThreadsByProblemSize(3, n3D, 3), 3);

  // 2048^3 elements overflow int.
  const int large[] = { 2048, 2048, 2048 };
  EXPECT_EQ(LimitThreadsByProblemSize(3, large, 24), 24);

  const int tiny[] = { 8, 8 };
  EXPECT_EQ(LimitThreadsByProblemSize(2, tiny, 0), 1);
}
