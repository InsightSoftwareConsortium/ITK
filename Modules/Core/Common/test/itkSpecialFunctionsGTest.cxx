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

#include "itkSpecialFunctions.h"
#include "itkMath.h"
#include <gtest/gtest.h>
#include <cmath>

namespace
{
// Compare with a relative tolerance for non-tiny references, absolute otherwise.
void
ExpectClose(double actual, double expected, double relTol, double absTol)
{
  const double err = itk::Math::Absolute(actual - expected);
  const double scale = itk::Math::Absolute(expected);
  if (scale > absTol)
  {
    EXPECT_LE(err / scale, relTol) << "actual=" << actual << " expected=" << expected;
  }
  else
  {
    EXPECT_LE(err, absTol) << "actual=" << actual << " expected=" << expected;
  }
}

// SciPy reference values: gammainc = regularized P(a,x), betainc = regularized I_x(a,b).
struct GammaCase
{
  double a, x, p;
};
struct BetaCase
{
  double a, b, x, i;
};

const GammaCase gammaCases[] = {
  { 0.5, 0.5, 0.68268949213708585 },
  { 0.5, 2.0, 0.95449973610364158 },
  { 1.0, 1.0, 0.63212055882855767 },
  { 2.0, 1.0, 0.26424111765711528 },
  { 2.0, 5.0, 0.95957231800548726 },
  { 5.0, 2.0, 0.052653017343711125 },
  { 5.0, 10.0, 0.97074731192303887 },
  { 0.5, 5.0, 0.9984345977419975 },
  { 50.0, 40.0, 0.070335066659394929 },
  { 50.0, 60.0, 0.91559331890630802 },
  { 3.0, 0.0, 0.0 },
  { 3.0, 0.001, 1.6654171665278094e-10 },
};

const BetaCase betaCases[] = {
  { 2.0, 3.0, 0.5, 0.6875 },
  { 0.5, 0.5, 0.5, 0.50000000000000011 },
  { 2.0, 2.0, 0.25, 0.15625 },
  { 5.0, 1.0, 0.9, 0.59049000000000007 },
  { 1.0, 5.0, 0.1, 0.40951000000000004 },
  { 10.0, 0.5, 0.5, 0.00023344743474862218 },
  { 0.5, 5.0, 0.2, 0.85507239459591955 },
  { 3.0, 7.0, 0.7, 0.99570910599999995 },
  { 2.0, 3.0, 0.0, 0.0 },
  { 2.0, 3.0, 1.0, 1.0 },
};
} // namespace

TEST(SpecialFunctions, IncompleteGammaPReference)
{
  for (const auto & c : gammaCases)
  {
    ExpectClose(itk::Math::IncompleteGammaP(c.a, c.x), c.p, 1e-12, 1e-15);
  }
}

TEST(SpecialFunctions, RegularizedIncompleteBetaReference)
{
  for (const auto & c : betaCases)
  {
    ExpectClose(itk::Math::RegularizedIncompleteBeta(c.a, c.b, c.x), c.i, 1e-12, 1e-15);
  }
}

// Boundary identities: P(a,0)=0, P(a,inf)=1, I_0(a,b)=0, I_1(a,b)=1.
TEST(SpecialFunctions, Boundaries)
{
  EXPECT_DOUBLE_EQ(itk::Math::IncompleteGammaP(3.0, 0.0), 0.0);
  ExpectClose(itk::Math::IncompleteGammaP(3.0, 1e6), 1.0, 1e-12, 1e-15);
  EXPECT_DOUBLE_EQ(itk::Math::RegularizedIncompleteBeta(2.0, 3.0, 0.0), 0.0);
  EXPECT_DOUBLE_EQ(itk::Math::RegularizedIncompleteBeta(2.0, 3.0, 1.0), 1.0);
}

// Incomplete-beta symmetry I_x(a,b) + I_{1-x}(b,a) = 1, across the CF crossover.
TEST(SpecialFunctions, BetaSymmetry)
{
  const double pairs[][3] = { { 2.0, 5.0, 0.3 }, { 5.0, 2.0, 0.8 }, { 0.5, 3.0, 0.6 }, { 7.0, 4.0, 0.45 } };
  for (const auto & p : pairs)
  {
    const double left = itk::Math::RegularizedIncompleteBeta(p[0], p[1], p[2]);
    const double right = itk::Math::RegularizedIncompleteBeta(p[1], p[0], 1.0 - p[2]);
    ExpectClose(left + right, 1.0, 1e-12, 1e-15);
  }
}

// The float overload resolves and tracks the double result within float epsilon.
TEST(SpecialFunctions, FloatOverload)
{
  EXPECT_NEAR(itk::Math::IncompleteGammaP(2.0f, 5.0f), 0.95957232f, 1e-5f);
  EXPECT_NEAR(itk::Math::RegularizedIncompleteBeta(2.0f, 3.0f, 0.5f), 0.6875f, 1e-5f);
}
