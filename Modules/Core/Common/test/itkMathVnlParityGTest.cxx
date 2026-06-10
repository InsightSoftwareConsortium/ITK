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

// Verifies that the native itk::Math functions are drop-in replacements for
// the vnl_math functions they superseded: identical results, bit-for-bit,
// over the inputs each contract supports.

#include "itkMath.h"
#include "itkGTest.h"
#include <vnl/vnl_math.h>
#include <cmath>
#include <limits>

namespace
{
constexpr double maxHalfIntDomain = static_cast<double>(INT_MAX / 2) - 2.0;

const double roundingSamples[] = { 0.0,  -0.0,   0.5,        -0.5,      1.5,     -1.5,    2.5,   -2.5,  3.5,
                                   -3.5, 0.4999, -0.4999,    0.5001,    -0.5001, 7.0,     -7.0,  7.25,  -7.25,
                                   1e6,  -1e6,   1234567.89, -987654.3, 0.0001,  -0.0001, 1.5e9, -1.5e9 };

const double angleSamples[] = { 0.0,
                                -0.0,
                                0.1,
                                -0.1,
                                itk::Math::pi / 2,
                                -itk::Math::pi / 2,
                                itk::Math::pi,
                                -itk::Math::pi,
                                3 * itk::Math::pi / 2,
                                -3 * itk::Math::pi / 2,
                                itk::Math::twopi,
                                -itk::Math::twopi,
                                itk::Math::twopi + 0.1,
                                -itk::Math::twopi - 0.1,
                                100.25,
                                -100.25,
                                1e6,
                                -1e6,
                                -1e-18,
                                std::nextafter(0.0, -1.0) };
} // namespace

TEST(itkMathVnlParity, RoundHalfIntegerToEven)
{
  for (const double x : roundingSamples)
  {
    EXPECT_EQ(itk::Math::rnd_halfinttoeven(x), vnl_math::rnd_halfinttoeven(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::rnd_halfinttoeven(static_cast<float>(x)), vnl_math::rnd_halfinttoeven(static_cast<float>(x)))
      << "x=" << x;
    EXPECT_EQ(itk::Math::rnd(x), vnl_math::rnd(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::rnd(static_cast<float>(x)), vnl_math::rnd(static_cast<float>(x))) << "x=" << x;
  }
}

TEST(itkMathVnlParity, RoundHalfIntegerUp)
{
  for (const double x : roundingSamples)
  {
    if (std::abs(x) >= maxHalfIntDomain)
    {
      continue; // outside the documented |x| < INT_MAX/2 contract
    }
    EXPECT_EQ(itk::Math::rnd_halfintup(x), vnl_math::rnd_halfintup(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::rnd_halfintup(static_cast<float>(x)), vnl_math::rnd_halfintup(static_cast<float>(x)))
      << "x=" << x;
  }
}

TEST(itkMathVnlParity, FloorCeil)
{
  for (const double x : roundingSamples)
  {
    EXPECT_EQ(itk::Math::floor(x), vnl_math::floor(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::floor(static_cast<float>(x)), vnl_math::floor(static_cast<float>(x))) << "x=" << x;
    EXPECT_EQ(itk::Math::ceil(x), vnl_math::ceil(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::ceil(static_cast<float>(x)), vnl_math::ceil(static_cast<float>(x))) << "x=" << x;
  }
}

TEST(itkMathVnlParity, SgnAndSgn0)
{
  for (const int x : { -5, -1, 0, 1, 5, INT_MAX, INT_MIN })
  {
    EXPECT_EQ(itk::Math::sgn(x), vnl_math::sgn(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::sgn0(x), vnl_math::sgn0(x)) << "x=" << x;
  }
  for (const double x : { -2.5, -0.0, 0.0, 1e-300, -1e-300, 3.75 })
  {
    EXPECT_EQ(itk::Math::sgn(x), vnl_math::sgn(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::sgn0(x), vnl_math::sgn0(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::sgn(static_cast<float>(x)), vnl_math::sgn(static_cast<float>(x))) << "x=" << x;
    EXPECT_EQ(itk::Math::sgn0(static_cast<float>(x)), vnl_math::sgn0(static_cast<float>(x))) << "x=" << x;
  }
}

TEST(itkMathVnlParity, SqrAndCube)
{
  for (const int x : { -46340, -7, -1, 0, 1, 7, 46340 })
  {
    EXPECT_EQ(itk::Math::sqr(x), vnl_math::sqr(x)) << "x=" << x;
  }
  for (const int x : { -1290, -3, 0, 3, 1290 })
  {
    EXPECT_EQ(itk::Math::cube(x), vnl_math::cube(x)) << "x=" << x;
  }
  for (const double x : { -2.5, 0.0, 0.125, 3.75, 1e150 })
  {
    EXPECT_EQ(itk::Math::sqr(x), vnl_math::sqr(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::cube(x), vnl_math::cube(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::sqr(static_cast<float>(x)), vnl_math::sqr(static_cast<float>(x))) << "x=" << x;
  }
}

TEST(itkMathVnlParity, SquaredMagnitude)
{
  // static_cast: char is unsigned on some targets and char{ -11 } would narrow.
  for (const char x : { static_cast<char>(-11), char{ 0 }, char{ 13 }, char{ 127 } })
  {
    EXPECT_EQ(itk::Math::squared_magnitude(x), vnl_math::squared_magnitude(x)) << "x=" << static_cast<int>(x);
  }
  for (const unsigned char x : { static_cast<unsigned char>(0u), static_cast<unsigned char>(200u) })
  {
    EXPECT_EQ(itk::Math::squared_magnitude(x), vnl_math::squared_magnitude(x)) << "x=" << static_cast<int>(x);
  }
  for (const int x : { -46340, -7, 0, 7, 46340 })
  {
    EXPECT_EQ(itk::Math::squared_magnitude(x), vnl_math::squared_magnitude(x)) << "x=" << x;
  }
  for (const unsigned int x : { 0u, 65535u, 65536u })
  {
    EXPECT_EQ(itk::Math::squared_magnitude(x), vnl_math::squared_magnitude(x)) << "x=" << x;
  }
  // Largest long whose square fits the unsigned result; long is 32-bit on LLP64.
  const long longSafe = static_cast<long>(std::sqrt(static_cast<double>(std::numeric_limits<long>::max()))) - 2;
  for (const long x : { -longSafe, -42L, 0L, 42L, longSafe })
  {
    EXPECT_EQ(itk::Math::squared_magnitude(x), vnl_math::squared_magnitude(x)) << "x=" << x;
  }
  for (const long long x : { -3037000499LL, 0LL, 3037000499LL })
  {
    EXPECT_EQ(itk::Math::squared_magnitude(x), vnl_math::squared_magnitude(x)) << "x=" << x;
  }
  for (const double x : { -2.5, 0.0, 3.75, 1e150 })
  {
    EXPECT_EQ(itk::Math::squared_magnitude(x), vnl_math::squared_magnitude(x)) << "x=" << x;
    EXPECT_EQ(itk::Math::squared_magnitude(static_cast<float>(x)), vnl_math::squared_magnitude(static_cast<float>(x)))
      << "x=" << x;
    EXPECT_EQ(itk::Math::squared_magnitude(static_cast<long double>(x)),
              vnl_math::squared_magnitude(static_cast<long double>(x)))
      << "x=" << x;
  }
}

TEST(itkMathVnlParity, RemainderSignMatrix)
{
  for (const int x : { -7, -3, 0, 3, 7, 1000000 })
  {
    for (const int y : { -5, -3, 3, 5, 1000003 })
    {
      EXPECT_EQ(itk::Math::remainder_truncated(x, y), vnl_math::remainder_truncated(x, y)) << "x=" << x << " y=" << y;
      EXPECT_EQ(itk::Math::remainder_floored(x, y), vnl_math::remainder_floored(x, y)) << "x=" << x << " y=" << y;
    }
  }
  for (const double x : { -7.5, -2.25, 0.0, 2.25, 7.5 })
  {
    for (const double y : { -2.0, -0.75, 0.75, 2.0 })
    {
      EXPECT_EQ(itk::Math::remainder_truncated(x, y), vnl_math::remainder_truncated(x, y)) << "x=" << x << " y=" << y;
      EXPECT_EQ(itk::Math::remainder_floored(x, y), vnl_math::remainder_floored(x, y)) << "x=" << x << " y=" << y;
      const auto fx = static_cast<float>(x);
      const auto fy = static_cast<float>(y);
      EXPECT_EQ(itk::Math::remainder_truncated(fx, fy), vnl_math::remainder_truncated(fx, fy))
        << "x=" << x << " y=" << y;
      EXPECT_EQ(itk::Math::remainder_floored(fx, fy), vnl_math::remainder_floored(fx, fy)) << "x=" << x << " y=" << y;
    }
  }
}

TEST(itkMathVnlParity, AngleNormalization)
{
  for (const double angle : angleSamples)
  {
    const double itkWrapped = itk::Math::angle_0_to_2pi(angle);
    const double vnlWrapped = vnl_math::angle_0_to_2pi(angle);
    EXPECT_EQ(itkWrapped, vnlWrapped) << "angle=" << angle;
    EXPECT_EQ(std::signbit(itkWrapped), std::signbit(vnlWrapped)) << "angle=" << angle;
    EXPECT_EQ(itk::Math::angle_minuspi_to_pi(angle), vnl_math::angle_minuspi_to_pi(angle)) << "angle=" << angle;
  }
}

TEST(itkMathVnlParity, Constants)
{
  EXPECT_EQ(itk::Math::e, vnl_math::e);
  EXPECT_EQ(itk::Math::log2e, vnl_math::log2e);
  EXPECT_EQ(itk::Math::log10e, vnl_math::log10e);
  EXPECT_EQ(itk::Math::ln2, vnl_math::ln2);
  EXPECT_EQ(itk::Math::ln10, vnl_math::ln10);
  EXPECT_EQ(itk::Math::pi, vnl_math::pi);
  EXPECT_EQ(itk::Math::twopi, vnl_math::twopi);
  EXPECT_EQ(itk::Math::pi_over_2, vnl_math::pi_over_2);
  EXPECT_EQ(itk::Math::pi_over_4, vnl_math::pi_over_4);
  EXPECT_EQ(itk::Math::pi_over_180, vnl_math::pi_over_180);
  EXPECT_EQ(itk::Math::one_over_pi, vnl_math::one_over_pi);
  EXPECT_EQ(itk::Math::two_over_pi, vnl_math::two_over_pi);
  EXPECT_EQ(itk::Math::deg_per_rad, vnl_math::deg_per_rad);
  EXPECT_EQ(itk::Math::sqrt2pi, vnl_math::sqrt2pi);
  EXPECT_EQ(itk::Math::two_over_sqrtpi, vnl_math::two_over_sqrtpi);
  EXPECT_EQ(itk::Math::one_over_sqrt2pi, vnl_math::one_over_sqrt2pi);
  EXPECT_EQ(itk::Math::sqrt2, vnl_math::sqrt2);
  EXPECT_EQ(itk::Math::sqrt1_2, vnl_math::sqrt1_2);
  EXPECT_EQ(itk::Math::sqrt1_3, vnl_math::sqrt1_3);
  EXPECT_EQ(itk::Math::euler, vnl_math::euler);
  EXPECT_EQ(itk::Math::eps, vnl_math::eps);
  EXPECT_EQ(itk::Math::float_eps, vnl_math::float_eps);
  EXPECT_EQ(itk::Math::float_sqrteps, vnl_math::float_sqrteps);
  // Intentional 1 ULP divergence: itk::Math::sqrteps is the exact sqrt(eps) = 2^-26,
  // vnl_math::sqrteps rounds 1 ULP above it (see the ITKv6 migration guide).
  EXPECT_EQ(std::nextafter(itk::Math::sqrteps, 1.0), vnl_math::sqrteps);
}
