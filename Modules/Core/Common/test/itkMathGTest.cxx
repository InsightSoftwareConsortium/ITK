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

#if !defined(ITK_LEGACY_REMOVE)
// Suppress MSVC warnings from VS2022, saying: "warning C4996: 'std::complex<T>::complex': warning STL4037: The effect
// of instantiating the template std::complex for any type other than float, double, or long double is unspecified."
#  define _SILENCE_NONFLOATING_COMPLEX_DEPRECATION_WARNING
#endif

#include "itkMath.h"
#include "itkGTest.h"
#include <limits>
#include <type_traits>

namespace
{

constexpr auto maxUnsignedValue = std::numeric_limits<uintmax_t>::max();

using itk::Math::UnsignedPower;
using itk::Math::UnsignedProduct;

static_assert((UnsignedPower(0, 1) == 0) && (UnsignedPower(0, 2) == 0) && (UnsignedPower(0, 3) == 0),
              "Check powers of zero");
static_assert((UnsignedPower(1, 0) == 1) && (UnsignedPower(1, 1) == 1) && (UnsignedPower(1, 2) == 1),
              "Check powers of one");
static_assert((UnsignedPower(2, 0) == 1) && (UnsignedPower(3, 0) == 1) && (UnsignedPower(maxUnsignedValue, 0) == 1),
              "Check zero as exponent");
static_assert((UnsignedPower(2, 1) == 2) && (UnsignedPower(3, 1) == 3) &&
                (UnsignedPower(maxUnsignedValue, 1) == maxUnsignedValue),
              "Check one as exponent");
static_assert((UnsignedPower(2, 2) == 4) && (UnsignedPower(2, 3) == 8), "Check powers of two");
static_assert((UnsignedPower(3, 2) == 9) && (UnsignedPower(3, 3) == 27), "Check powers of three");
static_assert(UnsignedPower(2, std::numeric_limits<uintmax_t>::digits - 1) ==
                (uintmax_t{ 1 } << (std::numeric_limits<uintmax_t>::digits - 1)),
              "Check 2^63 (at least when uintmax_t is 64 bits)");

static_assert(std::is_same_v<decltype(UnsignedPower(1, 1)), uintmax_t>,
              "The return type of UnsignedPower should be uintmax_t by default.");
static_assert(std::is_same_v<decltype(UnsignedPower<uint8_t>(1, 1)), uint8_t> &&
                std::is_same_v<decltype(UnsignedPower<uint16_t>(1, 1)), uint16_t> &&
                std::is_same_v<decltype(UnsignedPower<uint32_t>(1, 1)), uint32_t>,
              "UnsignedPower allows specifying the return type by its template argument.");

static_assert((UnsignedProduct(0, 0) == 0) && (UnsignedProduct(0, 1) == 0) && (UnsignedProduct(1, 0) == 0) &&
                (UnsignedProduct(1, 1) == 1),
              "Check all product combinations of zero and one");
static_assert((UnsignedProduct(2, 3) == 6) && (UnsignedProduct(3, 2) == 6), "Check 2*3 and 3*2");
static_assert((UnsignedProduct(1, maxUnsignedValue) == maxUnsignedValue) &&
                (UnsignedProduct(maxUnsignedValue, 1) == maxUnsignedValue),
              "Check products with the maximum unsigned value");


template <typename T1, typename T2>
void
TestIntegersAreSame(const T1 & v1, const T2 & v2)
{
  EXPECT_EQ(static_cast<T2>(v1), v2) << v1 << " static_cast " << static_cast<T2>(v1) << " != " << v2;
  EXPECT_FALSE(itk::Math::AlmostEquals(v2, v1))
    << "itk::Math::AlmostEquals(" << v2 << ", " << v1 << ") should be false";
  EXPECT_FALSE(itk::Math::AlmostEquals(v1, v2))
    << "itk::Math::AlmostEquals(" << v1 << ", " << v2 << ") should be false";
}


template <typename T>
void
ExerciseIsPrime()
{
  EXPECT_FALSE(itk::Math::IsPrime(static_cast<T>(0)));
  EXPECT_FALSE(itk::Math::IsPrime(static_cast<T>(1)));
  EXPECT_TRUE(itk::Math::IsPrime(static_cast<T>(2)));
  EXPECT_TRUE(itk::Math::IsPrime(static_cast<T>(3)));
  EXPECT_FALSE(itk::Math::IsPrime(static_cast<T>(4)));
  EXPECT_TRUE(itk::Math::IsPrime(static_cast<T>(5)));
  EXPECT_FALSE(itk::Math::IsPrime(static_cast<T>(6)));
  EXPECT_TRUE(itk::Math::IsPrime(static_cast<T>(7)));
  EXPECT_FALSE(itk::Math::IsPrime(static_cast<T>(8)));
  EXPECT_FALSE(itk::Math::IsPrime(static_cast<T>(9)));
  EXPECT_FALSE(itk::Math::IsPrime(static_cast<T>(10)));
  EXPECT_TRUE(itk::Math::IsPrime(static_cast<T>(11)));
  EXPECT_FALSE(itk::Math::IsPrime(static_cast<T>(12)));
  EXPECT_TRUE(itk::Math::IsPrime(static_cast<T>(13)));
}


template <typename T>
void
ExerciseGreatestPrimeFactor()
{
  EXPECT_EQ(itk::Math::GreatestPrimeFactor(static_cast<T>(12)), 3u);
  EXPECT_EQ(itk::Math::GreatestPrimeFactor(static_cast<T>(75)), 5u);
  EXPECT_EQ(itk::Math::GreatestPrimeFactor(static_cast<T>(1024)), 2u);
}

union FloatRepresentationF
{
  float        asFloat;
  itk::int32_t asInt;
};

union FloatRepresentationD
{
  double       asFloat;
  itk::int64_t asInt;
};

} // namespace

TEST(itkMath, Constants)
{
  EXPECT_GT(itk::Math::e, 2.71);
  EXPECT_GT(itk::Math::pi, 3.14);
  // Just a simple consistency check as in the original test
  double val = itk::Math::e * itk::Math::log2e * itk::Math::log10e * itk::Math::ln2 * itk::Math::pi *
               itk::Math::pi_over_2 * itk::Math::pi_over_4 * itk::Math::one_over_pi * itk::Math::two_over_pi *
               itk::Math::two_over_sqrtpi * itk::Math::one_over_sqrt2pi * itk::Math::sqrt2 * itk::Math::sqrt1_2;
  EXPECT_GT(val, 0.0);
}

TEST(itkMath, FloatAlmostEqual)
{
  FloatRepresentationF floatRepresentationfx1;
  floatRepresentationfx1.asFloat = -1.0f;

  FloatRepresentationF floatRepresentationfx2;
  floatRepresentationfx2.asFloat = itk::Math::FloatAddULP(floatRepresentationfx1.asFloat, -1);
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat), -1);
  EXPECT_TRUE(itk::Math::FloatAlmostEqual(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat));

  floatRepresentationfx2.asFloat = itk::Math::FloatAddULP(floatRepresentationfx1.asFloat, 1);
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat), 1);
  EXPECT_TRUE(itk::Math::FloatAlmostEqual(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat));

  floatRepresentationfx1.asFloat = 1.0f;
  floatRepresentationfx2.asFloat = itk::Math::FloatAddULP(floatRepresentationfx1.asFloat, 1);
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat), -1);
  EXPECT_TRUE(itk::Math::FloatAlmostEqual(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat));

  floatRepresentationfx2.asFloat = itk::Math::FloatAddULP(floatRepresentationfx1.asFloat, -1);
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat), 1);
  EXPECT_TRUE(itk::Math::FloatAlmostEqual(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat));

  // The default maxUlps is 4, so this should not be considered almost equals.
  floatRepresentationfx2.asFloat = itk::Math::FloatAddULP(floatRepresentationfx1.asFloat, 6);
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat), -6);
  EXPECT_FALSE(itk::Math::FloatAlmostEqual(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat));

  floatRepresentationfx2.asFloat = itk::Math::FloatAddULP(floatRepresentationfx1.asFloat, -6);
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat), 6);
  EXPECT_FALSE(itk::Math::FloatAlmostEqual(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat));

  floatRepresentationfx1.asFloat = -0.0f;
  floatRepresentationfx2.asFloat = 0.0f;
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat), 0);
  EXPECT_TRUE(itk::Math::FloatAlmostEqual(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat));

  floatRepresentationfx1.asFloat = 0.0f;
  floatRepresentationfx2.asFloat = 67329.234f - 67329.242f;
  EXPECT_TRUE(itk::Math::FloatAlmostEqual(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat, 4, 0.1f));

  floatRepresentationfx1.asFloat = 1e-8f;
  floatRepresentationfx2.asFloat = -1e-8f;
  EXPECT_GE(itk::Math::FloatDifferenceULP(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat), 0);

  floatRepresentationfx1.asFloat = -1e-8f;
  floatRepresentationfx2.asFloat = 1e-8f;
  EXPECT_LE(itk::Math::FloatDifferenceULP(floatRepresentationfx1.asFloat, floatRepresentationfx2.asFloat), 0);
}

TEST(itkMath, DoubleAlmostEqual)
{
  FloatRepresentationD floatRepresentationdx1;
  floatRepresentationdx1.asFloat = -1.0;

  FloatRepresentationD floatRepresentationdx2;
  floatRepresentationdx2.asFloat = itk::Math::FloatAddULP(floatRepresentationdx1.asFloat, -1);
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat), -1);
  EXPECT_TRUE(itk::Math::FloatAlmostEqual(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat));

  floatRepresentationdx2.asFloat = itk::Math::FloatAddULP(floatRepresentationdx1.asFloat, 1);
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat), 1);
  EXPECT_TRUE(itk::Math::FloatAlmostEqual(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat));

  // The default maxUlps is 4, so this should not be considered almost equals.
  floatRepresentationdx2.asFloat = itk::Math::FloatAddULP(floatRepresentationdx1.asFloat, -6);
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat), -6);
  EXPECT_FALSE(itk::Math::FloatAlmostEqual(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat));

  floatRepresentationdx2.asFloat = itk::Math::FloatAddULP(floatRepresentationdx1.asFloat, 6);
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat), 6);
  EXPECT_FALSE(itk::Math::FloatAlmostEqual(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat));

  floatRepresentationdx1.asFloat = -0.0;
  floatRepresentationdx2.asFloat = 0.0;
  EXPECT_EQ(itk::Math::FloatDifferenceULP(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat), 0);
  EXPECT_TRUE(itk::Math::FloatAlmostEqual(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat));

  floatRepresentationdx1.asFloat = 0.0;
  floatRepresentationdx2.asFloat = 67329.234 - 67329.242;
  EXPECT_TRUE(itk::Math::FloatAlmostEqual(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat, 4, 0.1));

  floatRepresentationdx1.asFloat = 1e-8;
  floatRepresentationdx2.asFloat = -1e-8;
  EXPECT_GE(itk::Math::FloatDifferenceULP(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat), 0);

  floatRepresentationdx1.asFloat = -1e-8;
  floatRepresentationdx2.asFloat = 1e-8;
  EXPECT_LE(itk::Math::FloatDifferenceULP(floatRepresentationdx1.asFloat, floatRepresentationdx2.asFloat), 0);
}

TEST(itkMath, EqualsOperations)
{
  constexpr signed char sc{ -1 };
  constexpr auto        uc = static_cast<unsigned char>(-1);
  TestIntegersAreSame(sc, uc);

  constexpr int  si{ -1 };
  constexpr auto ul = static_cast<unsigned long>(-1);
  TestIntegersAreSame(si, ul);

  constexpr auto ui = static_cast<unsigned int>(-1);
  TestIntegersAreSame(si, ui);

  constexpr auto ust = static_cast<size_t>(-1);
  TestIntegersAreSame(si, ust);

  constexpr float  f{ -1.0f };
  constexpr double d{ 1.01 };

  EXPECT_FALSE(itk::Math::AlmostEquals(f, d));
  EXPECT_FALSE(itk::Math::AlmostEquals(d, f));
  EXPECT_TRUE(itk::Math::AlmostEquals(f, sc));
  EXPECT_TRUE(itk::Math::AlmostEquals(sc, f));
  EXPECT_TRUE(itk::Math::AlmostEquals(1.0, 1.0f));
  EXPECT_TRUE(itk::Math::AlmostEquals(1.1, 1.1f));
  EXPECT_TRUE(itk::Math::AlmostEquals(1, 1.0));
  EXPECT_FALSE(itk::Math::AlmostEquals(2.0, 1.0));
  EXPECT_FALSE(itk::Math::AlmostEquals(1, 2));

  // ExactlyEquals constexpr tests
  static_assert(!itk::Math::ExactlyEquals(f, d));
  static_assert(!itk::Math::ExactlyEquals(d, f));
  static_assert(!itk::Math::NotExactlyEquals(1.0f, 1.0));
  static_assert(!itk::Math::NotExactlyEquals(1.0, 1.0f));

  FloatRepresentationD oneExact;
  FloatRepresentationD oneAlmost;
  oneExact.asFloat = 1.0;
  oneAlmost.asFloat = 1.0;
  oneAlmost.asInt += 1;

  EXPECT_TRUE(itk::Math::AlmostEquals(oneExact.asFloat, oneAlmost.asFloat));
  EXPECT_FALSE(itk::Math::ExactlyEquals(oneExact.asFloat, oneAlmost.asFloat));

  // Complex comparisons
  constexpr std::complex<double> z1Double(1.1, 2.1);
  constexpr std::complex<float>  z1Float(1.1f, 2.1f);
  EXPECT_TRUE(itk::Math::AlmostEquals(z1Double, z1Float));

#if !defined(ITK_LEGACY_REMOVE)
  constexpr std::complex<double> z2Double(1.0, 3.0);
  constexpr std::complex<int>    z2Int(1, 3);
  EXPECT_TRUE(itk::Math::AlmostEquals(z2Double, z2Int));
#endif

  FloatRepresentationD z1AlmostRealPart;
  z1AlmostRealPart.asFloat = z1Double.real();
  z1AlmostRealPart.asInt += 1;
  const std::complex<double> z1DoubleAlmost(z1AlmostRealPart.asFloat, z1Double.imag());
  EXPECT_TRUE(itk::Math::AlmostEquals(z1Double, z1DoubleAlmost));

  constexpr std::complex<double> z3Double(0.123, 0);
  constexpr float                r3Float{ 0.123 };
  constexpr double               r3Double{ 0.123 };
  EXPECT_TRUE(itk::Math::AlmostEquals(z3Double, r3Double));
  EXPECT_TRUE(itk::Math::AlmostEquals(z3Double, r3Float));

  constexpr std::complex<float> z4Float(0.123, 0);
  FloatRepresentationF          r4FloatAlmost;
  r4FloatAlmost.asFloat = z4Float.real();
  r4FloatAlmost.asInt += 1;
  EXPECT_TRUE(itk::Math::AlmostEquals(z4Float, r4FloatAlmost.asFloat));
  EXPECT_TRUE(itk::Math::AlmostEquals(z3Double, r4FloatAlmost.asFloat));
}

TEST(itkMath, IsPrime)
{
  ExerciseIsPrime<unsigned short>();
  ExerciseIsPrime<unsigned int>();
  ExerciseIsPrime<unsigned long>();
  ExerciseIsPrime<unsigned long long>();
}

TEST(itkMath, GreatestPrimeFactor)
{
  ExerciseGreatestPrimeFactor<unsigned short>();
  ExerciseGreatestPrimeFactor<unsigned int>();
  ExerciseGreatestPrimeFactor<unsigned long>();
  ExerciseGreatestPrimeFactor<unsigned long long>();
}

TEST(itkMath, Abs)
{
  // static_assert tests from an original file
  static_assert(itk::Math::Absolute(false) == false);
  static_assert(itk::Math::Absolute(true) == true);
  static_assert(itk::Math::Absolute(static_cast<unsigned char>(5)) == 5);
  static_assert(itk::Math::Absolute(static_cast<unsigned short>(5)) == 5);
  static_assert(itk::Math::Absolute(static_cast<unsigned int>(5)) == 5);
  static_assert(itk::Math::Absolute(static_cast<unsigned long>(5)) == 5);
  static_assert(itk::Math::Absolute(static_cast<unsigned long long>(5)) == 5);
  static_assert(itk::Math::Absolute(static_cast<signed char>(-5)) == 5);
  static_assert(itk::Math::Absolute(static_cast<signed char>(-128)) == 128);
  static_assert(itk::Math::Absolute(static_cast<short>(-5)) == 5);
  static_assert(itk::Math::Absolute<int>(-5) == 5u);
  static_assert(itk::Math::Absolute<long>(-5L) == 5ul);
  static_assert(itk::Math::Absolute<long long>(-5LL) == 5ull);
  static_assert(itk::Math::Absolute<double>(-5.0) == 5.0);
  static_assert(itk::Math::Absolute<float>(-5.0f) == 5.0f);

  constexpr auto cf = std::complex<float>(-3, -4);
  EXPECT_EQ(itk::Math::Absolute(cf), 5);
  constexpr auto cd = std::complex<double>(-3, -4);
  EXPECT_EQ(itk::Math::Absolute(cd), 5);
  constexpr auto cld = std::complex<long double>(-3, -4);
  EXPECT_EQ(itk::Math::Absolute(cld), 5);

  EXPECT_EQ(itk::Math::Absolute(false), false);
  EXPECT_EQ(itk::Math::Absolute(true), true);
  EXPECT_EQ(itk::Math::Absolute(static_cast<unsigned char>(5)), 5);
  EXPECT_EQ(itk::Math::Absolute(static_cast<signed char>(-5)), 5);
  EXPECT_EQ(itk::Math::Absolute(static_cast<signed char>(-128)), 128);
  EXPECT_EQ(itk::Math::Absolute(static_cast<short>(-5)), 5);
  EXPECT_EQ(itk::Math::Absolute<int>(-5), 5u);
  EXPECT_EQ(itk::Math::Absolute<long>(-5L), 5ul);
  EXPECT_EQ(itk::Math::Absolute<long long>(-5LL), 5ull);
  EXPECT_EQ(itk::Math::Absolute<double>(-5.0), 5.0);
  EXPECT_EQ(itk::Math::Absolute<float>(-5.0f), 5.0f);
  EXPECT_EQ(itk::Math::Absolute(-5), 5);
}


// Checks that `Math::Absolute(-0.0)` returns `+0.0`.
TEST(itkMath, AbsoluteMinusZeroReturnsPlusZero)
{
  constexpr auto expectAbsoluteReturnsPlusZero = [](const auto minusZero) {
    // Sanity check beforehand: assert that `minusZero` has indeed a minus sign.
    ASSERT_TRUE(std::signbit(minusZero));

    const auto absoluteValue = itk::Math::Absolute(minusZero);
    EXPECT_FALSE(std::signbit(absoluteValue));
    EXPECT_EQ(absoluteValue, 0);
  };

  // Check both float and double:
  expectAbsoluteReturnsPlusZero(-0.0F);
  expectAbsoluteReturnsPlusZero(-0.0);
}


TEST(itkMath, ConstexprTests)
{
  static_assert(itk::Math::ExactlyEquals(5, 5));
  static_assert(itk::Math::NotExactlyEquals(5, 6));
  static_assert(itk::Math::IsPrime(2));
  static_assert(itk::Math::IsPrime(3));
  static_assert(!itk::Math::IsPrime(4));
  static_assert(itk::Math::IsPrime(7));
  static_assert(itk::Math::IsPrime<uint32_t>(97));
  static_assert(!itk::Math::IsPrime<uint32_t>(100));
  static_assert(!itk::Math::IsPrime<uint8_t>(1));
  static_assert(itk::Math::IsPrime<uint32_t>(4294967291), "Failed on the largest uint32_t IsPrime computation");
  static_assert(itk::Math::GreatestPrimeFactor(10) == 5);
  static_assert(itk::Math::GreatestPrimeFactor(13) == 13);

#if !defined(WIN32) && __cplusplus >= 202302L
  static_assert(itk::Math::Round<int>(1.5) == 2);
  static_assert(itk::Math::Round<int>(2.5) == 3);
  static_assert(itk::Math::Floor<int>(1.9) == 1);
  static_assert(itk::Math::Floor<int>(-1.1) == -2);
  static_assert(itk::Math::Ceil<int>(1.1) == 2);
  static_assert(itk::Math::Ceil<int>(-1.9) == -1);
#endif
}

TEST(itkMath, RoundFloorCeil)
{
  EXPECT_EQ(itk::Math::Round<int>(1.5), 2);
  EXPECT_EQ(itk::Math::Round<int>(2.5), 3);
  EXPECT_EQ(itk::Math::Floor<int>(1.9), 1);
  EXPECT_EQ(itk::Math::Floor<int>(-1.1), -2);
  EXPECT_EQ(itk::Math::Ceil<int>(1.1), 2);
  EXPECT_EQ(itk::Math::Ceil<int>(-1.9), -1);
}
