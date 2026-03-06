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

#include "itkMath.h"
#include "itkIndex.h"
#include "itkGTest.h"

#include <iostream>
#include <string_view>

// Helper from original itkMathRoundTest
namespace
{
bool
math_test_helper(const std::string_view str, bool test)
{
  if (!test)
  {
    std::cout << "test (" << str << ") failed" << std::endl;
  }
  return test;
}
} // namespace

#define RoundTestHelperMacro(rndname, input, output)                                                         \
  if (rndname((input)) != (output))                                                                          \
  {                                                                                                          \
    std::cout << "Failure! " << #rndname << '(' << static_cast<int>(input) << ") expected "                  \
              << static_cast<int>(output) << " but got " << static_cast<int>(rndname((input))) << std::endl; \
    ok = false;                                                                                              \
  }                                                                                                          \
  ITK_MACROEND_NOOP_STATEMENT

namespace
{
template <typename T>
bool
TemplatedRoundTest()
{
  bool ok = true;

  constexpr unsigned int numberOfElements{ 15 };

  // input data for rounding methods
  constexpr float input[]{ -8.4999f, -8.50f,  -8.5001f, 8.4999f, 8.50f,    8.5001f, -9.4999f, -9.50f,
                           -9.5001f, 9.4999f, 9.50f,    9.5001f, -0.4999f, -.50f,   -.5001f };

  T roundOutput[] = { -8, -8, -9, 8, 9, 9, -9, -9, -10, 9, 10, 10, 0, 0, -1 };

  constexpr T halftoevenOutput[]{ -8, -8, -9, 8, 8, 9, -9, -10, -10, 9, 10, 10, 0, 0, -1 };

  T * halfupOutput = roundOutput;

  ////////
  // input data for floor and ceil methods
  constexpr float fcinput[]{ 8.0f,    8.9999f, 8.0001f,  -8.0f,    -8.9999f, -8.0001f, 9.0f,    9.9999f,
                             9.0001f, -9.0f,   -9.9999f, -9.0001f, -1.0f,    -0.9999f, -1.0001f };

  constexpr T floorOutput[]{ 8, 8, 8, -8, -9, -9, 9, 9, 9, -9, -10, -10, -1, -1, -2 };

  constexpr T ceilOutput[]{ 8, 9, 9, -8, -8, -8, 9, 10, 10, -9, -9, -9, -1, 0, -1 };

  // Round
  for (unsigned int i = 0; i < numberOfElements; ++i)
  {
    RoundTestHelperMacro(itk::Math::Round<T>, static_cast<float>(input[i]), roundOutput[i]);
    RoundTestHelperMacro(itk::Math::Round<T>, static_cast<double>(input[i]), roundOutput[i]);
  }

  // RoundHalfIntegerToEven
  for (unsigned int i = 0; i < numberOfElements; ++i)
  {
    RoundTestHelperMacro(itk::Math::RoundHalfIntegerToEven<T>, static_cast<float>(input[i]), halftoevenOutput[i]);
    RoundTestHelperMacro(itk::Math::RoundHalfIntegerToEven<T>, static_cast<double>(input[i]), halftoevenOutput[i]);
  }

  // RoundHalfIntegerUp
  for (unsigned int i = 0; i < numberOfElements; ++i)
  {
    RoundTestHelperMacro(itk::Math::RoundHalfIntegerUp<T>, static_cast<float>(input[i]), halfupOutput[i]);
    RoundTestHelperMacro(itk::Math::RoundHalfIntegerUp<T>, static_cast<double>(input[i]), halfupOutput[i]);
  }

  // Floor
  for (unsigned int i = 0; i < numberOfElements; ++i)
  {
    RoundTestHelperMacro(itk::Math::Floor<T>, static_cast<float>(fcinput[i]), floorOutput[i]);
    RoundTestHelperMacro(itk::Math::Floor<T>, static_cast<double>(fcinput[i]), floorOutput[i]);
  }

  // Ceil
  for (unsigned int i = 0; i < numberOfElements; ++i)
  {
    RoundTestHelperMacro(itk::Math::Ceil<T>, static_cast<float>(fcinput[i]), ceilOutput[i]);
    RoundTestHelperMacro(itk::Math::Ceil<T>, static_cast<double>(fcinput[i]), ceilOutput[i]);
  }

  return ok;
}

} // namespace

TEST(MathRound, IndexValueType)
{
  bool ok = true;

  using IndexValueType = itk::Index<3>::IndexValueType;

  ok &= math_test_helper("rnd(-8.4999) == -8", itk::Math::Round<IndexValueType>(-8.4999) == -8);
  ok &= math_test_helper("rnd(-8.4999f) == -8", itk::Math::Round<IndexValueType>(-8.4999f) == -8);
  ok &= math_test_helper("rnd(-8.50 == -8", itk::Math::Round<IndexValueType>(-8.50) == -8);
  ok &= math_test_helper("rnd(-8.50f) == -8", itk::Math::Round<IndexValueType>(-8.50f) == -8);
  ok &= math_test_helper("rnd(-8.5001) == -9", itk::Math::Round<IndexValueType>(-8.5001) == -9);
  ok &= math_test_helper("rnd(-8.5001f) == -9", itk::Math::Round<IndexValueType>(-8.5001f) == -9);
  ok &= math_test_helper("rnd(8.4999) == 8", itk::Math::Round<IndexValueType>(8.4999) == 8);
  ok &= math_test_helper("rnd(8.4999f) == 8", itk::Math::Round<IndexValueType>(8.4999f) == 8);
  ok &= math_test_helper("rnd(8.50) == 9", itk::Math::Round<IndexValueType>(8.50) == 9);
  ok &= math_test_helper("rnd(8.50f) == 9", itk::Math::Round<IndexValueType>(8.50f) == 9);
  ok &= math_test_helper("rnd(8.5001) == 9", itk::Math::Round<IndexValueType>(8.5001) == 9);
  ok &= math_test_helper("rnd(8.5001f) == 9", itk::Math::Round<IndexValueType>(8.5001f) == 9);

  ok &=
    math_test_helper("rnd_halfinttoeven(-8.50) == -8", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-8.50) == -8);
  ok &= math_test_helper("rnd_halfinttoeven(8.50) == 8", itk::Math::RoundHalfIntegerToEven<IndexValueType>(8.50) == 8);
  ok &= math_test_helper("rnd_halfinttoeven(-9.50) == -10",
                         itk::Math::RoundHalfIntegerToEven<IndexValueType>(-9.50) == -10);
  ok &=
    math_test_helper("rnd_halfinttoeven(9.50) == 10", itk::Math::RoundHalfIntegerToEven<IndexValueType>(9.50) == 10);

  ok &= math_test_helper("rnd_halfintup(-8.50) == -8", itk::Math::RoundHalfIntegerUp<IndexValueType>(-8.50) == -8);
  ok &= math_test_helper("rnd_halfintup(8.50) == 9", itk::Math::RoundHalfIntegerUp<IndexValueType>(8.50) == 9);
  ok &= math_test_helper("rnd_halfintup(-9.50) == -9 ", itk::Math::RoundHalfIntegerUp<IndexValueType>(-9.50) == -9);
  ok &= math_test_helper("rnd_halfintup(9.50) == 10", itk::Math::RoundHalfIntegerUp<IndexValueType>(9.50) == 10);

  ok &= math_test_helper("floor(8.0) == 8", itk::Math::Floor<IndexValueType>(8.0) == 8);
  ok &= math_test_helper("floor(-8.9999) == -9", itk::Math::Floor<IndexValueType>(-8.9999) == -9);
  ok &= math_test_helper("floor(-9.9999) == -10", itk::Math::Floor<IndexValueType>(-9.9999) == -10);

  ok &= math_test_helper("ceil(8.9999) == 9", itk::Math::Ceil<IndexValueType>(8.9999) == 9);
  ok &= math_test_helper("ceil(-8.0001) == -8", itk::Math::Ceil<IndexValueType>(-8.0001) == -8);
  ok &= math_test_helper("ceil(9.9999) == 10", itk::Math::Ceil<IndexValueType>(9.9999) == 10);

  EXPECT_TRUE(ok);
}

TEST(MathRound, CharType)
{
  std::cout << "Testing char type" << std::endl;
  EXPECT_TRUE(TemplatedRoundTest<signed char>());
}

TEST(MathRound, ShortType)
{
  std::cout << "Testing short type" << std::endl;
  EXPECT_TRUE(TemplatedRoundTest<short>());
}

TEST(MathRound, IntType)
{
  std::cout << "Testing int type" << std::endl;
  EXPECT_TRUE(TemplatedRoundTest<int>());
}

TEST(MathRound, LongType)
{
  std::cout << "Testing long type" << std::endl;
  EXPECT_TRUE(TemplatedRoundTest<long>());
}

TEST(MathRound, VxlInt64Type)
{
  std::cout << "Testing vxl_int_64 type" << std::endl;
  EXPECT_TRUE(TemplatedRoundTest<vxl_int_64>());
}
