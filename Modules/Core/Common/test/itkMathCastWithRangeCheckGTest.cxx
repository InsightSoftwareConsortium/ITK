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
#include "itkGTest.h"

#include <iostream>


namespace
{


// we validate the results against just converting the numbers to
// doubles. This will work for all integers less then 32-bit but is
// not always exact with larger integers.
template <typename T1, typename T2>
bool
DoCastWithRangeCheckTestVerify(const T2 value, const T1 = 0)
{
  T1 ret;
  // tying to convert T2 to T1
  try
  {
    ret = itk::Math::CastWithRangeCheck<T1>(value);
    // value should match
    if (itk::Math::NotExactlyEquals(static_cast<double>(ret), static_cast<double>(value)))
    {
      std::cout << "casting error with input value: " << static_cast<typename itk::NumericTraits<T2>::PrintType>(value)
                << " output value: " << static_cast<typename itk::NumericTraits<T1>::PrintType>(ret) << std::endl;
      return false;
    }
  }
  catch (...)
  {
    // conversion should result in some overflow problem
    auto retCheck = static_cast<T1>(value);
    if (itk::Math::ExactlyEquals(static_cast<double>(retCheck), static_cast<double>(value)))
    {
      std::cout << "unexpected exception with value: " << value << std::endl;
      return false;
    }
  }


  return true;
}


template <typename T1, typename T2>
bool
DoCastWithRangeCheckTestExulstive(const T1 * = nullptr, const T2 * = nullptr)
{
  // test convert T2 to T1
  bool pass = true;
  for (T2 i = itk::NumericTraits<T2>::NonpositiveMin(); i != itk::NumericTraits<T2>::max(); ++i)
  {
    pass &= DoCastWithRangeCheckTestVerify<T1, T2>(i);
  }

  return pass;
}

template <typename T1, typename T2>
bool
DoCastWithRangeCheckTest(const T1 * = nullptr, const T2 * = nullptr)
{
  constexpr int minus_one{ -1 };

  // test convert T2 to T1
  bool pass = true;
  pass &= DoCastWithRangeCheckTestVerify<T1, T2>(itk::NumericTraits<T2>::NonpositiveMin());
  pass &= DoCastWithRangeCheckTestVerify<T1, T2>(itk::NumericTraits<T2>::max());
  pass &= DoCastWithRangeCheckTestVerify<T1, T2>(T2{});
  pass &= DoCastWithRangeCheckTestVerify<T1, T2>(itk::NumericTraits<T2>::OneValue());
  pass &= DoCastWithRangeCheckTestVerify<T1, T2>(static_cast<T2>(itk::NumericTraits<T2>::OneValue() * minus_one));

  return pass;
}

template <typename T1>
bool
DoCastWithRangeCheckTestForTypes(const T1 * = nullptr)
{
  // call method for all type to be converted to type T1
  bool pass = true;

  pass &= DoCastWithRangeCheckTest<T1, unsigned char>();
  pass &= DoCastWithRangeCheckTest<T1, signed char>();
  pass &= DoCastWithRangeCheckTest<T1, unsigned short>();
  pass &= DoCastWithRangeCheckTest<T1, short>();
  pass &= DoCastWithRangeCheckTest<T1, unsigned int>();
  pass &= DoCastWithRangeCheckTest<T1, int>();
  pass &= DoCastWithRangeCheckTest<T1, unsigned long>();
  pass &= DoCastWithRangeCheckTest<T1, long>();
#ifdef ITK_USE_LONG_LONG
  pass &= DoCastWithRangeCheckTest<T1, unsigned long long>();
  pass &= DoCastWithRangeCheckTest<T1, long long>();
#endif
  return pass;
}

} // end namespace

TEST(MathCastWithRangeCheck, OverflowThrowsException)
{
  // Wrap in lambda to avoid comma in template args confusing the macro parser
  EXPECT_THROW(
    ([&]() { itk::Math::CastWithRangeCheck<short, int>(static_cast<int>(itk::NumericTraits<short>::max()) + 10); }()),
    itk::RangeError);
  std::cout << "caught exception as expected" << std::endl;
}

TEST(MathCastWithRangeCheck, ExhaustiveSmallTypes)
{
  bool pass = true;
  pass &= DoCastWithRangeCheckTestExulstive<signed char, unsigned char>();
  pass &= DoCastWithRangeCheckTestExulstive<unsigned char, signed char>();
  pass &= DoCastWithRangeCheckTestExulstive<unsigned char, short>();
  pass &= DoCastWithRangeCheckTestExulstive<signed char, short>();
  pass &= DoCastWithRangeCheckTestExulstive<unsigned int, signed char>();
  EXPECT_TRUE(pass);
}

TEST(MathCastWithRangeCheck, AllTypesToUnsignedChar) { EXPECT_TRUE(DoCastWithRangeCheckTestForTypes<unsigned char>()); }

TEST(MathCastWithRangeCheck, AllTypesToSignedChar) { EXPECT_TRUE(DoCastWithRangeCheckTestForTypes<signed char>()); }

TEST(MathCastWithRangeCheck, AllTypesToUnsignedShort)
{
  EXPECT_TRUE(DoCastWithRangeCheckTestForTypes<unsigned short>());
}

TEST(MathCastWithRangeCheck, AllTypesToShort)
{
  const bool pass = DoCastWithRangeCheckTestForTypes<short>();
  EXPECT_TRUE(pass);
}

TEST(MathCastWithRangeCheck, AllTypesToUnsignedInt) { EXPECT_TRUE(DoCastWithRangeCheckTestForTypes<unsigned int>()); }

TEST(MathCastWithRangeCheck, AllTypesToInt)
{
  const bool pass = DoCastWithRangeCheckTestForTypes<int>();
  EXPECT_TRUE(pass);
}

TEST(MathCastWithRangeCheck, AllTypesToUnsignedLong) { EXPECT_TRUE(DoCastWithRangeCheckTestForTypes<unsigned long>()); }

TEST(MathCastWithRangeCheck, AllTypesToLong)
{
  const bool pass = DoCastWithRangeCheckTestForTypes<long>();
  EXPECT_TRUE(pass);
}
