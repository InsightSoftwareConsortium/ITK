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

// First include the header file to be tested:
#include "itkNumberToString.h"
#include <gtest/gtest.h>

namespace
{

template <typename TValue>
void
Test_all_digits()
{
  const itk::NumberToString<TValue> numberToString{};

  for (auto i = 9; i >= 0; --i)
  {
    EXPECT_EQ(numberToString(i), std::to_string(i));
  }
}


template <typename TValue>
void
Test_non_finite_special_floating_point_values()
{
  using NumericLimitsType = std::numeric_limits<TValue>;
  const itk::NumberToString<TValue> numberToString{};

  EXPECT_EQ(numberToString(NumericLimitsType::quiet_NaN()), "NaN");
  EXPECT_EQ(numberToString(NumericLimitsType::infinity()), "Infinity");
  EXPECT_EQ(numberToString(-NumericLimitsType::infinity()), "-Infinity");
}


template <typename TValue>
void
Test_round_trip_of_finite_numeric_limits()
{
  using NumericLimitsType = std::numeric_limits<TValue>;
  const itk::NumberToString<TValue> numberToString{};

  for (const TValue expectedValue : { NumericLimitsType::lowest(),
                                      NumericLimitsType::epsilon(),
                                      NumericLimitsType::round_error(),
                                      NumericLimitsType::denorm_min(),
                                      NumericLimitsType::min(),
                                      NumericLimitsType::max() })
  {
    std::istringstream inputStream{ numberToString(expectedValue) };
    EXPECT_FALSE(inputStream.eof());
    TValue actualValue;
    inputStream >> actualValue;
    EXPECT_TRUE(inputStream.eof());
    EXPECT_EQ(actualValue, expectedValue);
  }
}


template <typename TValue>
void
Test_decimal_notation_supports_up_to_twentyone_digits()
{
  const itk::NumberToString<TValue> numberToString{};

  for (int8_t exponent{ 20 }; exponent > 0; --exponent)
  {
    const auto power_of_ten = std::pow(TValue{ 10 }, static_cast<TValue>(exponent));

    // Test +/- 10 ^ exponent
    EXPECT_EQ(numberToString(power_of_ten), '1' + std::string(exponent, '0'));
    EXPECT_EQ(numberToString(-power_of_ten), "-1" + std::string(exponent, '0'));
  }

  for (int8_t exponent{ -6 }; exponent < 0; ++exponent)
  {
    const auto power_of_ten = std::pow(TValue{ 10 }, static_cast<TValue>(exponent));

    // Test +/- 10 ^ exponent
    EXPECT_EQ(numberToString(power_of_ten), "0." + std::string(-1 - exponent, '0') + '1');
    EXPECT_EQ(numberToString(-power_of_ten), "-0." + std::string(-1 - exponent, '0') + '1');
  }
}


template <typename TValue>
void
Test_default_specialization_of_NumberToString()
{
  using NumericLimitsType = std::numeric_limits<TValue>;

  for (const TValue value : { NumericLimitsType::lowest(),
                              NumericLimitsType::denorm_min(),
                              TValue(),
                              NumericLimitsType::epsilon(),
                              NumericLimitsType::min(),
                              NumericLimitsType::max(),
                              NumericLimitsType::infinity(),
                              NumericLimitsType::quiet_NaN() })
  {
    // Expect the same string from the default specialization `NumberToString<>` as from the TValue specific
    // `NumberToString<TValue>`.
    EXPECT_EQ(itk::NumberToString<>()(value), itk::NumberToString<TValue>()(value));
  }
}

template <typename TValue>
void
Test_ConvertNumberToString()
{
  using NumericLimitsType = std::numeric_limits<TValue>;

  for (const TValue value : { NumericLimitsType::lowest(),
                              NumericLimitsType::denorm_min(),
                              TValue(),
                              NumericLimitsType::epsilon(),
                              NumericLimitsType::min(),
                              NumericLimitsType::max(),
                              NumericLimitsType::infinity(),
                              NumericLimitsType::quiet_NaN() })
  {
    // Expect the same string from `ConvertNumberToString` as from `NumberToString<TValue>`.
    EXPECT_EQ(itk::ConvertNumberToString(value), itk::NumberToString<TValue>()(value));
  }
}

} // namespace


// Tests NumberToString for 0 to 9.
TEST(NumberToString, SupportsAllDigits)
{
  Test_all_digits<int>();
  Test_all_digits<float>();
  Test_all_digits<double>();
}


// Tests that the function object returns a unique string for both positive and negative zero.
TEST(NumberToString, HasUniqueZeroString)
{
  const std::string expectedZeroString = "0";

  EXPECT_EQ(itk::NumberToString<int>{}(0), expectedZeroString);
  EXPECT_EQ(itk::NumberToString<float>{}(+0.0f), expectedZeroString);
  EXPECT_EQ(itk::NumberToString<float>{}(-0.0f), expectedZeroString);
  EXPECT_EQ(itk::NumberToString<double>{}(+0.0), expectedZeroString);
  EXPECT_EQ(itk::NumberToString<double>{}(-0.0), expectedZeroString);
}


TEST(NumberToString, NonFiniteSpecialFloatingPointValues)
{
  Test_non_finite_special_floating_point_values<float>();
  Test_non_finite_special_floating_point_values<double>();
}


TEST(NumberToString, RoundTripOfFiniteFloatingPointNumericLimits)
{
  Test_round_trip_of_finite_numeric_limits<float>();
  Test_round_trip_of_finite_numeric_limits<double>();
}


TEST(NumberToString, DecimalNotationUpTo21Digits)
{
  Test_decimal_notation_supports_up_to_twentyone_digits<float>();
  Test_decimal_notation_supports_up_to_twentyone_digits<double>();
}


TEST(NumberToString, DefaultSpecialization)
{
  Test_default_specialization_of_NumberToString<int8_t>();
  Test_default_specialization_of_NumberToString<uint8_t>();
  Test_default_specialization_of_NumberToString<intmax_t>();
  Test_default_specialization_of_NumberToString<uintmax_t>();
  Test_default_specialization_of_NumberToString<float>();
  Test_default_specialization_of_NumberToString<double>();
}

TEST(NumberToString, ConvertNumberToString)
{
  Test_ConvertNumberToString<int8_t>();
  Test_ConvertNumberToString<uint8_t>();
  Test_ConvertNumberToString<intmax_t>();
  Test_ConvertNumberToString<uintmax_t>();
  Test_ConvertNumberToString<float>();
  Test_ConvertNumberToString<double>();
}
