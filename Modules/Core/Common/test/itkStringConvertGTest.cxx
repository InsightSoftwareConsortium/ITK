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

#include "itkGTest.h"
#include "itkStringConvert.h"
#include "itkMacro.h"

#include <cstdint>
#include <limits>
#include <string>


// ----- StringToInt32 -----

TEST(StringTools, StringToInt32_Valid)
{
  EXPECT_EQ(itk::StringToInt32("0", "ctx"), 0);
  EXPECT_EQ(itk::StringToInt32("42", "ctx"), 42);
  EXPECT_EQ(itk::StringToInt32("-7", "ctx"), -7);
  EXPECT_EQ(itk::StringToInt32("  17", "ctx"), 17); // leading whitespace ok per std::stoll
  EXPECT_EQ(itk::StringToInt32("2147483647", "ctx"), std::numeric_limits<std::int32_t>::max());
  EXPECT_EQ(itk::StringToInt32("-2147483648", "ctx"), std::numeric_limits<std::int32_t>::min());
}

TEST(StringTools, StringToInt32_Empty_Throws)
{
  EXPECT_THROW(itk::StringToInt32("", "TestContext"), itk::ExceptionObject);
}

TEST(StringTools, StringToInt32_NonNumeric_Throws)
{
  EXPECT_THROW(itk::StringToInt32("abc", "TestContext"), itk::ExceptionObject);
}

TEST(StringTools, StringToInt32_OutOfRange_Throws)
{
  // Above INT32_MAX (2^31 - 1 = 2147483647)
  EXPECT_THROW(itk::StringToInt32("2147483648", "OverflowCtx"), itk::ExceptionObject);
  // Below INT32_MIN (-2^31 = -2147483648)
  EXPECT_THROW(itk::StringToInt32("-2147483649", "OverflowCtx"), itk::ExceptionObject);
  // Beyond even int64
  EXPECT_THROW(itk::StringToInt32("999999999999999999999", "OverflowCtx"), itk::ExceptionObject);
}

TEST(StringTools, StringToInt32_ContextAndInputAppearInMessage)
{
  try
  {
    itk::StringToInt32("not-a-number", "MyParserField");
    FAIL() << "Expected itk::ExceptionObject";
  }
  catch (const itk::ExceptionObject & e)
  {
    const std::string what = e.GetDescription();
    EXPECT_NE(what.find("MyParserField"), std::string::npos);
    EXPECT_NE(what.find("not-a-number"), std::string::npos);
    EXPECT_NE(what.find("int32_t"), std::string::npos);
  }
}

TEST(StringTools, StringToInt32_NullContextDoesNotCrash)
{
  EXPECT_THROW(itk::StringToInt32("xyz", nullptr), itk::ExceptionObject);
}

TEST(StringTools, StringToInt32_LongInputTruncatedInMessage)
{
  const std::string huge(10000, 'q');
  try
  {
    itk::StringToInt32(huge, "HugeCtx");
    FAIL() << "Expected itk::ExceptionObject";
  }
  catch (const itk::ExceptionObject & e)
  {
    const std::string what = e.GetDescription();
    EXPECT_NE(what.find("truncated"), std::string::npos);
    EXPECT_LT(what.size(), 4096u);
  }
}

TEST(StringTools, StringToInt32_TruncationLimitIs512Chars)
{
  // Locks in the exact maxQuotedInputLength contract: at-or-below the
  // limit is quoted in full; above is truncated to exactly the limit
  // with the original length reported in chars.
  const std::size_t    limit = 512;
  constexpr const char sentinel = 'q';
  const std::string    atLimit(limit, sentinel);
  const std::string    overLimit(limit + 50, sentinel);

  try
  {
    itk::StringToInt32(atLimit, "AtLimitCtx");
    FAIL() << "Expected itk::ExceptionObject";
  }
  catch (const itk::ExceptionObject & e)
  {
    const std::string what = e.GetDescription();
    EXPECT_EQ(what.find("truncated"), std::string::npos);
    EXPECT_NE(what.find(atLimit), std::string::npos);
  }

  try
  {
    itk::StringToInt32(overLimit, "OverLimitCtx");
    FAIL() << "Expected itk::ExceptionObject";
  }
  catch (const itk::ExceptionObject & e)
  {
    const std::string what = e.GetDescription();
    EXPECT_NE(what.find("truncated"), std::string::npos);
    EXPECT_NE(what.find(std::to_string(overLimit.size()) + " chars"), std::string::npos);
    const std::string quotedPrefix(limit, sentinel);
    EXPECT_NE(what.find(quotedPrefix), std::string::npos);
    const std::string oneOver(limit + 1, sentinel);
    EXPECT_EQ(what.find(oneOver), std::string::npos);
  }
}


// ----- StringToInt64 -----

TEST(StringTools, StringToInt64_Valid)
{
  EXPECT_EQ(itk::StringToInt64("0", "ctx"), std::int64_t{ 0 });
  EXPECT_EQ(itk::StringToInt64("123456", "ctx"), std::int64_t{ 123456 });
  EXPECT_EQ(itk::StringToInt64("-1", "ctx"), std::int64_t{ -1 });
  // Value > INT32_MAX — not representable as int32 but trivially fits in int64.
  EXPECT_EQ(itk::StringToInt64("9999999999", "ctx"), std::int64_t{ 9999999999LL });
  EXPECT_EQ(itk::StringToInt64("9223372036854775807", "ctx"), std::numeric_limits<std::int64_t>::max());
  EXPECT_EQ(itk::StringToInt64("-9223372036854775808", "ctx"), std::numeric_limits<std::int64_t>::min());
}

TEST(StringTools, StringToInt64_Bad_Throws)
{
  EXPECT_THROW(itk::StringToInt64("", "ctx"), itk::ExceptionObject);
  EXPECT_THROW(itk::StringToInt64("hello", "ctx"), itk::ExceptionObject);
}

TEST(StringTools, StringToInt64_OutOfRange_Throws)
{
  // 2^63 = 9223372036854775808 — smallest value above INT64_MAX
  EXPECT_THROW(itk::StringToInt64("9223372036854775808", "ctx"), itk::ExceptionObject);
}


// ----- StringToUInt32 -----

TEST(StringTools, StringToUInt32_Valid)
{
  EXPECT_EQ(itk::StringToUInt32("0", "ctx"), std::uint32_t{ 0 });
  EXPECT_EQ(itk::StringToUInt32("42", "ctx"), std::uint32_t{ 42 });
  EXPECT_EQ(itk::StringToUInt32("4294967295", "ctx"), std::numeric_limits<std::uint32_t>::max());
}

TEST(StringTools, StringToUInt32_RejectsLeadingMinus)
{
  EXPECT_THROW(itk::StringToUInt32("-1", "ctx"), itk::ExceptionObject);
  EXPECT_THROW(itk::StringToUInt32("  -7", "ctx"), itk::ExceptionObject);
}

TEST(StringTools, StringToUInt32_Bad_Throws)
{
  EXPECT_THROW(itk::StringToUInt32("", "ctx"), itk::ExceptionObject);
  EXPECT_THROW(itk::StringToUInt32("xyz", "ctx"), itk::ExceptionObject);
}

TEST(StringTools, StringToUInt32_OutOfRange_Throws)
{
  // 2^32 = 4294967296 — smallest value above UINT32_MAX
  EXPECT_THROW(itk::StringToUInt32("4294967296", "ctx"), itk::ExceptionObject);
  EXPECT_THROW(itk::StringToUInt32("99999999999999999999999", "ctx"), itk::ExceptionObject);
}


// ----- StringToUInt64 -----

TEST(StringTools, StringToUInt64_Valid)
{
  EXPECT_EQ(itk::StringToUInt64("0", "ctx"), std::uint64_t{ 0 });
  EXPECT_EQ(itk::StringToUInt64("4294967295", "ctx"), std::uint64_t{ 4294967295ULL });
  // Value > 2^32: cannot fit in `unsigned long` on 64-bit Windows (LLP64).
  // Locks in the cross-platform width contract that motivated the
  // fixed-width return type.
  EXPECT_EQ(itk::StringToUInt64("9999999999", "ctx"), std::uint64_t{ 9999999999ULL });
  EXPECT_EQ(itk::StringToUInt64("18446744073709551615", "ctx"), std::numeric_limits<std::uint64_t>::max());
}

TEST(StringTools, StringToUInt64_RejectsLeadingMinus)
{
  EXPECT_THROW(itk::StringToUInt64("-1", "ctx"), itk::ExceptionObject);
  EXPECT_THROW(itk::StringToUInt64("  -7", "ctx"), itk::ExceptionObject);
}

TEST(StringTools, StringToUInt64_Bad_Throws)
{
  EXPECT_THROW(itk::StringToUInt64("", "ctx"), itk::ExceptionObject);
  EXPECT_THROW(itk::StringToUInt64("xyz", "ctx"), itk::ExceptionObject);
}

TEST(StringTools, StringToUInt64_OutOfRange_Throws)
{
  // 2^64 = 18446744073709551616 — smallest value above UINT64_MAX
  EXPECT_THROW(itk::StringToUInt64("18446744073709551616", "ctx"), itk::ExceptionObject);
  EXPECT_THROW(itk::StringToUInt64("99999999999999999999999", "ctx"), itk::ExceptionObject);
}


// ----- StringToDouble -----

TEST(StringTools, StringToDouble_Valid)
{
  EXPECT_DOUBLE_EQ(itk::StringToDouble("3.14", "ctx"), 3.14);
  EXPECT_DOUBLE_EQ(itk::StringToDouble("-0.5", "ctx"), -0.5);
  EXPECT_DOUBLE_EQ(itk::StringToDouble("1e3", "ctx"), 1000.0);
}

TEST(StringTools, StringToDouble_Bad_Throws)
{
  EXPECT_THROW(itk::StringToDouble("", "ctx"), itk::ExceptionObject);
  EXPECT_THROW(itk::StringToDouble("abc", "ctx"), itk::ExceptionObject);
}


// ----- StringToFloat -----

TEST(StringTools, StringToFloat_Valid)
{
  EXPECT_FLOAT_EQ(itk::StringToFloat("2.5", "ctx"), 2.5f);
  EXPECT_FLOAT_EQ(itk::StringToFloat("-1.25", "ctx"), -1.25f);
}

TEST(StringTools, StringToFloat_Bad_Throws)
{
  EXPECT_THROW(itk::StringToFloat("", "ctx"), itk::ExceptionObject);
  EXPECT_THROW(itk::StringToFloat("nope", "ctx"), itk::ExceptionObject);
}
