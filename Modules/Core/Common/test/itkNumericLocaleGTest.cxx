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

#include "itkNumericLocale.h"
#include <gtest/gtest.h>
#include <locale.h>
#include <cstring>
#include <cstdlib>

// Test that NumericLocale temporarily sets LC_NUMERIC to "C"
TEST(NumericLocale, TemporarilySetsToCLocale)
{
  // Save initial locale
  const char * initialLocale = setlocale(LC_NUMERIC, nullptr);
  ASSERT_NE(initialLocale, nullptr);
  std::string savedInitialLocale(initialLocale);

  {
    // Create NumericLocale - should set to "C"
    itk::NumericLocale numericLocale;

    const char * currentLocale = setlocale(LC_NUMERIC, nullptr);
    ASSERT_NE(currentLocale, nullptr);
    EXPECT_STREQ(currentLocale, "C");
  }

  // After NumericLocale destroyed, locale should be restored
  const char * restoredLocale = setlocale(LC_NUMERIC, nullptr);
  ASSERT_NE(restoredLocale, nullptr);
  EXPECT_STREQ(restoredLocale, savedInitialLocale.c_str());
}

// Test that numeric parsing works correctly with NumericLocale
TEST(NumericLocale, ParsesFloatsWithDotDecimalSeparator)
{
  {
    itk::NumericLocale numericLocale;

    // Parse floating-point number with dot as decimal separator
    double value = std::strtod("3.14159", nullptr);
    EXPECT_DOUBLE_EQ(value, 3.14159);

    value = std::strtod("0.878906", nullptr);
    EXPECT_DOUBLE_EQ(value, 0.878906);

    value = std::strtod("2.5", nullptr);
    EXPECT_DOUBLE_EQ(value, 2.5);
  }
}

// Test that NumericLocale can be nested
TEST(NumericLocale, SupportsNesting)
{
  const char * initialLocale = setlocale(LC_NUMERIC, nullptr);
  ASSERT_NE(initialLocale, nullptr);
  std::string savedInitialLocale(initialLocale);

  {
    itk::NumericLocale outerLocale;
    EXPECT_STREQ(setlocale(LC_NUMERIC, nullptr), "C");

    {
      itk::NumericLocale innerLocale;
      EXPECT_STREQ(setlocale(LC_NUMERIC, nullptr), "C");
    }

    // After inner destroyed, still in C locale
    EXPECT_STREQ(setlocale(LC_NUMERIC, nullptr), "C");
  }

  // After both destroyed, locale is restored
  EXPECT_STREQ(setlocale(LC_NUMERIC, nullptr), savedInitialLocale.c_str());
}

// Test with a different locale if available (optional test)
TEST(NumericLocale, WorksWithDifferentInitialLocale)
{
  // Try to set a locale with comma as decimal separator
  const char * germanLocale = setlocale(LC_NUMERIC, "de_DE.UTF-8");

  if (germanLocale != nullptr)
  {
    // Verify we're in German locale (comma separator)
    const char * currentLocale = setlocale(LC_NUMERIC, nullptr);
    ASSERT_NE(currentLocale, nullptr);
    std::string savedLocale(currentLocale);

    // Without NumericLocale, parsing with dot would fail in German locale
    // (This test verifies the problem we're fixing)
    double valueWithoutFix = std::strtod("0.878906", nullptr);
    // In de_DE locale, this would parse as 0.0 (stops at dot)
    EXPECT_EQ(valueWithoutFix, 0.0);

    {
      // With NumericLocale, parsing should work correctly
      itk::NumericLocale numericLocale;

      double valueWithFix = std::strtod("0.878906", nullptr);
      EXPECT_DOUBLE_EQ(valueWithFix, 0.878906);

      double value2 = std::strtod("3.5", nullptr);
      EXPECT_DOUBLE_EQ(value2, 3.5);
    }

    // After NumericLocale destroyed, we should be back in German locale
    EXPECT_STREQ(setlocale(LC_NUMERIC, nullptr), savedLocale.c_str());

    // Restore to C locale for other tests
    setlocale(LC_NUMERIC, "C");
  }
  else
  {
    // de_DE.UTF-8 locale not available, skip this test
    GTEST_SKIP() << "de_DE.UTF-8 locale not available on this system";
  }
}

// Test that multiple sequential uses work correctly
TEST(NumericLocale, SupportsSequentialUses)
{
  for (int i = 0; i < 3; ++i)
  {
    itk::NumericLocale numericLocale;
    double             value = std::strtod("1.5", nullptr);
    EXPECT_DOUBLE_EQ(value, 1.5);
  }
}

// Test basic RAII behavior
TEST(NumericLocale, BasicRAII)
{
  const char * initialLocale = setlocale(LC_NUMERIC, nullptr);
  ASSERT_NE(initialLocale, nullptr);
  std::string savedInitialLocale(initialLocale);

  // Create and immediately destroy
  {
    itk::NumericLocale temp;
  }

  // Locale should be restored
  EXPECT_STREQ(setlocale(LC_NUMERIC, nullptr), savedInitialLocale.c_str());
}
