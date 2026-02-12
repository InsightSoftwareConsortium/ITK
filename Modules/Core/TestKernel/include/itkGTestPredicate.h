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

#ifndef itkGTestPredicate_h
#define itkGTestPredicate_h


#include "gtest/gtest.h"

#include "itkNumericTraits.h"
#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"
#include <cmath>


/** A custom GTest macro for comparing 2 itk array-like objects. This
 * verifies that the root mean squares error between the two array-like
 * objects doesn't exceed the given error.
 */
#define ITK_EXPECT_VECTOR_NEAR(val1, val2, rmsError) \
  EXPECT_PRED_FORMAT3(itk::GTest::Predicate::VectorDoubleRMSPredFormat, val1, val2, rmsError)

/** A custom GTest macro for verifying a value in a MetaDataDictionary.
 */
#define ITK_EXPECT_METADATA_VALUE(metaDict, key, knownValue) \
  EXPECT_PRED_FORMAT3(itk::GTest::Predicate::CheckMetaDataPredFormat, metaDict, key, knownValue)


/** \namespace itk::GTest::Predicate
 *  \brief The Predicate namespace contains functions used to
 *  implement custom GTest Predicate-Formatters.
 */
namespace itk::GTest::Predicate
{

/** Implements GTest Predicate Formatter for ITK_EXPECT_METADATA_VALUE
 * macro.
 */
template <typename T>
inline ::testing::AssertionResult
CheckMetaDataPredFormat(const char *              metaDictExpr,
                        const char *              keyExpr,
                        const char *              knownValueExpr,
                        itk::MetaDataDictionary & metaDict,
                        const std::string &       key,
                        const T &                 knownValue)
{
  T exposedValue{};

#if defined ITK_FUTURE_LEGACY_REMOVE
  static_assert(
    !std::is_same_v<itk::Array<char>, T>,
    "Should not use the ambiguous 'char' stored in meta data, because it is not-cross platform consistent.");
  static_assert(
    !std::is_same_v<char, T>,
    "Should not use the ambiguous 'char' stored in meta data, because it is not-cross platform consistent.");
  if (!itk::ExposeMetaData<T>(metaDict, key, exposedValue))
  {
    return ::testing::AssertionFailure() << "Failure ExposeMetaData for key '" << key << "' (" << keyExpr << ") in "
                                         << metaDictExpr;
  }
#else
  if constexpr (std::is_same_v<itk::Array<char>, T>)
  {
    if (!itk::ExposeMetaData<itk::Array<char>>(metaDict, key, exposedValue))
    {
      if constexpr (std::is_signed_v<char>)
      {
        itk::Array<signed char> temp_value{};
        if (!itk::ExposeMetaData<itk::Array<signed char>>(metaDict, key, temp_value))
        {
          return ::testing::AssertionFailure()
                 << "Failure ExposeMetaData '" << key << "' (" << keyExpr << ") in " << metaDictExpr;
        }
        exposedValue = temp_value;
      }
      else
      {
        itk::Array<unsigned char> temp_value{};
        if (!itk::ExposeMetaData<itk::Array<unsigned char>>(metaDict, key, temp_value))
        {
          return ::testing::AssertionFailure()
                 << "Failure ExposeMetaData '" << key << "' (" << keyExpr << ") in " << metaDictExpr;
        }
        exposedValue = temp_value;
      }
    }
  }
  else if constexpr (std::is_same_v<char, T>)
  {
    if (!itk::ExposeMetaData<char>(metaDict, key, exposedValue))
    {
      if constexpr (std::is_signed_v<char>)
      {
        signed char temp_value{};
        if (!itk::ExposeMetaData<signed char>(metaDict, key, temp_value))
        {
          return ::testing::AssertionFailure()
                 << "Failure ExposeMetaData '" << key << "' (" << keyExpr << ") in " << metaDictExpr;
        }
        exposedValue = static_cast<T>(temp_value);
      }
      else
      {
        unsigned char temp_value{};
        if (!itk::ExposeMetaData<unsigned char>(metaDict, key, temp_value))
        {
          return ::testing::AssertionFailure()
                 << "Failure ExposeMetaData '" << key << "' (" << keyExpr << ") in " << metaDictExpr;
        }
        exposedValue = static_cast<T>(temp_value);
      }
    }
  }
  else if (!itk::ExposeMetaData<T>(metaDict, key, exposedValue))
  {
    return ::testing::AssertionFailure() << "Failure ExposeMetaData for key '" << key << "' (" << keyExpr << ") in "
                                         << metaDictExpr;
  }
#endif

  bool match = false;
  if constexpr (std::is_floating_point_v<T>)
  {
    match = itk::Math::AlmostEquals(exposedValue, knownValue);
  }
  else
  {
    match = (exposedValue == knownValue);
  }

  if (match)
  {
    return ::testing::AssertionSuccess();
  }

  ::testing::AssertionResult failure = ::testing::AssertionFailure();
  failure << "Incorrect meta value read in for " << keyExpr << " ('" << key << "') in " << metaDictExpr << "\n"
          << "  Actual: " << exposedValue << "\n"
          << "  Expected: " << knownValue << " (" << knownValueExpr << ")\n";
  failure << "========================================\n";
  std::stringstream ss;
  metaDict.Print(ss);
  failure << ss.str();
  failure << "========================================";
  return failure;
}


/** Implements GTest Predicate Formatter for ITK_EXPECT_VECTOR_NEAR
 * macro. This macro and formatter work with any combination of ITK
 * array-like objects which are supported by itk::NumericTraits. The
 * arrays must be the same length, and the root mean squares error is
 * used to check if the arrays are "near" each other.
 */
template <typename T1, typename T2>
inline ::testing::AssertionResult
VectorDoubleRMSPredFormat(const char * expr1,
                          const char * expr2,
                          const char * rmsErrorExpr,
                          const T1 &   val1,
                          const T2 &   val2,
                          double       rmsError)
{
  const size_t val1Size = itk::NumericTraits<T1>::GetLength(val1);
  const size_t val2Size = itk::NumericTraits<T2>::GetLength(val2);
  if (val1Size != val2Size)
  {
    return ::testing::AssertionFailure() << "The size of " << expr1 << " and " << expr2 << " different, where\n"
                                         << expr1 << " evaluates to " << val1 << ",\n"
                                         << expr2 << " evaluates to " << val2 << '.';
  }
  double total = 0.0;
  for (unsigned int i = 0; i < val1Size; ++i)
  {
    const double temp = (val1[i] - val2[i]);
    total += temp * temp;
  }
  const double rms = std::sqrt(total / val1Size);
  if (rms <= rmsError)
  {
    return ::testing::AssertionSuccess();
  }

  return ::testing::AssertionFailure() << "The RMS difference between " << expr1 << " and " << expr2 << " is " << rms
                                       << ",\n  which exceeds " << rmsErrorExpr << ", where\n"
                                       << expr1 << " evaluates to " << val1 << ",\n"
                                       << expr2 << " evaluates to " << val2 << ", and\n"
                                       << rmsErrorExpr << " evaluates to " << rmsError << '.';
}

} // namespace itk::GTest::Predicate
// end namespace GTest

#endif // itkGTestPredicate_h
