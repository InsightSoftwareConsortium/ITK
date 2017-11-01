/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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
#include <cmath>


/** A custom GTest macro for comparing 2 itk array-like objects. This
 * verifies that the root mean squares error between the two array-like
 * objects doesn't exceed the given error.
 */
#define EXPECT_VECTOR_NEAR(val1, val2, rmsError)       \
  EXPECT_PRED_FORMAT3(itk::GTest::Predicate::VectorDoubleRMSPredFormat, \
                      val1, val2, rmsError)


namespace itk
{

namespace GTest
{

/** \namespace itk::GTest::Predicate
 *  \brief The Predicate namespace contains functions used to
 *  implement custom GTest Predicate-Formatters.
 */
namespace Predicate
{

/** Implements GTest Predicate Formatter for EXPECT_VECTOR_NEAR
 * macro. This macro and formatter work with any combination of ITK
 * array-like objects which are supported by itk::NumericTraits. The
 * arrays must be the same length, and the root mean squares error is
 * used to check if the arrays are "near" each other.
 */
template<typename T1, typename T2>
inline ::testing::AssertionResult VectorDoubleRMSPredFormat(const char* expr1,
                                                            const char* expr2,
                                                            const char* rmsErrorExpr,
                                                            const T1 &val1,
                                                            const T2 &val2,
                                                            double rmsError)
{
  const size_t val1Size = itk::NumericTraits<T1>::GetLength(val1);
  const size_t val2Size = itk::NumericTraits<T2>::GetLength(val2);
  if ( val1Size != val2Size )
    {
    return ::testing::AssertionFailure()
      << "The size of " << expr1 << " and " << expr2
      << " different, where\n"
      << expr1 << " evaluates to " << val1 << ",\n"
      << expr2 << " evaluates to " << val2 << ".";

    }
  double total = 0.0;
  for ( unsigned int i = 0; i < val1Size; ++i )
    {
    const double temp = (val1[i]-val2[i]);
    total += temp*temp;
    }
  const double rms = std::sqrt(total/val1Size);
  if (rms <= rmsError)
    {
    return ::testing::AssertionSuccess();
    }

  return ::testing::AssertionFailure()
    << "The RMS difference between " << expr1 << " and " << expr2
    << " is " << rms << ",\n  which exceeds " << rmsErrorExpr << ", where\n"
    << expr1 << " evaluates to " << val1 << ",\n"
    << expr2 << " evaluates to " << val2 << ", and\n"
    << rmsErrorExpr << " evaluates to " << rmsError << ".";
}

} // end namespace Predicate
} // end namespace GTest
} // end namespace itk

#endif // itkGTestPredicate_h
