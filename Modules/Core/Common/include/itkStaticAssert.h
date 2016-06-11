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

#ifndef itkStaticAssert_h
#define itkStaticAssert_h

#include "itkMacro.h"

#if ITK_COMPILER_CXX_STATIC_ASSERT
//  Use the C++11 static_assert if available

/** Static assertion.
 * This macro will either emulate static assertions on the current compiler or
 * use C++11 \c static_assert() keyword.
 * \param expr compile-time expression to check
 * \param str  string literal that will appear (C++11, GCC 4.3+) as compiler
 * error if \c expr is \c false.
 * \ingroup ITKCommon
 */
#   define itkStaticAssert(expr, str) static_assert(expr, str)
#elif defined(__GNUC__) && ((__GNUC__ * 100) + __GNUC_MINOR__ ) >= 403 && !defined(__clang__) && !defined( __INTEL_COMPILER )
//  GCC 4.3 is enough for this trick
//  But it restricts the static assertion to non global contexts (-> functions)
#   define itkStaticAssert(expr,str)                                  \
      ({extern int __attribute__((error(str))) StaticAssertFailure(); \
       ((void)((expr) ? 0: StaticAssertFailure()), 0);                \
       })
#else
//  Usual trick (boost, clang, ...), but it will loose the error message on the
//  way
#   define ITK_JOIN(X,Y)     ITK_DO_JOIN(X,Y)
#   define ITK_DO_JOIN(X,Y)  ITK_DO_JOIN2(X,Y)
#   define ITK_DO_JOIN2(X,Y) X##Y

namespace itk {
/// \cond HIDE_META_PROGRAMMING
/** Internal class to emulate static assertions of pre-C++11 compilers.
 * \sa \c itkStaticAssert
 * \ingroup ITKCommon
 */
template <bool V> struct StaticAssertFailure;
template <> struct StaticAssertFailure<true>{};
/// \endcond
} // itk namespace

#   define itkStaticAssert(expr,str) \
      enum { ITK_JOIN(static_assert_typedef, __LINE__) = sizeof(itk::StaticAssertFailure<((expr) == 0 ? false : true)>) };
#endif

#endif // itkStaticAssert_h
