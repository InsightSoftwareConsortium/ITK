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

#ifndef itkStaticAssert_h
#define itkStaticAssert_h

#include "itkMacro.h"

#if !defined(ITK_LEGACY_REMOVE)
/** Static assertion.
 * This macro will use C++11 \c static_assert() keyword.
 * \param expr compile-time expression to check
 * \param str  string literal that will appear as compiler error if \c expr is \c false.
 * \ingroup ITKCommon
 */
#  define itkStaticAssert(expr, str) static_assert(expr, str)
#else
#  define itkStaticAssert(expr, str) Use C++ 11 static_assert directly
#endif

// TODO: remove this file entirely in the future (e.g. with ITKv6)

#endif // itkStaticAssert_h
