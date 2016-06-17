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
#ifndef itkIsBaseOf_h
#define itkIsBaseOf_h

#include "itkIsConvertible.h"
#include "itkIsSame.h"

namespace itk {
/// \cond HIDE_META_PROGRAMMING
namespace mpl {
/** Traits that emulates \c std::is_base_of<>.
 * \tparam TBase base type
 * \tparam TDerived derived type
 * \return (in \c Value) whether \c TDerived inherits (publicly) from \c TBase
 * (directly, or indirectly)
 * \author The definition provided follows the code snippet available in Andrei
 * Alexandrescu's <em>Modern C++ Design</em>.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <typename TBase, typename TDerived>
struct IsBaseOf
  {
  static ITK_CONSTEXPR_VAR bool Value
    =    IsConvertible<const TDerived*, const TBase*>::Value
    && ! IsSame<const TBase*, const void*>::Value;
  };
} // itk::mpl namespace

/// \endcond
} // itk namespace

#endif // itkIsBaseOf_h
