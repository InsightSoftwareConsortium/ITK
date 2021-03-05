/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkMetaProgrammingLibrary_h
#define itkMetaProgrammingLibrary_h

#include "itkMacro.h"
#include "itkSmartPointer.h"

namespace itk
{

/// \cond HIDE_META_PROGRAMMING
namespace mpl
{

/**\defgroup MetaProgrammingLibrary Meta Programming Library
 * This module contains definitions aimed at metaprogramming.
 * They are mainly codes borrowed or inspired from C++11 or indirectly boost,
 * with ITK UpperCamelCase naming policy.
 */

/** borrowed from `<type_traits>`.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
struct TrueType
{
  using ValueType = bool;
  using Type = TrueType;

  static constexpr ValueType Value = true;
                             operator ValueType() { return Value; }
};

/** borrowed from `<type_traits>`.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
struct FalseType
{
  using ValueType = bool;
  using Type = FalseType;
  static constexpr ValueType Value = false;
                             operator ValueType() { return Value; }
};

/** MPL \c if control-statement.
 * \tparam VP Boolean predicate
 * \tparam T1 Type returned if the predicate is true
 * \tparam T2 Type returned if the predicate is false
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <bool VP, typename T1, typename T2>
struct If;
/// \cond SPECIALIZATION_IMPLEMENTATION
template <typename T1, typename T2>
struct If<true, T1, T2>
{
  using Type = T1;
};
template <typename T1, typename T2>
struct If<false, T1, T2>
{
  using Type = T2;
};
/// \endcond

/** MPL \c OR operator on constants.
 * \tparam VF1 First boolean expression
 * \tparam VF2 Second boolean expression
 * \tparam VF3 Third (optional) boolean expression
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <bool VF1, bool VF2, bool VF3 = false>
struct OrC : TrueType
{};
/// \cond SPECIALIZATION_IMPLEMENTATION
template <>
struct OrC<false, false, false> : FalseType
{};
/// \endcond
/** MPL \c OR operator on types.
 * \tparam TF1 First boolean type
 * \tparam TF2 Second boolean type
 * \tparam VF3 Third (optional) boolean type
 *
 * This \em overload automatically fetches \c TF1, \c TF2 and \c TF3 values.
 * However, beware, it won't work with standard C++ traits or boost traits.
 * Indeed, this overload expects the \em value to follow UpperCamelCase ITK
 * naming policy instead of the standard snake_case policy.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <typename TF1, typename TF2, typename TF3 = FalseType>
struct Or : OrC<TF1::Value, TF2::Value, TF3::Value>
{
  using Type = typename OrC<TF1::Value, TF2::Value, TF3::Value>::Type;
};

/** MPL \c AND operator on constants.
 * \tparam VF1 First boolean expression
 * \tparam VF2 Second boolean expression
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <bool VF1, bool VF2>
struct AndC : FalseType
{};
/// \cond SPECIALIZATION_IMPLEMENTATION
template <>
struct AndC<true, true> : TrueType
{};
/// \endcond
/** MPL \c AND operator on types.
 * \tparam TF1 First boolean type
 * \tparam TF2 Second boolean type
 *
 * This \em overload automatically fetches \c TF1 and \c TF2 values.
 * However, beware, it won't work with standard C++ traits or boost traits.
 * Indeed, this overload expects the \em value to follow UpperCamelCase ITK
 * naming policy instead of the standard snake_case policy.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <typename TF1, typename TF2>
struct And : AndC<TF1::Value, TF2::Value>
{
  using Type = typename AndC<TF1::Value, TF2::Value>::Type;
};

/** MPL \c XOR operator on constants.
 * \tparam VF1 First boolean expression
 * \tparam VF2 Second boolean expression
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <bool VF1, bool VF2>
struct XorC : FalseType
{};
/// \cond SPECIALIZATION_IMPLEMENTATION
template <>
struct XorC<true, false> : TrueType
{};
template <>
struct XorC<false, true> : TrueType
{};
/// \endcond
/** MPL \c XOR operator on types.
 * \tparam TF1 First boolean type
 * \tparam TF2 Second boolean type
 *
 * This \em overload automatically fetches \c TF1 and \c TF2 values.
 * However, beware, it won't work with standard C++ traits or boost traits.
 * Indeed, this overload expects the \em value to follow UpperCamelCase ITK
 * naming policy instead of the standard snake_case policy.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <typename TF1, typename TF2>
struct Xor : XorC<TF1::Value, TF2::Value>
{
  using Type = typename XorC<TF1::Value, TF2::Value>::Type;
};

/** MPL \c NOT operator on constants.
 * \tparam VF boolean expression to negate
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <bool VF>
struct NotC : FalseType
{};
/// \cond SPECIALIZATION_IMPLEMENTATION
template <>
struct NotC<false> : TrueType
{};
template <>
struct NotC<true> : FalseType
{};
/// \endcond
/** MPL \c NOT operator on types.
 * \tparam TF Second boolean type
 *
 * This \em overload automatically fetches \c TF value.
 * However, beware, it won't work with standard C++ traits or boost traits.
 * Indeed, this overload expects the \em value to follow UpperCamelCase ITK
 * naming policy instead of the standard snake_case policy.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <typename TF>
struct Not : NotC<TF::Value>
{
  using Type = typename NotC<TF::Value>::Type;
};

/** MPL type trait to check if type is a SmartPointer.
 */
template <typename T>
struct IsSmartPointer : FalseType
{};


/// \cond SPECIALIZATION_IMPLEMENTATION
template <typename T>
struct IsSmartPointer<SmartPointer<T>> : TrueType
{};

template <typename T>
struct IsSmartPointer<const SmartPointer<T>> : TrueType
{};
/// \endcond

/** MPL relational type trait to check if a static_cast conversion
 * exists.
 *
 * Identifies if "static_cast<TToType>(TFromType)" can be done.
 */
template <typename TFromType, typename TToType>
using is_static_castable = std::integral_constant<bool,
                                                  std::is_constructible<TToType, TFromType>::value ||
                                                    std::is_convertible<TFromType, TToType>::value>;
} // namespace mpl


// TrueType and FalseType have moved to itk::mpl.
// Expect itk::TrueType and itk::False type to be deprecated.
using mpl::TrueType;
using mpl::FalseType;

/// \endcond
} // namespace itk
#endif // itkMetaProgrammingLibrary_h
