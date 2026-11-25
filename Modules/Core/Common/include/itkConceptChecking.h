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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkConceptChecking_h
#define itkConceptChecking_h

#include "itkPixelTraits.h"
#include "itkNumericTraits.h"
#include <iostream>
#include <type_traits>

/** \brief Compile-time check that a template argument satisfies an itk::Concept.
 *
 * \p conceptType is parenthesized, so it may contain commas. Prefer using a concept directly:
 * \code
 * static_assert(Concept::HasNumericTraits<InputPixelType>::value, "InputPixelType must have NumericTraits");
 * \endcode
 */
#define itkConceptMacro(name, conceptType)                                    \
  static_assert(::itk::Concept::Detail::IsSatisfied<void conceptType>::value, \
                "ITK concept check failed: " #name " " #conceptType)

namespace itk::Concept
{
namespace Detail
{
/** Unwraps the parenthesized concept passed to itkConceptMacro, as `void(TConcept)`. */
template <typename TSignature>
struct IsSatisfied;
template <typename TConcept>
struct IsSatisfied<void(TConcept)> : TConcept
{};

template <typename TVoid, template <typename...> typename TExpression, typename... TArgs>
struct IsValidExpressionImpl : std::false_type
{};
template <template <typename...> typename TExpression, typename... TArgs>
struct IsValidExpressionImpl<std::void_t<TExpression<TArgs...>>, TExpression, TArgs...> : std::true_type
{};
/** True when the type alias TExpression<TArgs...> is well-formed (detection idiom). */
template <template <typename...> typename TExpression, typename... TArgs>
inline constexpr bool IsValidExpression = IsValidExpressionImpl<void, TExpression, TArgs...>::value;

template <typename TBoolean>
using RequireBoolean = std::enable_if_t<std::is_convertible_v<TBoolean, bool>>;
template <typename T>
T &
Lvalue(); // Shorthand for std::declval<T &>(), only used in unevaluated operands.

template <typename T1, typename T2>
using StaticCast = decltype(static_cast<T2>(Lvalue<T1>()));
template <typename T1, typename T2>
using LessThan = std::void_t<RequireBoolean<decltype(Lvalue<T1>() < Lvalue<T2>())>,
                             RequireBoolean<decltype(Lvalue<T1>() <= Lvalue<T2>())>>;
template <typename T1, typename T2>
using GreaterThan = std::void_t<RequireBoolean<decltype(Lvalue<T1>() > Lvalue<T2>())>,
                                RequireBoolean<decltype(Lvalue<T1>() >= Lvalue<T2>())>>;
ITK_GCC_PRAGMA_PUSH
ITK_GCC_SUPPRESS_Wfloat_equal
template <typename T1, typename T2>
using Equality = std::void_t<RequireBoolean<decltype(Lvalue<T1>() == Lvalue<T2>())>,
                             RequireBoolean<decltype(Lvalue<T1>() != Lvalue<T2>())>>;
ITK_GCC_PRAGMA_POP
template <typename T1, typename T2, typename T3>
using Additive = std::void_t<decltype(Lvalue<T3>() = static_cast<T3>(Lvalue<const T1>() + Lvalue<const T2>())),
                             decltype(Lvalue<T3>() = static_cast<T3>(Lvalue<const T1>() - Lvalue<const T2>()))>;
template <typename T1, typename T2>
using AdditiveAndAssign =
  std::void_t<decltype(Lvalue<T2>() += Lvalue<const T1>()), decltype(Lvalue<T2>() -= Lvalue<const T1>())>;
template <typename T1, typename T2, typename T3>
using Multiply = decltype(Lvalue<T3>() = static_cast<T3>(Lvalue<const T1>() * Lvalue<const T2>()));
template <typename T1, typename T2>
using MultiplyAndAssign = decltype(Lvalue<T2>() *= Lvalue<const T1>());
template <typename T1, typename T2, typename T3>
using Division = decltype(Lvalue<T3>() = static_cast<T3>(Lvalue<const T1>() / Lvalue<const T2>()));
template <typename T1, typename T2>
using DivisionAndAssign = decltype(Lvalue<T2>() /= Lvalue<const T1>());
template <typename T1, typename T2, typename T3>
using Bitwise = std::void_t<decltype(Lvalue<T3>() = static_cast<T3>(Lvalue<const T1>() & Lvalue<const T2>())),
                            decltype(Lvalue<T3>() = static_cast<T3>(Lvalue<const T1>() | Lvalue<const T2>())),
                            decltype(Lvalue<T3>() = static_cast<T3>(Lvalue<const T1>() ^ Lvalue<const T2>())),
                            decltype(Lvalue<T3>() &= static_cast<T3>(Lvalue<const T2>())),
                            decltype(Lvalue<T3>() |= static_cast<T3>(Lvalue<const T2>())),
                            decltype(Lvalue<T3>() ^= static_cast<T3>(Lvalue<const T2>()))>;
template <typename T1, typename T2, typename T3>
using Bracket = decltype(Lvalue<T3>() = static_cast<T3>(Lvalue<const T1>()[Lvalue<const T2>()]));
template <typename T>
using Not = decltype(Lvalue<T>() = !Lvalue<T>());
template <typename T>
using IncrementDecrement =
  std::void_t<decltype(Lvalue<T>()++), decltype(Lvalue<T>()--), decltype(++Lvalue<T>()), decltype(--Lvalue<T>())>;
template <typename T>
using OStreamWrite = decltype(Lvalue<std::ostream>() << Lvalue<T>());
template <typename T, typename TTraits = NumericTraits<T>>
using NumericTraitsMembers = std::void_t<typename TTraits::ValueType,
                                         typename TTraits::PrintType,
                                         typename TTraits::AbsType,
                                         typename TTraits::AccumulateType,
                                         typename TTraits::RealType,
                                         typename TTraits::ScalarRealType,
                                         typename TTraits::FloatType,
                                         decltype(Lvalue<T>() = TTraits::NonpositiveMin(Lvalue<const T>())),
                                         decltype(Lvalue<T>() = TTraits::ZeroValue(Lvalue<const T>())),
                                         decltype(Lvalue<T>() = TTraits::OneValue(Lvalue<const T>())),
                                         RequireBoolean<decltype(TTraits::IsPositive(Lvalue<const T>()))>,
                                         RequireBoolean<decltype(TTraits::IsNonpositive(Lvalue<const T>()))>,
                                         RequireBoolean<decltype(TTraits::IsNegative(Lvalue<const T>()))>,
                                         RequireBoolean<decltype(TTraits::IsNonnegative(Lvalue<const T>()))>>;
/** The primary NumericTraits template derives from an unspecialized std::numeric_limits<T>. */
template <typename T>
inline constexpr bool IsPrimaryNumericTraits =
  std::is_base_of_v<std::numeric_limits<T>, NumericTraits<T>> && !std::numeric_limits<T>::is_specialized;
template <typename T>
using PixelTraitsMembers =
  std::void_t<typename PixelTraits<T>::ValueType,
              std::enable_if_t<std::is_convertible_v<decltype(PixelTraits<T>::Dimension), unsigned int>>>;
template <typename T>
using ValueTypeMember = typename T::ValueType;
template <typename T1, typename T2>
using JoinTraitsMember = typename JoinTraits<T1, T2>::ValueType;
template <typename T>
inline constexpr bool IsExact = std::numeric_limits<typename NumericTraits<T>::ValueType>::is_exact;
} // namespace Detail

/** Each concept is a std::bool_constant telling whether its requirement holds. */
template <typename T>
struct DefaultConstructible : std::is_default_constructible<T>
{};
template <typename T>
struct CopyConstructible : std::is_copy_constructible<T>
{};
/** T1 is convertible to T2 by static_cast. */
template <typename T1, typename T2>
struct Convertible : std::bool_constant<Detail::IsValidExpression<Detail::StaticCast, T1, T2>>
{};
template <typename T>
struct Assignable : std::bool_constant<std::is_copy_constructible_v<T> && std::is_copy_assignable_v<T>>
{};
/** T1 < T2 and T1 <= T2. */
template <typename T1, typename T2 = T1>
struct LessThanComparable : std::bool_constant<Detail::IsValidExpression<Detail::LessThan, T1, T2>>
{};
/** T1 > T2 and T1 >= T2. */
template <typename T1, typename T2 = T1>
struct GreaterThanComparable : std::bool_constant<Detail::IsValidExpression<Detail::GreaterThan, T1, T2>>
{};
/** T1 == T2 and T1 != T2. */
template <typename T1, typename T2 = T1>
struct EqualityComparable : std::bool_constant<Detail::IsValidExpression<Detail::Equality, T1, T2>>
{};
/** T1 <, >, <=, >=, ==, != T2. */
template <typename T1, typename T2 = T1>
struct Comparable
  : std::bool_constant<LessThanComparable<T1, T2>::value && GreaterThanComparable<T1, T2>::value &&
                       EqualityComparable<T1, T2>::value>
{};
/** T3 = T1 + T2 and T3 = T1 - T2. */
template <typename T1, typename T2 = T1, typename T3 = T1>
struct AdditiveOperators : std::bool_constant<Detail::IsValidExpression<Detail::Additive, T1, T2, T3>>
{};
/** T2 += T1 and T2 -= T1. */
template <typename T1, typename T2 = T1>
struct AdditiveAndAssignOperators : std::bool_constant<Detail::IsValidExpression<Detail::AdditiveAndAssign, T1, T2>>
{};
/** T3 = T1 * T2. */
template <typename T1, typename T2 = T1, typename T3 = T1>
struct MultiplyOperator : std::bool_constant<Detail::IsValidExpression<Detail::Multiply, T1, T2, T3>>
{};
/** T2 *= T1. */
template <typename T1, typename T2 = T1>
struct MultiplyAndAssignOperator : std::bool_constant<Detail::IsValidExpression<Detail::MultiplyAndAssign, T1, T2>>
{};
/** T3 = T1 / T2. */
template <typename T1, typename T2 = T1, typename T3 = T1>
struct DivisionOperators : std::bool_constant<Detail::IsValidExpression<Detail::Division, T1, T2, T3>>
{};
/** T2 /= T1. */
template <typename T1, typename T2 = T1>
struct DivisionAndAssignOperators : std::bool_constant<Detail::IsValidExpression<Detail::DivisionAndAssign, T1, T2>>
{};
/** T3 = T1 op T2 and T3 op= T2, for op in &, |, ^. */
template <typename T1, typename T2 = T1, typename T3 = T1>
struct BitwiseOperators : std::bool_constant<Detail::IsValidExpression<Detail::Bitwise, T1, T2, T3>>
{};
/** T3 = T1[T2]. */
template <typename T1, typename T2 = T1, typename T3 = T1>
struct BracketOperator : std::bool_constant<Detail::IsValidExpression<Detail::Bracket, T1, T2, T3>>
{};
template <typename T>
struct NotOperator : std::bool_constant<Detail::IsValidExpression<Detail::Not, T>>
{};
template <typename T>
struct IncrementDecrementOperators : std::bool_constant<Detail::IsValidExpression<Detail::IncrementDecrement, T>>
{};
template <typename T>
struct OStreamWritable : std::bool_constant<Detail::IsValidExpression<Detail::OStreamWrite, T>>
{};
template <typename T>
struct Signed : std::bool_constant<NumericTraits<T>::is_signed>
{};
template <typename T1, typename T2>
struct SameType : std::is_same<T1, T2>
{};
template <unsigned int D1, unsigned int D2>
struct SameDimension : std::bool_constant<D1 == D2>
{};
/** NumericTraits<T> is specialized and provides the members ITK relies on. */
template <typename T>
struct HasNumericTraits
  : std::bool_constant<!Detail::IsPrimaryNumericTraits<T> && std::is_default_constructible_v<T> &&
                       Detail::IsValidExpression<Detail::NumericTraitsMembers, T>>
{};
template <typename T>
struct HasPixelTraits : std::bool_constant<Detail::IsValidExpression<Detail::PixelTraitsMembers, T>>
{};
template <typename T>
struct HasValueType : std::bool_constant<Detail::IsValidExpression<Detail::ValueTypeMember, T>>
{};
template <typename T>
struct HasZero : std::is_default_constructible<T>
{};
template <typename T1, typename T2>
struct HasJoinTraits : std::bool_constant<Detail::IsValidExpression<Detail::JoinTraitsMember, T1, T2>>
{};
/** D2 is D1 or D1 - 1. */
template <unsigned int D1, unsigned int D2>
struct SameDimensionOrMinusOne : std::bool_constant<D2 == D1 || D2 == D1 - 1>
{};
/** D2 is D1, D1 - 1 or D1 - 2. */
template <unsigned int D1, unsigned int D2>
struct SameDimensionOrMinusOneOrTwo : std::bool_constant<D2 == D1 || D2 == D1 - 1 || D2 == D1 - 2>
{};
template <typename T>
struct IsInteger : std::is_integral<T>
{};
/** T is unsigned, according to NumericTraits. */
template <typename T>
struct IsUnsignedInteger : std::bool_constant<!NumericTraits<T>::is_signed>
{};
template <typename T>
struct IsNonInteger : std::bool_constant<!std::is_integral_v<T>>
{};
/** The NumericTraits ValueType of T is inexact (floating point). */
template <typename T>
struct IsFloatingPoint : std::bool_constant<!std::is_integral_v<T> && !Detail::IsExact<T>>
{};
/** The NumericTraits ValueType of T is exact, but T is not integral. */
template <typename T>
struct IsFixedPoint : std::bool_constant<!std::is_integral_v<T> && Detail::IsExact<T>>
{};
} // namespace itk::Concept

#endif
