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
#ifndef itkEnableIf_h
#define itkEnableIf_h

namespace itk
{
/// \cond HIDE_META_PROGRAMMING
namespace mpl
{

/** \brief This is an implementation of the enable if idiom.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 *
 * Enable if is a common C++ meta-programming technique for example
 * there is a boost implementation, for more information on its usage
 * please see:
 * http://www.boost.org/doc/libs/1_49_0/libs/utility/enable_if.html
 *
 * This template enables specialization of a templated function based
 * on some traits or concepts. It is implemented with SFINAE.
 *
 * If the parameter V is true then the Type trait is the second
 * template parameter, otherwise an implementation does not exist and
 * with SFINAE another implementation may be choosen.
 *
 * Example:
 \code
 template< typename TType>
   typename EnableIfC<
     IsSame<TType, typename NumericTraits<TType>::ValueType>::Value,
     TType >::Type
 GetComponent(const TType pix,
              unsigned int itkNotUsed( idx ) ) const
 {
   return pix;
 }
 \endcode
 *
 * \sa \c EnableIf
 */
template <bool V, typename TType = void> struct EnableIfC {};
/// \cond SPECIALIZATION_IMPLEMENTATION
template <typename TType> struct EnableIfC<true, TType> { typedef TType Type; };
/// \endcond


/** \brief An implementation of the negation of the enable if idiom.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 *
 * \sa \c EnableIfC
 * \sa \c DisableIf
 */
template <bool V, typename TType = void> struct DisableIfC {};
/// \cond SPECIALIZATION_IMPLEMENTATION
template <typename TType> struct DisableIfC<false, TType> { typedef TType Type; };
/// \endcond

/** \brief simplified way to dispose of \c enable_if.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 * \tparam TCondition Condition type. It's expected to provide a boolean value
 * through its \c Value member.
 * \tparam TType      Type to \em return when the \c TCondition is (a) true (type).
 *
 * This \em overload automatically fetches \c TCondition value. However, beware, it
 * won't work with standard C++ traits or boost traits. Indeed, this \c
 * enable_if overload expects the \em value to follow UpperCamelCase ITK naming
 * policy instead of the standard snake_case policy.
 *
 * Example:
 * \code
 * template< typename TType>
 *   typename EnableIf<
 *     IsSame<TType, typename NumericTraits<TType>::ValueType>,
 *     TType >::Type
 * GetComponent(const TType pix,
 *              unsigned int itkNotUsed( idx ) ) const
 * {
 *   return pix;
 * }
 * \endcode
 * \sa \c EnableIfC
 * \sa \c DisableIf
 */
template <class TCondition, class TType = void>
struct EnableIf : public EnableIfC<TCondition::Value, TType> {};

/** \brief simplified way to dispose of \c disable_if.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 * \tparam TCondition Condition type. It's expected to provide a boolean value
 * through its \c Value member.
 * \tparam TType      Type to \em return when the \c TCondition is (a) false (type).
 *
 * This \em overload automatically fetches \c TCondition value. However, beware, it
 * won't work with standard C++ traits or boost traits. Indeed, this \c
 * enable_if overload expects the \em value to follow UpperCamelCase ITK naming
 * policy instead of the standard snake_case policy.
 *
 * Example:
 * \code
 * template< typename TType>
 *   typename DisableIf<
 *     mpl::Not_<IsSame<TType, typename NumericTraits<TType>::ValueType>>,
 *     TType >::Type
 * GetComponent(const TType pix,
 *              unsigned int itkNotUsed( idx ) ) const
 * {
 *   return pix;
 * }
 * \endcode
 * \sa \c EnableIfC
 * \sa \c DisableIf
 */
template <class TCondition, class TType = void>
struct DisableIf : public DisableIfC<TCondition::Value, TType> {};

} // namespace itk::mpl

// itk::EnableIf(C), DisableIf(C) have move to itk::mpl
// Expect them to be deprecated.
using mpl::EnableIf;
using mpl::DisableIf;
using mpl::EnableIfC;
using mpl::DisableIfC;

/// \endcond
} // namespace itk

#endif // itkEnableIf_h
