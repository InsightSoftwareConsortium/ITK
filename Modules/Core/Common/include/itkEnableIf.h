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
#ifndef __itkEnableIf_h
#define __itkEnableIf_h

namespace itk
{

/* \brief This is an implementation of the enable if idiom.
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
 * with SFIANE another implementation may be choosen.
 *
 * Example:
 * \code
 *
 * template< typename T>
 *   typename EnableIfC<
 *     IsSame<T, typename NumericTraits<T>::ValueType>::Value,
 *     T >::Type
 * GetComponent(const T pix,
 *              unsigned int itkNotUsed( idx ) ) const
 * {
 *   return pix;
 * }
 * \endcode
 *
 */
template <bool V, typename T = void> struct EnableIfC {};
/** \cond SPECIALIZATION_IMPLEMENTATION */
template <typename T> struct EnableIfC<true, T> { typedef T Type; };
/**\endcond*/


/* \brief An implementation of the negation of the enable if idiom.
 *
 * \sa EnableIf
 */
template <bool V, typename T = void> struct DisableIfC {};
/** \cond SPECIALIZATION_IMPLEMENTATION */
template <typename T> struct DisableIfC<false, T> { typedef T Type; };
/**\endcond*/

}

#endif // __itkEnableIf_h
