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

#ifndef itkIsNumber_h
#define itkIsNumber_h

#include "itkMetaProgrammingLibrary.h"
#include "itkIntTypes.h"


/// \cond HIDE_META_PROGRAMMING
namespace itk::mpl
{
/** Tells whether a type is a number.
 * \c TrueType for all kinds of numbers from \c short to `long long`,
 * and from \c float to `long double`.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <typename T>
struct IsNumber : FalseType
{};

/// \cond SPECIALIZATION_IMPLEMENTATION
template <>
struct IsNumber<unsigned char> : TrueType
{};
template <>
struct IsNumber<signed char> : TrueType
{};
template <>
struct IsNumber<unsigned short> : TrueType
{};
template <>
struct IsNumber<short> : TrueType
{};
template <>
struct IsNumber<int> : TrueType
{};
template <>
struct IsNumber<unsigned int> : TrueType
{};
template <>
struct IsNumber<long> : TrueType
{};
template <>
struct IsNumber<unsigned long> : TrueType
{};
template <>
struct IsNumber<long long> : TrueType
{};
template <>
struct IsNumber<unsigned long long> : TrueType
{};
template <>
struct IsNumber<float> : TrueType
{};
template <>
struct IsNumber<double> : TrueType
{};
template <>
struct IsNumber<long double> : TrueType
{};
/// \endcond

} // namespace itk::mpl
/// \endcond

#endif // itkIsNumber_h
