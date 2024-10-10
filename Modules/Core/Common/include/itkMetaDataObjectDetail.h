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

#ifndef itkMetaDataObjectDetail_h
#define itkMetaDataObjectDetail_h

#include <type_traits>
#include <utility>
#include <iosfwd>

namespace itk
{

// Implementation details for MetaDataObject meta programming
namespace MetaDataObjectDetail
{
template <class T, class = void>
struct has_Print : std::false_type
{};

template <class T>
struct has_Print<T, std::void_t<decltype(std::declval<T>().Print(std::declval<std::ostream &>()))>> : std::true_type
{};

template <class T, class = void>
struct has_output_operator : std::false_type
{};

template <class T>
struct has_output_operator<T, std::void_t<decltype(std::declval<std::ostream &>() << std::declval<T>())>>
  : std::true_type
{};
} // namespace MetaDataObjectDetail
} // namespace itk

#endif
