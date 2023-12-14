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
#ifndef itkMetaDataObject_hxx
#define itkMetaDataObject_hxx

#include <type_traits>
#include <iterator>

template <typename T, typename = void>
inline constexpr bool is_iterable_print_v = false;
// Specialize for std::vector<T> and std::vector<std::vector<T>>
template <typename T>
inline constexpr bool is_iterable_print_v<std::vector<T>, std::void_t<>> = true;
template <typename T>
inline constexpr bool is_iterable_print_v<std::vector<std::vector<T>>, std::void_t<>> = true;

namespace itk
{
template <typename TIterable>
void
printIterable(std::ostream & os, const TIterable & iterable)
{
  if constexpr (is_iterable_print_v<TIterable>)
  {
    os << "[";
    auto begin = std::begin(iterable);
    auto end = std::end(iterable);
    for (auto it = begin; it != end; ++it)
    {
      if (it != begin)
      {
        os << ", ";
      }
      printIterable(os, *it);
    }
    os << "]";
  }
  else
  {
    // Handle non-iterable types
    os << iterable;
  }
}

template <typename MetaDataObjectType>
void
MetaDataObject<MetaDataObjectType>::PrintValue(std::ostream & os) const
{
  if constexpr (is_iterable_print_v<MetaDataObjectType>)
  {
    os << "[";
    auto begin = std::begin(m_MetaDataObjectValue);
    auto end = std::end(m_MetaDataObjectValue);
    for (auto it = begin; it != end; ++it)
    {
      if (it != begin)
      {
        os << ", ";
      }
      printIterable(os, *it);
    }
    os << "]";
  }
  else
  {
    os << m_MetaDataObjectValue;
  }
}

template <typename MetaDataObjectType>
void
MetaDataObject<MetaDataObjectType>::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent;
  this->PrintValue(os);
  os << std::endl;
}

template <typename MetaDataObjectType>
const char *
MetaDataObject<MetaDataObjectType>::GetMetaDataObjectTypeName() const
{
  return typeid(MetaDataObjectType).name();
}

template <typename MetaDataObjectType>
const std::type_info &
MetaDataObject<MetaDataObjectType>::GetMetaDataObjectTypeInfo() const
{
  return typeid(MetaDataObjectType);
}

template <typename MetaDataObjectType>
const MetaDataObjectType &
MetaDataObject<MetaDataObjectType>::GetMetaDataObjectValue() const
{
  return m_MetaDataObjectValue;
}

template <typename MetaDataObjectType>
void
MetaDataObject<MetaDataObjectType>::SetMetaDataObjectValue(const MetaDataObjectType & newValue)
{
  Self::Assign(m_MetaDataObjectValue, newValue);
}


} // end namespace itk

#endif
