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


namespace itk
{
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

namespace
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
} // namespace

template <typename MetaDataObjectType>
void
MetaDataObject<MetaDataObjectType>::Print(std::ostream & os) const
{
  // future c++20 feature
  // constexpr bool hasPrint = false; requires( const &MetaDataObjectType obj ) { obj.Print(os); };

  if constexpr (has_Print<MetaDataObjectType>::value)
  {
    m_MetaDataObjectValue.Print(os);
  }
  else if constexpr (has_output_operator<MetaDataObjectType>::value)
  {
    os << m_MetaDataObjectValue;
  }
  else
  {
    Superclass::Print(os);
  }
}

} // end namespace itk

#endif
