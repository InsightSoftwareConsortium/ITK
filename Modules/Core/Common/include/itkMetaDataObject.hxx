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

#include "itkMetaDataObject.h"

namespace itk
{

template< typename MetaDataObjectType >
MetaDataObject< MetaDataObjectType >
::MetaDataObject()
{
  // m_MetaDataObjectValue takes this types default value.
}

template< typename MetaDataObjectType >
MetaDataObject< MetaDataObjectType >
::~MetaDataObject()
{
}

template< typename MetaDataObjectType >
const char *
MetaDataObject< MetaDataObjectType >
::GetMetaDataObjectTypeName() const
{
  return typeid( MetaDataObjectType ).name();
}

template< typename MetaDataObjectType >
const std::type_info &
MetaDataObject< MetaDataObjectType >
::GetMetaDataObjectTypeInfo() const
{
  return typeid( MetaDataObjectType );
}

template< typename MetaDataObjectType >
const MetaDataObjectType &
MetaDataObject< MetaDataObjectType >
::GetMetaDataObjectValue() const
{
  return m_MetaDataObjectValue;
}

template< typename MetaDataObjectType >
void
MetaDataObject< MetaDataObjectType >
::SetMetaDataObjectValue(const MetaDataObjectType & newValue)
{
  m_MetaDataObjectValue = newValue;
}

template< typename MetaDataObjectType >
void
MetaDataObject< MetaDataObjectType >
::Print(std::ostream & os) const
{
  Superclass::Print(os);
}

} // end namespace itk

#endif
