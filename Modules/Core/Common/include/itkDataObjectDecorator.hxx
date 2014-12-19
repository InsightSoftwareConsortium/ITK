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
#ifndef itkDataObjectDecorator_hxx
#define itkDataObjectDecorator_hxx

#include "itkDataObjectDecorator.h"
#include <algorithm>

namespace itk
{
/**
 *
 */
template< typename T >
DataObjectDecorator< T >
::DataObjectDecorator()
{}

/**
 *
 */
template< typename T >
DataObjectDecorator< T >
::~DataObjectDecorator()
{}

/**
 *
 */
template< typename T >
void
DataObjectDecorator< T >
::Set( const ComponentType *val)
{
  if ( m_Component != val )
    {
    m_Component = const_cast<ComponentType*>(val);
    this->Modified();
    }
}

/**
 *
 */
template< typename T >
const T *
DataObjectDecorator< T >
::Get() const
{
  return m_Component.GetPointer();
}

/**
 *
 */
template< typename T >
T *
DataObjectDecorator< T >
::GetModifiable()
{
  return m_Component.GetPointer();
}

/**
 *
 */
template< typename T >
ModifiedTimeType
DataObjectDecorator< T >
::GetMTime() const
{
  const ModifiedTimeType t = Superclass::GetMTime();
  if (m_Component.IsNotNull())
    {
    return std::max(t, m_Component->GetMTime());
    }
  return t;
}

/**
 *
 */
template< typename T >
void
DataObjectDecorator< T >
::Initialize()
{
  Superclass::Initialize();

  // make sure the MTime does not change
  if ( m_Component.IsNull())
    {
    return;
    }
  if ( m_Component->GetMTime() > Superclass::GetMTime() )
    {
    this->SetTimeStamp(m_Component->GetTimeStamp());
    }
  m_Component = ITK_NULLPTR;
}

/**
 *
 */
template< typename T >
void
DataObjectDecorator< T >
::Graft( const DataObject *data )
{
  const Self *decorator = dynamic_cast< const Self * >( data );
  this->Graft(decorator);
}

/**
 *
 */
template< typename T >
void
DataObjectDecorator< T >
::Graft( const Self *data )
{
  if ( !data )
    {
    return;
    }

  this->Set(data->m_Component);
}

/**
 *
 */
template< typename T >
void
DataObjectDecorator< T >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Component: " << m_Component << std::endl;
}
} // end namespace itk

#endif
