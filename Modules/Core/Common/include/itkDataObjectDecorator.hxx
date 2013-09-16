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
#ifndef __itkDataObjectDecorator_hxx
#define __itkDataObjectDecorator_hxx

#include "itkDataObjectDecorator.h"

namespace itk
{
/**
 *
 */
template< typename T >
DataObjectDecorator< T >
::DataObjectDecorator():m_Component()
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
::Set(const T *val)
{
  if ( m_Component != val )
    {
    m_Component = val;
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
  return m_Component;
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
