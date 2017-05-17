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
#ifndef itkSimpleDataObjectDecorator_hxx
#define itkSimpleDataObjectDecorator_hxx

#include "itkSimpleDataObjectDecorator.h"
#include "itkMath.h"

namespace itk
{
/**
 *
 */
template< typename T >
SimpleDataObjectDecorator< T >
::SimpleDataObjectDecorator()
{
  this->m_Component = ComponentType(); // initialize here to avoid Purify UMR
  this->m_Initialized = false;         // Still needed since not all objects
                                       // are initialized at construction time.
                                       // for example the itkArray.
}

/**
 *
 */
template< typename T >
SimpleDataObjectDecorator< T >
::~SimpleDataObjectDecorator()
{}

/**
 *
 */
template< typename T >
void
SimpleDataObjectDecorator< T >
::Set(const T & val)
{
  if ( !this->m_Initialized || ( Math::NotExactlyEquals(this->m_Component, val) ) )
    {
    this->m_Component = val;
    this->m_Initialized = true;
    this->Modified();
    }
}

/**
 *
 */
template< typename T >
void
SimpleDataObjectDecorator< T >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  // This is necessary to avoid linker warnings on MacOS
  #if !defined(ITK_PRIVATE_DYNAMIC_CAST)
  os << indent << "Component  : unknown" << std::endl;
  #else
  os << indent << "Component  : " << typeid( this->m_Component ).name() << std::endl;
  #endif
  os << indent << "Initialized: " << this->m_Initialized << std::endl;
}
} // end namespace itk

#endif
