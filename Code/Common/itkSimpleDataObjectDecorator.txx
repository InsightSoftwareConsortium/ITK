/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleDataObjectDecorator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSimpleDataObjectDecorator_txx
#define __itkSimpleDataObjectDecorator_txx

#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/**
 *
 */
template< class T >
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
template< class T >
SimpleDataObjectDecorator< T >
::~SimpleDataObjectDecorator()
{}

/**
 *
 */
template< class T >
void
SimpleDataObjectDecorator< T >
::Set(const T & val)
{
  if ( !this->m_Initialized || ( this->m_Component != val ) )
    {
    this->m_Component = val;
    this->m_Initialized = true;
    this->Modified();
    }
}

/**
 *
 */
template< class T >
void
SimpleDataObjectDecorator< T >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Component  : " << typeid( this->m_Component ).name() << std::endl;
  os << indent << "Initialized: " << this->m_Initialized << std::endl;
}
} // end namespace itk

#endif
