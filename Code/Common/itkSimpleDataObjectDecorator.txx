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
template<class T>
SimpleDataObjectDecorator<T>
::SimpleDataObjectDecorator() 
{
  m_Component = ComponentType(); // initialize here to avoid Purify UMR
}


/**
 *
 */
template<class T>
SimpleDataObjectDecorator<T>
::~SimpleDataObjectDecorator()
{
}


/**
 *
 */
template<class T>
void
SimpleDataObjectDecorator<T>
::Set(const T& val)
{
  if (m_Component != val)
    {
    m_Component = val;
    this->Modified();
    }
}

/**
 *
 */
template<class T>
void 
SimpleDataObjectDecorator<T>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Component: " << typeid(m_Component).name() << std::endl;
}

} // end namespace itk

#endif
