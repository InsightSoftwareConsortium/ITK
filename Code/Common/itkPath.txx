/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPath.txx
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
#ifndef __itkPath_txx
#define __itkPath_txx

#include "itkPath.h"
#include "itkProcessObject.h"

/* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** */
/* ** **  Much of the code in this file is based on itkPointSet.txx  ** ** */
/* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** */

namespace itk
{

/**
 * A protected default constructor allows the New() routine to create an
 * instance of PointSet.  All the containers are initialized to non-existent.
 */
template <class TInput, class TOutput, unsigned int VDimension>
Path<TInput, TOutput, VDimension>
::Path()
{
  m_ZeroOffset.Fill(0);
  m_ZeroIndex.Fill(0);
}

template <class TInput, class TOutput, unsigned int VDimension>
void
Path<TInput, TOutput, VDimension>
::PrintSelf(std::ostream &os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "ZeroOffset: " << m_ZeroOffset << std::endl;
  os << indent << "ZeroIndex: " << m_ZeroIndex << std::endl;
}

} // end namespace itk

#endif
