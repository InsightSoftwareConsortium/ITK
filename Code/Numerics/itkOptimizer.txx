/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkOptimizer_txx
#define _itkOptimizer_txx

#include "itkOptimizer.h"

namespace itk
{




void
Optimizer
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "InitialPosition: "
     << m_InitialPosition << std::endl;
  os << indent << "CurrentPosition: "
     << m_CurrentPosition << std::endl;
  os << indent << "Scales: "
     << m_Scales << std::endl;
}



} // end namespace itk

#endif
