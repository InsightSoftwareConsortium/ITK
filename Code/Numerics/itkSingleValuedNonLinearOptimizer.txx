/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedNonLinearOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSingleValuedNonLinearOptimizer_txx
#define _itkSingleValuedNonLinearOptimizer_txx

#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{

SingleValuedNonLinearOptimizer
::SingleValuedNonLinearOptimizer()
{
}



void
SingleValuedNonLinearOptimizer
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Cost Function " << m_CostFunction << std::endl;
}


} // namespace itk

#endif
