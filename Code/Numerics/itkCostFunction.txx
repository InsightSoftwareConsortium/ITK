/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCostFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCostFunction_txx
#define _itkCostFunction_txx

#include "itkCostFunction.h"

namespace itk
{
void
CostFunction
::PrintSelf(std::ostream& os, Indent indent) const
{ 
  Superclass::PrintSelf(os,indent);
  os << indent << "Parameters: " << m_Parameters << std::endl;
}

} // end namespace itk

#endif
