/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedNonLinearOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMultipleValuedNonLinearOptimizer_txx
#define _itkMultipleValuedNonLinearOptimizer_txx

#include "itkMultipleValuedNonLinearOptimizer.h"

namespace itk
{
MultipleValuedNonLinearOptimizer
::MultipleValuedNonLinearOptimizer()
{
  m_CostFunction = 0;
}

/**
 * Connect a Cost Function
 */
void
MultipleValuedNonLinearOptimizer
::SetCostFunction(CostFunctionType *costFunction)
{
  if ( m_CostFunction.GetPointer() == costFunction )
    {
    return;
    }

  itkDebugMacro("setting CostFunction  to " <<  costFunction);

  m_CostFunction = costFunction;

  if ( !m_ScalesInitialized )
    {
    const unsigned int numberOfParameters =
      m_CostFunction->GetNumberOfParameters();

    ScalesType scales(numberOfParameters);
    scales.Fill(1.0f);
    SetScales(scales);
    m_ScalesInitialized = true;
    }

  this->Modified();
}

void
MultipleValuedNonLinearOptimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if ( m_CostFunction )
    {
    os << indent << "Cost Function: " << m_CostFunction.GetPointer() << std::endl;
    }
}
} // namespace itk

#endif
