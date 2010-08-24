/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryThresholdSpatialFunction_txx
#define __itkBinaryThresholdSpatialFunction_txx

#include "itkBinaryThresholdSpatialFunction.h"

namespace itk
{
template< typename TFunction >
BinaryThresholdSpatialFunction< TFunction >
::BinaryThresholdSpatialFunction()
{
  m_LowerThreshold = NumericTraits< FunctionOutputType >::NonpositiveMin();
  m_UpperThreshold = NumericTraits< FunctionOutputType >::max();
  m_Function = NULL;
}

template< typename TFunction >
BinaryThresholdSpatialFunction< TFunction >
::~BinaryThresholdSpatialFunction()
{}

template< typename TFunction >
void
BinaryThresholdSpatialFunction< TFunction >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << " m_LowerThreshold: " << m_LowerThreshold << std::endl;
  os << indent << " m_UpperThreshold: " << m_UpperThreshold << std::endl;
  os << indent << " m_Function: " << m_Function.GetPointer() << std::endl;
}

template< typename TFunction >
typename BinaryThresholdSpatialFunction< TFunction >
::OutputType
BinaryThresholdSpatialFunction< TFunction >
::Evaluate(const InputType & point) const
{
  FunctionOutputType value = m_Function->Evaluate(point);

  if ( m_LowerThreshold <= value && value <= m_UpperThreshold )
    {
    return true;
    }
  return false;
}
} // end namespace itk

#endif
