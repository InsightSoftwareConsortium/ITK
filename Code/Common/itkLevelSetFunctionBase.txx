/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetFunctionBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevelSetFunctionBase_txx_
#define __itkLevelSetFunctionBase_txx_

#include "itkLevelSetFunctionBase.h"

namespace itk {
  
template <class TImageType>
typename LevelSetFunctionBase<TImageType>::VectorType
LevelSetFunctionBase<TImageType>::InitializeZeroVectorConstant()
{
  VectorType ans;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    { 
    ans[i] = NumericTraits<ScalarValueType>::Zero; 
    }

  return ans;
}

template <class TImageType>
typename LevelSetFunctionBase<TImageType>::VectorType
LevelSetFunctionBase<TImageType>::m_ZeroVectorConstant =
LevelSetFunctionBase<TImageType>::InitializeZeroVectorConstant();

template <class TImageType>
void
LevelSetFunctionBase<TImageType>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "EpsilonMagnitude: " << m_EpsilonMagnitude << std::endl;
  os << indent << "AdvectionWeight: " << m_AdvectionWeight << std::endl;
  os << indent << "PropagationWeight: " << m_PropagationWeight << std::endl;
  os << indent << "CurvatureWeight: " << m_CurvatureWeight << std::endl;
  os << indent << "LaplacianSmoothingWeight: " << m_LaplacianSmoothingWeight << std::endl;
}
  
} // end namespace itk

#endif
