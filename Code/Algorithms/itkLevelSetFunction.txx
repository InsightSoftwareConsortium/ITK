/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevelSetFunction_txx_
#define __itkLevelSetFunction_txx_

namespace itk {
  
template <class TImageType>
typename LevelSetFunction<TImageType>::VectorType
LevelSetFunction<TImageType>::InitializeZeroVectorConstant()
{
  VectorType ans;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    { 
    ans[i] = NumericTraits<ScalarValueType>::Zero; 
    }

  return ans;
}

template <class TImageType>
typename LevelSetFunction<TImageType>::VectorType
LevelSetFunction<TImageType>::m_ZeroVectorConstant =
LevelSetFunction<TImageType>::InitializeZeroVectorConstant();

template <class TImageType>
void
LevelSetFunction<TImageType>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "EpsilonMagnitude: " << m_EpsilonMagnitude << std::endl;
  os << indent << "AdvectionWeight: " << m_AdvectionWeight << std::endl;
  os << indent << "PropagationWeight: " << m_PropagationWeight << std::endl;
  os << indent << "CurvatureWeight: " << m_CurvatureWeight << std::endl;
}
  
} // end namespace itk

#endif
