/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDerivativeSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianDerivativeSpatialFunction_txx
#define __itkGaussianDerivativeSpatialFunction_txx

#include <math.h>
#include "itkGaussianDerivativeSpatialFunction.h"

namespace itk
{

template <typename TOutput, unsigned int VImageDimension, typename TInput>
GaussianDerivativeSpatialFunction<TOutput, VImageDimension, TInput>
::GaussianDerivativeSpatialFunction()
{
  m_Mean = ArrayType::Filled(0.0);
  m_Sigma = ArrayType::Filled(1.0);
  m_Scale = 1.0;
  m_Normalized = false;
  m_Direction = 0;
}

template <typename TOutput, unsigned int VImageDimension, typename TInput>
GaussianDerivativeSpatialFunction<TOutput, VImageDimension, TInput>
::~GaussianDerivativeSpatialFunction()
{

}

template <typename TOutput, unsigned int VImageDimension, typename TInput>
typename GaussianDerivativeSpatialFunction<TOutput, VImageDimension, TInput>::OutputType 
GaussianDerivativeSpatialFunction<TOutput, VImageDimension, TInput>
::Evaluate(const TInput& position) const
{
  // Normalizing the Gaussian is important for statistical applications
  // but is generally not desirable for creating images because of the
  // very small numbers involved (would need to use doubles)
  double prefixDenom;

  if (m_Normalized)
    {
    prefixDenom = m_Sigma[0]*m_Sigma[0]*m_Sigma[0];

    for(unsigned int i = 1; i < VImageDimension; i++)
      {
      prefixDenom *= m_Sigma[i]*m_Sigma[i]*m_Sigma[i];
      }

    prefixDenom *= 2 * 3.1415927;
    }
  else
    {
    prefixDenom = 1.0;
    }

  double suffixExp = 0;

  for(unsigned int i = 0; i < VImageDimension; i++)
    {
    suffixExp += (position[m_Direction] - m_Mean[m_Direction])*(position[m_Direction] - m_Mean[m_Direction]) / (2 * m_Sigma[m_Direction] * m_Sigma[m_Direction]);
    }

 
  double value = -2*(position[m_Direction] - m_Mean[m_Direction])*m_Scale * (1 / prefixDenom) * exp(-1 * suffixExp);
 
  return (TOutput) value;
}


/** Evaluate the function at a given position and return a vector */
template <typename TOutput, unsigned int VImageDimension, typename TInput>
typename GaussianDerivativeSpatialFunction<TOutput, VImageDimension, TInput>::VectorType 
GaussianDerivativeSpatialFunction<TOutput, VImageDimension, TInput>
::EvaluateVector(const TInput& position) const
{
  VectorType gradient;
  for(unsigned int i = 0; i < VImageDimension; i++)
    {
      m_Direction = i;
      gradient[i]=this->Evaluate(position);
    } 
}

template <typename TOutput, unsigned int VImageDimension, typename TInput>
void
GaussianDerivativeSpatialFunction<TOutput, VImageDimension, TInput>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int i;
  os << indent << "Sigma: [";
  for (i=0; i+1 < VImageDimension ; i++)
    {
    os << m_Sigma[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Mean: [";
  for (i=0; i+1 < VImageDimension ; i++)
    {
    os << m_Mean[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Scale: " << m_Scale << std::endl;
  os << indent << "Normalized?: " << m_Normalized << std::endl;
  os << indent << "Direction: " << m_Direction << std::endl;
}


} // end namespace itk

#endif
