/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianSpatialFunction_txx
#define __itkGaussianSpatialFunction_txx

#include <math.h>
#include "vnl/vnl_math.h"
#include "itkGaussianSpatialFunction.h"

namespace itk
{
template< typename TOutput, unsigned int VImageDimension, typename TInput >
GaussianSpatialFunction< TOutput, VImageDimension, TInput >
::GaussianSpatialFunction()
{
  m_Mean = ArrayType::Filled(10.0);
  m_Sigma = ArrayType::Filled(5.0);
  m_Scale = 1.0;
  m_Normalized = false;
}

template< typename TOutput, unsigned int VImageDimension, typename TInput >
GaussianSpatialFunction< TOutput, VImageDimension, TInput >
::~GaussianSpatialFunction()
{}

template< typename TOutput, unsigned int VImageDimension, typename TInput >
typename GaussianSpatialFunction< TOutput, VImageDimension, TInput >::OutputType
GaussianSpatialFunction< TOutput, VImageDimension, TInput >
::Evaluate(const TInput & position) const
{
  // We have to compute the gaussian in several stages, because of the
  // n-dimensional generalization

  // Normalizing the Gaussian is important for statistical applications
  // but is generally not desirable for creating images because of the
  // very small numbers involved (would need to use doubles)
  double prefixDenom = 1.0;

  if ( m_Normalized )
    {
    const double squareRootOfTwoPi = vcl_sqrt(2.0 * vnl_math::pi);

    for ( unsigned int i = 0; i < VImageDimension; i++ )
      {
      prefixDenom *= m_Sigma[i] * squareRootOfTwoPi;
      }
    }

  double suffixExp = 0;

  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    suffixExp += ( position[i] - m_Mean[i] ) * ( position[i] - m_Mean[i] )
                 / ( 2 * m_Sigma[i] * m_Sigma[i] );
    }

  double value = m_Scale * ( 1 / prefixDenom ) * vcl_exp(-1 * suffixExp);

  return (TOutput)value;
}

template< typename TOutput, unsigned int VImageDimension, typename TInput >
void
GaussianSpatialFunction< TOutput, VImageDimension, TInput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Sigma: " << m_Sigma << std::endl;
  os << indent << "Mean: " << m_Mean << std::endl;
  os << indent << "Scale: " << m_Scale << std::endl;
  os << indent << "Normalized?: " << m_Normalized << std::endl;
}
} // end namespace itk

#endif
