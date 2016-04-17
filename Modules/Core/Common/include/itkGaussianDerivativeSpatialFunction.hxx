/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkGaussianDerivativeSpatialFunction_hxx
#define itkGaussianDerivativeSpatialFunction_hxx

#include <cmath>
#include "itkMath.h"
#include "itkGaussianDerivativeSpatialFunction.h"

namespace itk
{
template< typename TOutput, unsigned int VImageDimension, typename TInput >
GaussianDerivativeSpatialFunction< TOutput, VImageDimension, TInput >
::GaussianDerivativeSpatialFunction()
{
  m_Mean = ArrayType::Filled(0.0);
  m_Sigma = ArrayType::Filled(1.0);
  m_Scale = 1.0;
  m_Normalized = false;
  m_Direction = 0;
}

template< typename TOutput, unsigned int VImageDimension, typename TInput >
GaussianDerivativeSpatialFunction< TOutput, VImageDimension, TInput >
::~GaussianDerivativeSpatialFunction()
{}

template< typename TOutput, unsigned int VImageDimension, typename TInput >
typename GaussianDerivativeSpatialFunction< TOutput, VImageDimension, TInput >::OutputType
GaussianDerivativeSpatialFunction< TOutput, VImageDimension, TInput >
::Evaluate(const TInput & position) const
{
  // Normalizing the Gaussian is important for statistical applications
  // but is generally not desirable for creating images because of the
  // very small numbers involved (would need to use doubles)
  double prefixDenom;

  if ( m_Normalized )
    {
    prefixDenom = m_Sigma[m_Direction] * m_Sigma[m_Direction];

    for ( unsigned int i = 0; i < VImageDimension; i++ )
      {
      prefixDenom *= m_Sigma[i];
      }

    prefixDenom *= 2 * std::pow(2 * itk::Math::pi, VImageDimension / 2.0);
    }
  else
    {
    prefixDenom = 1.0;
    }

  double suffixExp = 0;

  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    suffixExp += ( position[m_Direction] - m_Mean[m_Direction] )
                 * ( position[m_Direction] - m_Mean[m_Direction] )
                 / ( 2 * m_Sigma[m_Direction] * m_Sigma[m_Direction] );
    }

  double value = -2 * ( position[m_Direction] - m_Mean[m_Direction] ) * m_Scale * ( 1 / prefixDenom ) * std::exp(
    -1 * suffixExp);

  return static_cast<TOutput>(value);
}

/** Evaluate the function at a given position and return a vector */
template< typename TOutput, unsigned int VImageDimension, typename TInput >
typename GaussianDerivativeSpatialFunction< TOutput, VImageDimension, TInput >::VectorType
GaussianDerivativeSpatialFunction< TOutput, VImageDimension, TInput >
::EvaluateVector(const TInput & position) const
{
  VectorType gradient;

  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    m_Direction = i;
    gradient[i] = this->Evaluate(position);
    }
  return gradient;
}

template< typename TOutput, unsigned int VImageDimension, typename TInput >
void
GaussianDerivativeSpatialFunction< TOutput, VImageDimension, TInput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Sigma: " << m_Sigma << std::endl;
  os << indent << "Mean: " <<  m_Mean << std::endl;
  os << indent << "Scale: " << m_Scale << std::endl;
  os << indent << "Normalized?: " << m_Normalized << std::endl;
  os << indent << "Direction: " << m_Direction << std::endl;
}
} // end namespace itk

#endif
