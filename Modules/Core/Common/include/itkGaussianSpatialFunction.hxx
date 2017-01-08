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
#ifndef itkGaussianSpatialFunction_hxx
#define itkGaussianSpatialFunction_hxx

#include <cmath>
#include "itkMath.h"
#include "itkGaussianSpatialFunction.h"

namespace itk
{
template< typename TOutput, unsigned int VImageDimension, typename TInput >
GaussianSpatialFunction< TOutput, VImageDimension, TInput >
::GaussianSpatialFunction() :
  m_Scale( 1.0 ),
  m_Normalized( false )
{
  m_Mean = ArrayType::Filled(10.0);
  m_Sigma = ArrayType::Filled(5.0);
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
  // We have to compute the Gaussian in several stages, because of the
  // n-dimensional generalization

  // Normalizing the Gaussian is important for statistical applications
  // but is generally not desirable for creating images because of the
  // very small numbers involved (would need to use doubles)
  double prefixDenom = 1.0;

  if ( m_Normalized )
    {
    const double squareRootOfTwoPi = std::sqrt(2.0 * itk::Math::pi);

    for ( unsigned int i = 0; i < VImageDimension; ++i )
      {
      prefixDenom *= m_Sigma[i] * squareRootOfTwoPi;
      }
    }

  double suffixExp = 0;

  for ( unsigned int i = 0; i < VImageDimension; ++i )
    {
    suffixExp += ( position[i] - m_Mean[i] ) * ( position[i] - m_Mean[i] )
                 / ( 2 * m_Sigma[i] * m_Sigma[i] );
    }

  const double value = m_Scale * ( 1 / prefixDenom ) * std::exp(-1 * suffixExp);

  return static_cast< TOutput >( value );
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
