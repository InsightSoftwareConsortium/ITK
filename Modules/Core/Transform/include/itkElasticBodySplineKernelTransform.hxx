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
#ifndef __itkElasticBodySplineKernelTransform_hxx
#define __itkElasticBodySplineKernelTransform_hxx
#include "itkElasticBodySplineKernelTransform.h"

namespace itk
{
template< class TScalarType, unsigned int NDimensions >
ElasticBodySplineKernelTransform< TScalarType, NDimensions >::ElasticBodySplineKernelTransform()
{
  // Alpha = 12 ( 1 - \nu ) - 1
  m_Alpha = 12.0 * ( 1.0 - .25 ) - 1;
}

template< class TScalarType, unsigned int NDimensions >
ElasticBodySplineKernelTransform< TScalarType, NDimensions >::
~ElasticBodySplineKernelTransform()
{}

template< class TScalarType, unsigned int NDimensions >
void
ElasticBodySplineKernelTransform< TScalarType, NDimensions >
::ComputeG(const InputVectorType & x, GMatrixType & gmatrix) const
{
  const TScalarType r       = x.GetNorm();
  const TScalarType factor  = -3.0 * r;
  const TScalarType radial  = m_Alpha * ( r * r ) * r;

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    const typename InputVectorType::ValueType xi = x[i] * factor;
    // G is symmetric
    for ( unsigned int j = 0; j < i; j++ )
      {
      const TScalarType value = xi * x[j];
      gmatrix[i][j] = value;
      gmatrix[j][i] = value;
      }
    gmatrix[i][i] =  radial + xi * x[i];
    }
}

template< class TScalarType, unsigned int NDimensions >
void
ElasticBodySplineKernelTransform< TScalarType, NDimensions >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Alpha: " << m_Alpha << std::endl;
}
} // namespace itk
#endif
