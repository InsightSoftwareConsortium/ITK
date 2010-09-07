/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkElasticBodyReciprocalSplineKernelTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkElasticBodyReciprocalSplineKernelTransform_txx
#define __itkElasticBodyReciprocalSplineKernelTransform_txx
#include "itkElasticBodyReciprocalSplineKernelTransform.h"

namespace itk
{
template< class TScalarType, unsigned int NDimensions >
ElasticBodyReciprocalSplineKernelTransform< TScalarType, NDimensions >::ElasticBodyReciprocalSplineKernelTransform()
{
  // Alpha = 8 ( 1 - \nu ) - 1
  m_Alpha = 8.0 * ( 1.0 - .25 ) - 1;
}

template< class TScalarType, unsigned int NDimensions >
ElasticBodyReciprocalSplineKernelTransform< TScalarType, NDimensions >::
~ElasticBodyReciprocalSplineKernelTransform()
{}

template< class TScalarType, unsigned int NDimensions >
void
ElasticBodyReciprocalSplineKernelTransform< TScalarType, NDimensions >
::ComputeG(const InputVectorType & x, GMatrixType & gmatrix) const
{
  const TScalarType r       = x.GetNorm();
  const TScalarType factor  =
    ( r > 1e-8 ) ? ( -1.0 / r ) : NumericTraits< TScalarType >::Zero;
  const TScalarType radial  = m_Alpha * r;

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
ElasticBodyReciprocalSplineKernelTransform< TScalarType, NDimensions >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Alpha: " << m_Alpha << std::endl;
}
} // namespace itk
#endif
