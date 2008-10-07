/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkElasticBodySplineKernelTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkElasticBodySplineKernelTransform_txx
#define __itkElasticBodySplineKernelTransform_txx
#include "itkElasticBodySplineKernelTransform.h"

namespace itk
{

template <class TScalarType, unsigned int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
ElasticBodySplineKernelTransform() 
{
  // Alpha = 12 ( 1 - \nu ) - 1
  m_Alpha = 12.0 * ( 1.0 - .25 ) - 1;
}

template <class TScalarType, unsigned int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
~ElasticBodySplineKernelTransform()
{
}

/**
 * This method has been deprecated as of ITK 3.6.
 * Please use the method: void ComputeG(vector,gmatrix) instead.
 */
#if !defined(ITK_LEGACY_REMOVE)
template <class TScalarType, unsigned int NDimensions>
const typename ElasticBodySplineKernelTransform<TScalarType, NDimensions>::GMatrixType &
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
ComputeG( const InputVectorType & ) const
{
  itkLegacyReplaceBodyMacro(itkElasticBodySplineKernelTransform::ComputeG_vector, 
    3.6,itkElasticBodySplineKernelTransform::ComputeG_vector_gmatrix);
  return this->m_GMatrix;
}
#endif
template <class TScalarType, unsigned int NDimensions>
void
ElasticBodySplineKernelTransform<TScalarType, NDimensions>
::ComputeG(const InputVectorType & x, GMatrixType & gmatrix) const
{
  const TScalarType r       = x.GetNorm();
  const TScalarType factor  = -3.0 * r;
  const TScalarType radial  = m_Alpha * ( r * r ) * r;
  for(unsigned int i=0; i<NDimensions; i++)
    {
    const typename InputVectorType::ValueType xi = x[i] * factor;
    // G is symmetric
    for(unsigned int j=0; j<i; j++)
      {
      const TScalarType value = xi * x[j]; 
      gmatrix[i][j] = value;
      gmatrix[j][i] = value;
      }
    gmatrix[i][i] =  radial + xi * x[i];
    }
}

template <class TScalarType, unsigned int NDimensions>
void
ElasticBodySplineKernelTransform<TScalarType, NDimensions>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "m_Alpha: " << m_Alpha << std::endl;
}

} // namespace itk
#endif
