/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkElasticBodySplineKernelTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkElasticBodySplineKernelTransform_txx
#define _itkElasticBodySplineKernelTransform_txx
#include "itkElasticBodySplineKernelTransform.h"

namespace itk
{

template <class TScalarType, unsigned int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
ElasticBodySplineKernelTransform() 
{
  m_Alpha = .25;
}

template <class TScalarType, unsigned int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
~ElasticBodySplineKernelTransform()
{
}

template <class TScalarType, unsigned int NDimensions>
const typename ElasticBodySplineKernelTransform<TScalarType, NDimensions>::GMatrixType &
ElasticBodySplineKernelTransform<TScalarType, NDimensions>
::ComputeG(const InputVectorType & x) const
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
        m_GMatrix[i][j] = value;
        m_GMatrix[j][i] = value;
      }
    m_GMatrix[i][i] =  radial + xi * x[i];
    }
  
  return m_GMatrix;
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
