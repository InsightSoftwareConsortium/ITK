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
#include "itkElasticBodySplineKernelTransform.h"

namespace itk
{

template <class TScalarType, int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
ElasticBodySplineKernelTransform() 
{
  m_Alpha = .25;
}

template <class TScalarType, int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
~ElasticBodySplineKernelTransform()
{
}

template <class TScalarType, int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::GMatrixType
ElasticBodySplineKernelTransform<TScalarType, NDimensions>
::ComputeG(const InputVectorType & x) const
{
  const TScalarType r = x.GetNorm();
  IMatrixType CV;
  for(unsigned int i=0; i<NDimensions; i++)
    {
    for(unsigned int j=0; j<NDimensions; j++)
      {
        CV[i][j] = x[i] * x[j];
      }
    }
  return ( (m_Alpha * (r*r) * m_I) - ( CV*3.0 ) ) * r;
}

} // namespace itk
