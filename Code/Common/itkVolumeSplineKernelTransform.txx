/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVolumeSplineKernelTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVolumeSplineKernelTransform_txx
#define _itkVolumeSplineKernelTransform_txx
#include "itkVolumeSplineKernelTransform.h"

namespace itk
{

template <class TScalarType, int NDimensions>
const VolumeSplineKernelTransform<TScalarType, NDimensions>::GMatrixType &
VolumeSplineKernelTransform<TScalarType, NDimensions>::
ComputeG(const InputVectorType & x) const
{

  const TScalarType r = x.GetNorm();
  m_GMatrix.fill( NumericTraits< TScalarType >::Zero );
  const TScalarType r3 = r * r * r;
  for(unsigned int i=0; i<NDimensions; i++)
    {
    m_GMatrix[i][i] = r3;
    }
  return m_GMatrix;
}



} // namespace itk
#endif
