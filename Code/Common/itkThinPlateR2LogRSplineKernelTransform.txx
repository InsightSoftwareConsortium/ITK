/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThinPlateR2LogRSplineKernelTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkThinPlateR2LogRSplineKernelTransform_txx
#define _itkThinPlateR2LogRSplineKernelTransform_txx
#include "itkThinPlateR2LogRSplineKernelTransform.h"

namespace itk
{

template <class TScalarType, unsigned int NDimensions>
const typename ThinPlateR2LogRSplineKernelTransform<TScalarType, NDimensions>::GMatrixType &
ThinPlateR2LogRSplineKernelTransform<TScalarType, NDimensions>::
ComputeG(const InputVectorType & x) const
{

  const TScalarType r = x.GetNorm();
  m_GMatrix.fill( NumericTraits< TScalarType >::Zero );
  const TScalarType R2logR = 
      ( r > 1e-8 )? r * r * log( r ) : NumericTraits<TScalarType>::Zero;
  for(unsigned int i=0; i<NDimensions; i++)
    {
    m_GMatrix[i][i] = R2logR;
    }
  return m_GMatrix;
}



template <class TScalarType, unsigned int NDimensions>
void
ThinPlateR2LogRSplineKernelTransform<TScalarType, NDimensions>::
ComputeDeformationContribution( const InputPointType  & thisPoint,
                                      OutputPointType & result     ) const
{

  unsigned long numberOfLandmarks = m_SourceLandmarks->GetNumberOfPoints();

  PointsIterator sp  = m_SourceLandmarks->GetPoints()->Begin();

  for(unsigned int lnd=0; lnd < numberOfLandmarks; lnd++ )
    {
    InputVectorType position = thisPoint - sp->Value();
    const TScalarType r = position.GetNorm();
    const TScalarType R2logR = 
      ( r > 1e-8 )? r * r * log( r ) : NumericTraits<TScalarType>::Zero;
    for(unsigned int odim=0; odim < NDimensions; odim++ )
      {
      result[ odim ] += R2logR * m_DMatrix(odim,lnd);
      }
    ++sp;
    }

}


} // namespace itk
#endif
