/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformationFieldJacobianDeterminantFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDeformationFieldJacobianDeterminantFilter_txx
#define __itkDeformationFieldJacobianDeterminantFilter_txx

#include "itkDeformationFieldJacobianDeterminantFilter.h"

#include "itkNeighborhoodAlgorithm.h"
#include "itkImageRegionIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkProgressReporter.h"
#include "itkVectorCastImageFilter.h"

#include "vnl/vnl_math.h"

namespace itk
{
template< typename TInputImage, typename TRealType, typename TOutputImage >
DeformationFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
::DeformationFieldJacobianDeterminantFilter()
{}

template< typename TInputImage, typename TRealType, typename TOutputImage >
TRealType
DeformationFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
::EvaluateAtNeighborhood(const ConstNeighborhoodIteratorType & it) const
{
  vnl_matrix_fixed< TRealType, ImageDimension, VectorDimension > J;
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    for ( unsigned int j = 0; j < VectorDimension; ++j )
      {
      J[i][j] = this->m_HalfDerivativeWeights[i] * ( it.GetNext(i)[j] - it.GetPrevious(i)[j] );
      }
    }
  return vnl_det(J);
}

template< typename TInputImage, typename TRealType, typename TOutputImage >
void
DeformationFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
