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
#ifndef itkDeformationFieldJacobianDeterminantFilter_hxx
#define itkDeformationFieldJacobianDeterminantFilter_hxx

#include "itkDeformationFieldJacobianDeterminantFilter.h"

#include "itkNeighborhoodAlgorithm.h"
#include "itkImageRegionIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkProgressReporter.h"
#include "itkVectorCastImageFilter.h"

#include "itkMath.h"

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
