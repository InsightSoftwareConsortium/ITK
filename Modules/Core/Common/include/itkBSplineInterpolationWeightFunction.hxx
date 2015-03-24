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
#ifndef itkBSplineInterpolationWeightFunction_hxx
#define itkBSplineInterpolationWeightFunction_hxx

#include "itkBSplineInterpolationWeightFunction.h"
#include "itkImage.h"
#include "itkMatrix.h"
#include "itkMath.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/** Constructor */
template< typename TCoordRep, unsigned int VSpaceDimension,
          unsigned int VSplineOrder >
BSplineInterpolationWeightFunction< TCoordRep, VSpaceDimension, VSplineOrder >
::BSplineInterpolationWeightFunction()
{
  // Initialize the number of weights;
  m_NumberOfWeights =
    static_cast< unsigned int >( std::pow( static_cast< double >( SplineOrder + 1 ),
                                          static_cast< double >( SpaceDimension ) ) );

  // Initialize support region is a hypercube of length SplineOrder + 1
  m_SupportSize.Fill(SplineOrder + 1);

  // Initialize offset to index lookup table
  m_OffsetToIndexTable.set_size(m_NumberOfWeights, SpaceDimension);

  typedef Image< char, SpaceDimension > CharImageType;
  typename CharImageType::Pointer tempImage = CharImageType::New();
  tempImage->SetRegions(m_SupportSize);
  tempImage->Allocate(true); // initialize buffer to zero

  typedef ImageRegionConstIteratorWithIndex< CharImageType > IteratorType;
  IteratorType iterator( tempImage, tempImage->GetBufferedRegion() );
  unsigned int counter = 0;

  while ( !iterator.IsAtEnd() )
    {
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      m_OffsetToIndexTable[counter][j] = iterator.GetIndex()[j];
      }
    ++counter;
    ++iterator;
    }

  // Initialize the interpolation kernel
  m_Kernel = KernelType::New();
}

/**
 * Standard "PrintSelf" method
 */
template< typename TCoordRep, unsigned int VSpaceDimension,
          unsigned int VSplineOrder >
void
BSplineInterpolationWeightFunction< TCoordRep, VSpaceDimension, VSplineOrder >
::PrintSelf(
  std::ostream & os,
  Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfWeights: " << m_NumberOfWeights << std::endl;
  os << indent << "SupportSize: " << m_SupportSize << std::endl;
}

/** Compute weights for interpolation at continuous index position */
template< typename TCoordRep, unsigned int VSpaceDimension,
          unsigned int VSplineOrder >
typename BSplineInterpolationWeightFunction< TCoordRep, VSpaceDimension,
                                             VSplineOrder >
::WeightsType
BSplineInterpolationWeightFunction< TCoordRep, VSpaceDimension, VSplineOrder >
::Evaluate(
  const ContinuousIndexType & index) const
{
  WeightsType weights(m_NumberOfWeights);
  IndexType   startIndex;

  this->Evaluate(index, weights, startIndex);

  return weights;
}

/** Compute weights for interpolation at continuous index position */
template< typename TCoordRep, unsigned int VSpaceDimension,
          unsigned int VSplineOrder >
void BSplineInterpolationWeightFunction< TCoordRep, VSpaceDimension,
                                         VSplineOrder >
::Evaluate(
  const ContinuousIndexType & index,
  WeightsType & weights,
  IndexType & startIndex) const
{
  unsigned int j, k;

  // Find the starting index of the support region
  for ( j = 0; j < SpaceDimension; j++ )
    {
    startIndex[j] = Math::Floor< IndexValueType >(index[j] - static_cast< double >( SplineOrder - 1 ) / 2.0);
    }

  // Compute the weights
  Matrix< double, SpaceDimension, SplineOrder + 1 > weights1D;
  for ( j = 0; j < SpaceDimension; j++ )
    {
    double x = index[j] - static_cast< double >( startIndex[j] );

    for ( k = 0; k <= SplineOrder; k++ )
      {
      weights1D[j][k] = m_Kernel->Evaluate(x);
      x -= 1.0;
      }
    }

  for ( k = 0; k < m_NumberOfWeights; k++ )
    {
    weights[k] = 1.0;

    for ( j = 0; j < SpaceDimension; j++ )
      {
      weights[k] *= weights1D[j][m_OffsetToIndexTable[k][j]];
      }
    }
}
} // end namespace itk

#endif
