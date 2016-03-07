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
#ifndef itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction_hxx
#define itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction_hxx

#include "itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction.h"

#include "itkMath.h"

namespace itk
{
/**
 * Define the number of neighbors
 */
template< typename TInputImage, typename TCoordRep >
const unsigned int
VectorLinearInterpolateNearestNeighborExtrapolateImageFunction< TInputImage, TCoordRep >
::m_Neighbors = 1 << TInputImage::ImageDimension;

/**
 * Constructor
 */
template< typename TInputImage, typename TCoordRep >
VectorLinearInterpolateNearestNeighborExtrapolateImageFunction< TInputImage, TCoordRep >
::VectorLinearInterpolateNearestNeighborExtrapolateImageFunction()
{}

/**
 * PrintSelf
 */
template< typename TInputImage, typename TCoordRep >
void
VectorLinearInterpolateNearestNeighborExtrapolateImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}

/**
 * Evaluate at image index position
 */
template< typename TInputImage, typename TCoordRep >
typename VectorLinearInterpolateNearestNeighborExtrapolateImageFunction< TInputImage, TCoordRep >
::OutputType
VectorLinearInterpolateNearestNeighborExtrapolateImageFunction< TInputImage, TCoordRep >
::EvaluateAtContinuousIndex(const ContinuousIndexType & index) const
{
  unsigned int dim;  // index over dimension

  /**
   * Compute base index = closest index below point
   * Compute distance from point to base index
   */
  IndexType baseIndex;
  IndexType neighIndex;
  double    distance[ImageDimension];

  for ( dim = 0; dim < ImageDimension; dim++ )
    {
    baseIndex[dim] = Math::Floor< IndexValueType >(index[dim]);

    if ( baseIndex[dim] >=  this->m_StartIndex[dim] )
      {
      if ( baseIndex[dim] <  this->m_EndIndex[dim] )
        {
        distance[dim] = index[dim] - static_cast< double >( baseIndex[dim] );
        }
      else
        {
        baseIndex[dim] = this->m_EndIndex[dim];
        distance[dim] = 0.0;
        }
      }
    else
      {
      baseIndex[dim] = this->m_StartIndex[dim];
      distance[dim] = 0.0;
      }
    }

  /**
   * Interpolated value is the weight some of each of the surrounding
   * neighbors. The weight for each neighbour is the fraction overlap
   * of the neighbor pixel with respect to a pixel centered on point.
   */
  OutputType output;
  NumericTraits<OutputType>::SetLength( output, this->GetInputImage()->GetNumberOfComponentsPerPixel() );
  output.Fill(0.0);

  RealType totalOverlap = 0.0;

  for ( unsigned int counter = 0; counter < m_Neighbors; counter++ )
    {
    double       overlap = 1.0;    // fraction overlap
    unsigned int upper = counter;  // each bit indicates upper/lower neighbour

    // get neighbor index and overlap fraction
    for ( dim = 0; dim < ImageDimension; dim++ )
      {
      if ( upper & 1 )
        {
        neighIndex[dim] = baseIndex[dim] + 1;
        overlap *= distance[dim];
        }
      else
        {
        neighIndex[dim] = baseIndex[dim];
        overlap *= 1.0 - distance[dim];
        }

      upper >>= 1;
      }

    // get neighbor value only if overlap is not zero
    if ( overlap )
      {
      const PixelType input = this->GetInputImage()->GetPixel(neighIndex);
      for ( unsigned int k = 0; k < this->GetInputImage()->GetNumberOfComponentsPerPixel(); k++ )
        {
        output[k] += overlap * static_cast< RealType >( input[k] );
        }
      totalOverlap += overlap;
      }

    if ( totalOverlap == 1.0 )
      {
      // finished
      break;
      }
    }

  return ( output );
}

/**
 * Evaluate at image index position
 */
template< typename TInputImage, typename TCoordRep >
typename VectorLinearInterpolateNearestNeighborExtrapolateImageFunction< TInputImage, TCoordRep >
::OutputType
VectorLinearInterpolateNearestNeighborExtrapolateImageFunction< TInputImage, TCoordRep >
::EvaluateAtIndex(const IndexType & index) const
{
  // Find the index that is closest to the requested one
  // but that lies within the image
  IndexType insideIndex;

  for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    if ( index[dim] >=  this->m_StartIndex[dim] )
      {
      if ( index[dim] <  this->m_EndIndex[dim] )
        {
        insideIndex[dim] = index[dim];
        }
      else
        {
        insideIndex[dim] = this->m_EndIndex[dim];
        }
      }
    else
      {
      insideIndex[dim] = this->m_StartIndex[dim];
      }
    }

  // Now call the superclass implementation of EvaluateAtIndex
  // since we have ensured that the index lies in the image region
  return this->Superclass::EvaluateAtIndex(insideIndex);
}
} // end namespace itk

#endif
