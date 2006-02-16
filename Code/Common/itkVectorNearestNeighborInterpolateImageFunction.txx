/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorNearestNeighborInterpolateImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorNearestNeighborInterpolateImageFunction_txx
#define __itkVectorNearestNeighborInterpolateImageFunction_txx

#include "itkVectorNearestNeighborInterpolateImageFunction.h"

#include "vnl/vnl_math.h"

namespace itk
{

/**
 * Define the number of neighbors
 */
template<class TInputImage, class TCoordRep>
const unsigned long
VectorNearestNeighborInterpolateImageFunction< TInputImage, TCoordRep >
::m_Neighbors = 1 << TInputImage::ImageDimension;


/**
 * Constructor
 */
template<class TInputImage, class TCoordRep>
VectorNearestNeighborInterpolateImageFunction< TInputImage, TCoordRep >
::VectorNearestNeighborInterpolateImageFunction()
{

}


/**
 * PrintSelf
 */
template<class TInputImage, class TCoordRep>
void
VectorNearestNeighborInterpolateImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
}


/**
 * Evaluate at image index position, no bounds checking....  
 * use IsInsideBuffer() if needed
 */

/**
 * The nearest neighbour is assumed to be the pixel that has the largest
* overlap with the pixel that is centered on the indicated point (defined
* by ContinuousIndexType& index.
* 
* We also achieve speedup by noting that if overlap is > 1/2 it
* is the nearest neighbour and if < 1/m_Neighbors, it isn't.
* If all overlaps are the same, we choose the last neighbour.
*/
template<class TInputImage, class TCoordRep>
typename VectorNearestNeighborInterpolateImageFunction< TInputImage, TCoordRep >
::OutputType
VectorNearestNeighborInterpolateImageFunction< TInputImage, TCoordRep >
::EvaluateAtContinuousIndex(
  const ContinuousIndexType& index) const 
{
  unsigned int dim;  // index over dimension

  /**
   * Compute base index = closest index below point
   * Compute distance from point to base index
   */
  signed long baseIndex[ImageDimension];
  double distance[ImageDimension];

  for( dim = 0; dim < ImageDimension; dim++ ) 
    {
    baseIndex[dim] = (long) floor( index[dim] );
    distance[dim] = index[dim] - double( baseIndex[dim] );
    }
  
  OutputType output;
  output.Fill( 0.0 );

  double uniformOverlap = (1/(static_cast <double> (m_Neighbors)));
  double currentMaxOverlap = 0.0; // max overlap until now

  for( unsigned int counter = 0; counter < m_Neighbors; counter++ )
    {

    double overlap = 1.0;          // fraction overlap
    unsigned int upper = counter;  // each bit indicates upper/lower neighbour
    IndexType neighIndex;

    // get neighbor index and overlap fraction
    for( dim = 0; dim < ImageDimension; dim++ )
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
    
    // get neighbor value only if overlap is greater than previous ones
    if((overlap > uniformOverlap) && (overlap > currentMaxOverlap) )
      {
      const PixelType input = this->GetInputImage()->GetPixel( neighIndex );
      for(unsigned int k = 0; k < Dimension; k++ )
        {
        output[k] = static_cast<RealType>( input[k] );
        }
      currentMaxOverlap = overlap;
      }

    if( currentMaxOverlap >= 0.5 )
      {
      // finished
      break;
      }
      
    /** 
     * If all overlaps are a the same, we adopt a uniform policy of choosing 
     * the last neighbor. Just a convention..
     */
    if( (counter == (m_Neighbors-1)) && (currentMaxOverlap == uniformOverlap) )
      {
      const PixelType input = this->GetInputImage()->GetPixel( neighIndex );
      for(unsigned int k = 0; k < Dimension; k++ )
        {
        output[k] = static_cast<RealType>( input[k] );
        }
      }
    }

  return ( output );
}

} // end namespace itk

#endif
