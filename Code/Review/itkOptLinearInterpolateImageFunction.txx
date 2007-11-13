/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptLinearInterpolateImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOptLinearInterpolateImageFunction_txx
#define __itkOptLinearInterpolateImageFunction_txx

#include "itkOptLinearInterpolateImageFunction.h"

#include "vnl/vnl_math.h"

namespace itk
{

/**
 * Define the number of neighbors
 */
template<class TInputImage, class TCoordRep>
const unsigned long
OptLinearInterpolateImageFunction< TInputImage, TCoordRep >
::m_Neighbors = 1 << TInputImage::ImageDimension;


/**
 * Constructor
 */
template<class TInputImage, class TCoordRep>
OptLinearInterpolateImageFunction< TInputImage, TCoordRep >
::OptLinearInterpolateImageFunction()
{
}

template<class TInputImage, class TCoordRep>
OptLinearInterpolateImageFunction< TInputImage, TCoordRep >
::~OptLinearInterpolateImageFunction()
{
}

/**
 * PrintSelf
 */
template<class TInputImage, class TCoordRep>
void
OptLinearInterpolateImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
}


/**
 * Evaluate at image index position
 */
template<class TInputImage, class TCoordRep>
typename OptLinearInterpolateImageFunction< TInputImage, TCoordRep >
::OutputType
OptLinearInterpolateImageFunction< TInputImage, TCoordRep >
::EvaluateUnoptimized( const ContinuousIndexType& index) const
{
  unsigned int dim;  // index over dimension

  /**
   * Compute base index = closet index below point
   * Compute distance from point to base index
   */
  signed long baseIndex[ImageDimension];
  double distance[ImageDimension];
  long tIndex;

  for( dim = 0; dim < ImageDimension; dim++ )
    {
    // The following "if" block is equivalent to the following line without
    // having to call floor.
    //    baseIndex[dim] = (long) vcl_floor(index[dim] );
    if (index[dim] >= 0.0)
      {
      baseIndex[dim] = (long) index[dim];
      }
    else
      {
      tIndex = (long) index[dim];
      if (double(tIndex) != index[dim])
        {
        tIndex--;
        }
      baseIndex[dim] = tIndex;
      }
    distance[dim] = index[dim] - static_cast< RealType >( baseIndex[dim] );
    }
  
  /**
   * Interpolated value is the weighted sum of each of the surrounding
   * neighbors. The weight for each neighbor is the fraction overlap
   * of the neighbor pixel with respect to a pixel centered on point.
   */
  RealType value = NumericTraits<RealType>::Zero;
  RealType totalOverlap = NumericTraits<RealType>::Zero;

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
    
    // get neighbor value only if overlap is not zero
    if( overlap )
      {
      value += overlap * this->GetInputImage()->GetPixel( neighIndex );
      totalOverlap += overlap;
      }

    if( totalOverlap == 1.0 )
      {
      // finished
      break;
      }

    }

  return ( static_cast<OutputType>( value ) );
}

} // end namespace itk

#endif
