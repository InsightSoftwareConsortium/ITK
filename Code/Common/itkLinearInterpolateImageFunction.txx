/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLinearInterpolateImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLinearInterpolateImageFunction_txx
#define __itkLinearInterpolateImageFunction_txx

#include "itkLinearInterpolateImageFunction.h"

#include "vnl/vnl_math.h"

namespace itk
{
/**
 * Define the number of neighbors
 */
template< class TInputImage, class TCoordRep >
const unsigned long
LinearInterpolateImageFunction< TInputImage, TCoordRep >
::m_Neighbors = 1 << TInputImage::ImageDimension;

/**
 * Constructor
 */
template< class TInputImage, class TCoordRep >
LinearInterpolateImageFunction< TInputImage, TCoordRep >
::LinearInterpolateImageFunction()
{}

template< class TInputImage, class TCoordRep >
LinearInterpolateImageFunction< TInputImage, TCoordRep >
::~LinearInterpolateImageFunction()
{}

/**
 * PrintSelf
 */
template< class TInputImage, class TCoordRep >
void
LinearInterpolateImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}

/**
 * Evaluate at image index position
 */
template< class TInputImage, class TCoordRep >
typename LinearInterpolateImageFunction< TInputImage, TCoordRep >
::OutputType
LinearInterpolateImageFunction< TInputImage, TCoordRep >
::EvaluateUnoptimized(const ContinuousIndexType & index) const
{
  unsigned int dim;  // index over dimension

  /**
   * Compute base index = closet index below point
   * Compute distance from point to base index
   */
  IndexType baseIndex;
  double    distance[ImageDimension];

  for ( dim = 0; dim < ImageDimension; dim++ )
    {
    baseIndex[dim] = Math::Floor< IndexValueType >(index[dim]);
    distance[dim] = index[dim] - static_cast< double >( baseIndex[dim] );
    }

  /**
   * Interpolated value is the weighted sum of each of the surrounding
   * neighbors. The weight for each neighbor is the fraction overlap
   * of the neighbor pixel with respect to a pixel centered on point.
   */
  RealType value = NumericTraits< RealType >::Zero;

  typedef typename NumericTraits< InputPixelType >::ScalarRealType ScalarRealType;
  ScalarRealType totalOverlap = NumericTraits< ScalarRealType >::Zero;

  for ( unsigned int counter = 0; counter < m_Neighbors; counter++ )
    {
    double       overlap = 1.0;    // fraction overlap
    unsigned int upper = counter;  // each bit indicates upper/lower neighbour
    IndexType    neighIndex;

    // get neighbor index and overlap fraction
    for ( dim = 0; dim < ImageDimension; dim++ )
      {
      if ( upper & 1 )
        {
        neighIndex[dim] = baseIndex[dim] + 1;
        // Take care of the case where the pixel is just
        // in the outer upper boundary of the image grid.
        if ( neighIndex[dim] > this->m_EndIndex[dim] )
          {
          neighIndex[dim] = this->m_EndIndex[dim];
          }
        overlap *= distance[dim];
        }
      else
        {
        neighIndex[dim] = baseIndex[dim];
        // Take care of the case where the pixel is just
        // in the outer lower boundary of the image grid.
        if ( neighIndex[dim] < this->m_StartIndex[dim] )
          {
          neighIndex[dim] = this->m_StartIndex[dim];
          }
        overlap *= 1.0 - distance[dim];
        }

      upper >>= 1;
      }

    // get neighbor value only if overlap is not zero
    if ( overlap )
      {
      value += static_cast< RealType >( this->GetInputImage()->GetPixel(neighIndex) ) * overlap;
      totalOverlap += overlap;
      }

    if ( totalOverlap == 1.0 )
      {
      // finished
      break;
      }
    }

  return ( static_cast< OutputType >( value ) );
}
} // end namespace itk

#endif
