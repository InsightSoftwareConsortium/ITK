/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBresenhamLine.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBresenhamLine_txx
#define __itkBresenhamLine_txx

#include "itkBresenhamLine.h"

namespace itk
{
template< unsigned int VDimension >
typename BresenhamLine< VDimension >::OffsetArray BresenhamLine< VDimension >
::BuildLine(LType Direction, unsigned int length)
{
  // copied from the line iterator
  /** Variables that drive the Bresenham-Algorithm */
  // The dimension with the largest difference between start and end
  unsigned int m_MainDirection;

  // Accumulated error for the other dimensions
  IndexType m_AccumulateError;

  // Increment for the error for each step. Two times the difference between
  // start and end
  IndexType m_IncrementError;

  // If enough is accumulated for a dimension, the index has to be
  // incremented. Will be the number of pixels in the line
  IndexType m_MaximalError;

  // Direction of increment. -1 or 1
  IndexType m_OverflowIncrement;

  // After an overflow, the accumulated error is reduced again. Will be
  // two times the number of pixels in the line
  IndexType m_ReduceErrorAfterIncrement;

  OffsetArray result(length);

  IndexType m_CurrentImageIndex, StartIndex, LastIndex;

  Direction.Normalize();
  // we are going to start at 0
  m_CurrentImageIndex.Fill(0);
  StartIndex.Fill(0);
  for ( unsigned i = 0; i < VDimension; i++ )
    {
    LastIndex[i] = (IndexValueType)( length * Direction[i] );
    }
  // Find the dominant direction
  IndexValueType maxDistance = 0;
  unsigned int   maxDistanceDimension = 0;
  for ( unsigned i = 0; i < VDimension; i++ )
    {
    IndexValueType distance = abs(LastIndex[i]);
    if ( distance > maxDistance )
      {
      maxDistance = distance;
      maxDistanceDimension = i;
      }
    m_IncrementError[i] = 2 * distance;
    m_OverflowIncrement[i] = ( LastIndex[i] < 0 ? -1 : 1 );
    }
  m_MainDirection = maxDistanceDimension;
  m_MaximalError.Fill(maxDistance);
  m_ReduceErrorAfterIncrement.Fill(2 * maxDistance);
  m_AccumulateError.Fill(0);
  unsigned int steps = 1;
  result[0] = m_CurrentImageIndex - StartIndex;
  while ( steps < length )
    {
    // This part is from ++ in LineConstIterator
    // We need to modify m_AccumulateError, m_CurrentImageIndex, m_IsAtEnd
    for ( unsigned int i = 0; i < VDimension; ++i )
      {
      if ( i == m_MainDirection )
        {
        m_CurrentImageIndex[i] += m_OverflowIncrement[i];
        }
      else
        {
        m_AccumulateError[i] += m_IncrementError[i];
        if ( m_AccumulateError[i] >= m_MaximalError[i] )
          {
          m_CurrentImageIndex[i] += m_OverflowIncrement[i];
          m_AccumulateError[i] -= m_ReduceErrorAfterIncrement[i];
          }
        }
      }

    result[steps] = m_CurrentImageIndex - StartIndex; // produce an offset

    ++steps;
    }
  return ( result );
}
} // namespace itk

#endif
