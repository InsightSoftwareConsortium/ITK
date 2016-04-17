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
#ifndef itkBresenhamLine_hxx
#define itkBresenhamLine_hxx

#include "itkBresenhamLine.h"
#include "itkPoint.h"
#include "itkMath.h"
#include "itkMath.h"

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
    IndexValueType distance = static_cast<long>(itk::Math::abs(LastIndex[i]));
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

template< unsigned int VDimension >
typename BresenhamLine< VDimension >::IndexArray BresenhamLine< VDimension >
::BuildLine(IndexType p0, IndexType p1)
{
  itk::Point<float,VDimension> point0;
  itk::Point<float,VDimension> point1;
  for(unsigned int i = 0; i < VDimension; i++)
    {
    point0[i] = p0[i];
    point1[i] = p1[i];
    }

  const unsigned int distance = itk::Math::RoundHalfIntegerToEven<unsigned int, double>( point0.EuclideanDistanceTo(point1) );
  OffsetArray offsets = this->BuildLine(point1-point0, distance);

  IndexArray indices(offsets.size());
  for(unsigned int i = 0; i < offsets.size(); i++)
    {
    indices[i] = p0 + offsets[i];
    }
  return indices;
}

} // namespace itk

#endif
