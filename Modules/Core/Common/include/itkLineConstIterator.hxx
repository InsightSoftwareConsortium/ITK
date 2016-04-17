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
#ifndef itkLineConstIterator_hxx
#define itkLineConstIterator_hxx

#include "itkLineConstIterator.h"

namespace itk
{
template< typename TImage >
LineConstIterator< TImage >
::LineConstIterator(const ImageType *imagePtr, const IndexType & firstIndex, const IndexType & lastIndex)
{

  m_Image = imagePtr;

  m_StartIndex = firstIndex;
  m_LastIndex = lastIndex;

  IndexType difference;
  for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
    {
    difference[i] = lastIndex[i] - firstIndex[i];
    }

  IndexValueType maxDistance = 0;
  unsigned int maxDistanceDimension = 0;
  for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
    {
    IndexValueType distance = itk::Math::abs(difference[i]);
    if ( distance > maxDistance )
      {
      maxDistance = distance;
      maxDistanceDimension = i;
      }
    m_IncrementError[i] = 2 * distance;
    m_OverflowIncrement[i] = ( difference[i] < 0 ? -1 : 1 );
    }
  m_MainDirection = maxDistanceDimension;
  m_MaximalError.Fill(maxDistance);
  m_ReduceErrorAfterIncrement.Fill(2 * maxDistance);

  // Need to set m_EndIndex to be one pixel past the m_LastIndex along
  // the bresenham line. This is tricky to do.
  // m_EndIndex[m_MainDirection] is always offset by one from the
  // m_LastIndex[m_MainDirection].  The other indices may or may not
  // be incremented depending on the accumulated error to that point.
  //
  // To avoid traversing the line to determine whether the other
  // components need to be adjusted, we merely set the main direction
  // to be incremented and keep the remaining indices to be same as
  // LastIndex. Then in the test for IsAtEnd, we just check the
  // MainDirection component of the index.
  for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
    {
    if ( i == m_MainDirection )
      {
      m_EndIndex[i] = m_LastIndex[i] + m_OverflowIncrement[i];
      }
    else
      {
      m_EndIndex[i] = m_LastIndex[i];
      }
    }

  m_Region = m_Image->GetBufferedRegion();

  this->GoToBegin();
}

template< typename TImage >
LineConstIterator< TImage > &
LineConstIterator< TImage >
::operator=(const Self & it)
{
  if(this != &it)
    {
    m_Image  = it.m_Image;  // copy the smart pointer
    m_Region = it.m_Region;
    m_IsAtEnd = it.m_IsAtEnd;
    m_CurrentImageIndex   = it.m_CurrentImageIndex;
    m_StartIndex = it.m_StartIndex;
    m_LastIndex = it.m_LastIndex;
    m_EndIndex = it.m_EndIndex;
    m_MainDirection = it.m_MainDirection;
    m_AccumulateError = it.m_AccumulateError;
    m_IncrementError = it.m_IncrementError;
    m_MaximalError = it.m_MaximalError;
    m_OverflowIncrement = it.m_OverflowIncrement;
    m_ReduceErrorAfterIncrement = it.m_ReduceErrorAfterIncrement;
    }
  return *this;
}

template< typename TImage >
void
LineConstIterator< TImage >
::GoToBegin()
{
  m_CurrentImageIndex   = m_StartIndex;
  m_AccumulateError.Fill(0);
  m_IsAtEnd = ( m_StartIndex[m_MainDirection] == m_EndIndex[m_MainDirection] );
}

template< typename TImage >
void
LineConstIterator< TImage >
::operator++()
{
  // We need to modify m_AccumulateError, m_CurrentImageIndex, m_IsAtEnd
  for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
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

  if ( m_CurrentImageIndex[m_MainDirection] == m_EndIndex[m_MainDirection] )
    {
    m_IsAtEnd = true;
    }
  else if ( !m_Region.IsInside(m_CurrentImageIndex) )
    {
    // The new index is outside the acceptable region.  We can iterate no
    // farther, call this the end.  NOTE THAT INPUT IS STILL INCREMENTED.
    m_IsAtEnd = true;
    itkWarningMacro(<< "Line left region; unable to finish tracing it");
    }
}
} // end namespace itk

#endif
