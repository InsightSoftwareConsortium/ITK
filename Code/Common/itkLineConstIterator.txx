/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLineConstIterator_txx
#define _itkLineConstIterator_txx

#include "itkLineConstIterator.h"

namespace itk
{

template<class TImage>
LineConstIterator<TImage>
::LineConstIterator(const ImageType *imagePtr, const IndexType &firstIndex, const IndexType &lastIndex)
{
  unsigned int i;
  
  m_Image = imagePtr;

  m_StartIndex = firstIndex;
  m_LastIndex = lastIndex;

  IndexType difference;
  for (i = 0; i < TImage::ImageDimension; ++i)
    {
    difference[i] = lastIndex[i] - firstIndex[i];
    }

  IndexValueType maxDistance = 0;
  int maxDistanceDimension = 0;
  for (i = 0; i < TImage::ImageDimension; ++i)
    {
    IndexValueType distance = vcl_abs(difference[i]);
    if (distance > maxDistance)
      {
      maxDistance = distance;
      maxDistanceDimension = i;
      }
    m_IncrementError[i] = 2*distance;
    m_OverflowIncrement[i] = (difference[i] < 0 ? -1 : 1);
    }
  m_MainDirection = maxDistanceDimension;
  m_MaximalError.Fill(maxDistance);
  m_ReduceErrorAfterIncrement.Fill(2*maxDistance);

  // Need to set m_EndIndex to be one pixel past the m_LastIndex along
  // the bresenham line. THis is tricky to
  // do. m_EndIndex[m_MainDirection] is always offset by one from the
  // m_LastIndex[m_MainDirection].  The other indices may or may not
  // be incremented depending on the accumulated error to that point.
  //
  // To avoid traversing the line to determine whether the other
  // components need to be adjusted, we merely set the main direction
  // to be incremented and keep the remaining indices to be same as
  // LastIndex. THen in the test for IsAtEnd, we just check the
  // MainDirection component of the index.
  for (i = 0; i < TImage::ImageDimension; ++i)
    {
    if (i == m_MainDirection)
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


template<class TImage>
LineConstIterator<TImage> &
LineConstIterator<TImage>
::operator=(const Self & it)
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
  
  return *this;
}


template<class TImage>
void
LineConstIterator<TImage>
::GoToBegin()
{
  m_CurrentImageIndex   = m_StartIndex;
  m_AccumulateError.Fill(0);
  m_IsAtEnd = (m_StartIndex[m_MainDirection] == m_EndIndex[m_MainDirection]);
}


template<class TImage>
void
LineConstIterator<TImage>
::operator++()
{
  // We need to modify m_AccumulateError, m_CurrentImageIndex, m_IsAtEnd
  for (unsigned int i = 0; i < TImage::ImageDimension; ++i)
    {
    if (i == m_MainDirection)
      {
      m_CurrentImageIndex[i] += m_OverflowIncrement[i];
      }
    else
      {
      m_AccumulateError[i] += m_IncrementError[i];
      if (m_AccumulateError[i] >= m_MaximalError[i])
        {
        m_CurrentImageIndex[i] += m_OverflowIncrement[i];
        m_AccumulateError[i] -= m_ReduceErrorAfterIncrement[i];
        }
      }
    }

  if (m_CurrentImageIndex[m_MainDirection] == m_EndIndex[m_MainDirection])
    {
    m_IsAtEnd = true;
    }
  else if( ! m_Region.IsInside( m_CurrentImageIndex ) )
    {
    // The new index is outside the acceptable region.  We can iterate no
    // farther, call this the end.  NOTE THAT INPUT IS STILL INCREMENTED.
    m_IsAtEnd = true;
    itkWarningMacro(<<"Line left region; unable to finish tracing it");
    }
}

} // end namespace itk

#endif
