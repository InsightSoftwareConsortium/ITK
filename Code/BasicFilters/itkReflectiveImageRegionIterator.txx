/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectiveImageRegionIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkReflectiveImageRegionIterator_txx
#define _itkReflectiveImageRegionIterator_txx


#include "itkReflectiveImageRegionIterator.h"

namespace itk
{

template<class TImage>
ReflectiveImageRegionIterator<TImage>
::ReflectiveImageRegionIterator()
{
  for(unsigned int i=0;i<TImage::ImageDimension;i++) 
    {
    m_Reflecting[i] = false;
    m_Done[i] = false;
    }
  m_Fastest = 0;
  m_Current = 0;
}

template<class TImage>
ReflectiveImageRegionIterator<TImage>
::ReflectiveImageRegionIterator(TImage *ptr, const RegionType& region) :
  ImageIteratorWithIndex<TImage>(ptr, region)
{
  for(unsigned int i=0;i<TImage::ImageDimension;i++) 
    {
    m_Reflecting[i] = false;
    m_Done[i] = false;
    }
  m_Fastest = 0;
  m_Current = 0;
}

template<class TImage>
bool
ReflectiveImageRegionIterator<TImage>
::IsReflected(unsigned int dim) const
{
  return m_Reflecting[ dim ];
}

template<class TImage>
void
ReflectiveImageRegionIterator<TImage>
::IncrementLoop(unsigned int dim)
{
  if (dim == TImage::ImageDimension)
    {
    return;
    }
  if (dim != m_Fastest)
    {
    if (m_PositionIndex[dim] < m_EndIndex[dim])
      {
      m_PositionIndex[dim]++;
      }
    if (m_PositionIndex[dim] == m_EndIndex[dim])
      {
      m_Done[dim] = true;
      m_PositionIndex[dim] = m_BeginIndex[dim];
      this->IncrementLoop(dim + 1);
      }
    }
  else
    {
    this->IncrementLoop(dim + 1);
    }
}

//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template<class TImage>
ReflectiveImageRegionIterator<TImage> &
ReflectiveImageRegionIterator<TImage>
::operator++()
{
  int i;

  // which way should the inner most index be moved?
  if (m_Reflecting[m_Fastest])
    {
    m_PositionIndex[m_Fastest]--;
    }
  else
    {
    m_PositionIndex[m_Fastest]++;
    }

  // finished forward?
  if (m_PositionIndex[m_Fastest] == m_EndIndex[m_Fastest])
    {
    m_Reflecting[m_Fastest] = true;
    m_PositionIndex[m_Fastest]--;
    }
  // finished backward?
  else if (m_PositionIndex[m_Fastest] < m_BeginIndex[m_Fastest])
    {
    m_Reflecting[m_Fastest] = false;
    m_PositionIndex[m_Fastest] = m_BeginIndex[m_Fastest];
    m_Done[m_Fastest] = true;
    this->IncrementLoop(0);
    }

  // shall we move the fastest index
  int done;
  for(done = 0, i = 0; i < TImage::ImageDimension; i++) 
    {
    if (m_Done[i])
      {
      done++;
      }
    }
  if (done == TImage::ImageDimension)
    {
    m_Fastest++;
    if (m_Fastest < TImage::ImageDimension)
      {
      m_Reflecting[m_Fastest] = false;
      m_PositionIndex[m_Fastest] = m_BeginIndex[m_Fastest];
      for(i = 0; i < TImage::ImageDimension; i++) 
        {
        m_Done[i] = false;
        }
      }
    }

  if (m_Fastest < TImage::ImageDimension)
    {
    this->SetIndex(m_PositionIndex);
    }
  else
    {
    m_Position = m_End;
    m_Remaining = false;
    }

  return *this;
}

} // end namespace itk

#endif
