/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionExclusionConstIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRegionExclusionConstIteratorWithIndex_txx
#define _itkImageRegionExclusionConstIteratorWithIndex_txx

#include "itkImageRegionExclusionConstIteratorWithIndex.h"

namespace itk
{
  

   
//----------------------------------------------------------------------
//  Set the region to exclude from the walk
//----------------------------------------------------------------------
template<class TImage>
void
ImageRegionExclusionConstIteratorWithIndex<TImage>
::SetExclusionRegion( const RegionType & region )
{
  
  if( !m_Region.IsInside( region ) )
    {
    itkGenericExceptionMacro(<< "Attempt to set a exclusion region that is NOT contained inside the iterator region");
    }
  m_ExclusionRegion      = region;
  m_ExclusionBegin       = m_ExclusionRegion.GetIndex();
  SizeType exclusionSize = m_ExclusionRegion.GetSize();
  
  for(unsigned int i=0; i<TImage::ImageDimension; ++i)
    {
    m_ExclusionEnd[i] = m_ExclusionBegin[i] + exclusionSize[i];
    }

}

 

//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template<class TImage>
ImageRegionExclusionConstIteratorWithIndex<TImage> &
ImageRegionExclusionConstIteratorWithIndex<TImage>
::operator++()
{
  
  m_Remaining = false;
  for( unsigned int in=0; in<TImage::ImageDimension; in++ )
    {
    m_PositionIndex[ in  ]++;
    
    // if entering the exclusion region... jump over it
    if( m_ExclusionRegion.IsInside( m_PositionIndex ) )
      {
      m_PositionIndex[ in ]  = m_ExclusionEnd[ in ];
      m_Position            += m_OffsetTable[in] * 
        m_ExclusionRegion.GetSize()[ in ];
      }

    if( m_PositionIndex[ in ] < m_EndIndex[ in ] )
      {
      m_Position += m_OffsetTable[in];
      m_Remaining = true;
      break;
      }
    else 
      {
      m_Position -= m_OffsetTable[ in ] * ( static_cast<long>(m_Region.GetSize()[in])-1 );
      m_PositionIndex[ in ] = m_BeginIndex[ in ]; 
      }
    }

  if( !m_Remaining ) // It will not advance here otherwise
    {
    m_Position = m_End;
    }

  return *this;
}


//----------------------------------------------------------------------
//  Advance along the line in reverse direction
//----------------------------------------------------------------------
template<class TImage>
ImageRegionExclusionConstIteratorWithIndex<TImage> &
ImageRegionExclusionConstIteratorWithIndex<TImage>
::operator--()
{
  
  m_Remaining = false;
  for( unsigned int in=0; in<TImage::ImageDimension; in++ )
    {
      
    if( m_PositionIndex[ in ] > m_BeginIndex[ in ] )
      {

      m_PositionIndex[ in  ]--;
      m_Position -= m_OffsetTable[in];

      // if entering the exclusion region... jump over it
      if( m_ExclusionRegion.IsInside( m_PositionIndex ) )
        {
        m_PositionIndex[ in ]  = m_ExclusionBegin[ in ]-1;
        m_Position            -= m_OffsetTable[in] * m_ExclusionRegion.GetSize()[ in ];
        }

      m_Remaining = true;
      break;
      }
    else 
      {
      m_PositionIndex[ in  ]--;
      m_Position += m_OffsetTable[ in ] * ( static_cast<long>(m_Region.GetSize()[in])-1 );
      m_PositionIndex[ in ] = m_EndIndex[ in ] - 1; 
      }

    }

  if( !m_Remaining ) // It will not advance here otherwise
    {
    m_Position = m_End;
    }

  return *this;
}

} // end namespace itk

#endif
