/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleImageRegionIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkSimpleImageRegionIterator_txx
#define _itkSimpleImageRegionIterator_txx

// #include "itkSimpleImageRegionIterator.h"

namespace itk
{
  
//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template<class TImage>
SimpleImageRegionIterator<TImage> &
SimpleImageRegionIterator<TImage>
::operator++()
{
  
  m_Remaining = false;
  for( unsigned int in=0; in<TImage::ImageDimension; in++ )
  {
    m_PositionIndex[ in  ]++;
    if( m_PositionIndex[ in ] < m_EndIndex[ in ] )
    {
      m_Position += m_OffsetTable[in];
      m_Remaining = true;
      break;
    }
    else 
    {
      m_Position -= m_OffsetTable[ in ] * ( m_Region.GetSize()[in]-1 );
      m_PositionIndex[ in ] = m_BeginIndex[ in ]; 
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
