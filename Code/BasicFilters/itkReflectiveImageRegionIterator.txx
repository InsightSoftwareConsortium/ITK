/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectiveImageRegionIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkReflectiveImageRegionIterator_txx
#define _itkReflectiveImageRegionIterator_txx


#include "itkReflectiveImageRegionIterator.h"

namespace itk
{

template<class TImage>
ReflectiveImageRegionIterator<TImage>
::ReflectiveImageRegionIterator() : ImageIteratorWithIndex<TImage>() 
{
  for(unsigned int i=0;i<TImage::ImageDimension;i++) 
  {
    m_IsFirstPass[i] = true;
  }

}

template<class TImage>
bool
ReflectiveImageRegionIterator<TImage>
::IsReflected(unsigned int dim) const
{
  return !m_IsFirstPass[ dim ];
}


//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template<class TImage>
ReflectiveImageRegionIterator<TImage> &
ReflectiveImageRegionIterator<TImage>
::operator++()
{
  
  m_Remaining = false;
  for( unsigned int in=0; in<TImage::ImageDimension; in++ )
  {    
    if( m_IsFirstPass[ in ] ) 
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
        m_PositionIndex[ in ] = m_EndIndex[ in ]-1; 
		m_IsFirstPass[ in ] = false;
		m_Remaining = true;
		break;
      }
	}
	else 
	{
      
      if( m_PositionIndex[ in ] > m_BeginIndex[ in ] )
      {
	    m_PositionIndex[ in  ]--;
        m_Position -= m_OffsetTable[in];
        m_Remaining = true;
        break;
      }
      else 
      {
	    m_PositionIndex[ in  ]--;
        m_Position -= m_OffsetTable[ in ] * ( m_Region.GetSize()[in]-1 );
        m_PositionIndex[ in ] = m_BeginIndex[ in ]; 
		m_IsFirstPass[ in ] = true;
      }

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
