/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionConstIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRegionConstIteratorWithIndex_txx
#define _itkImageRegionConstIteratorWithIndex_txx

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
  
//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template<class TImage>
ImageRegionConstIteratorWithIndex<TImage> &
ImageRegionConstIteratorWithIndex<TImage>
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
      m_Position -= m_OffsetTable[ in ]
        * ( static_cast<long>(m_Region.GetSize()[in])-1 );
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
ImageRegionConstIteratorWithIndex<TImage> &
ImageRegionConstIteratorWithIndex<TImage>
::operator--()
{
  
  m_Remaining = false;
  for( unsigned int in=0; in<TImage::ImageDimension; in++ )
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
      m_Position += m_OffsetTable[ in ]
        * ( static_cast<long>(m_Region.GetSize()[in])-1 );
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
