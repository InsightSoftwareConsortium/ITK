/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageLinearIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageLinearIterator_txx
#define _itkImageLinearIterator_txx

// #include "itkSimpleImageRegionIterator.h"

namespace itk
{




//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
void 
ImageLinearIterator<TImage>
::NextLine(void)
{

  m_PositionIndex[m_Direction] = m_BeginIndex[m_Direction];   
  m_Position -= m_OffsetTable[ m_Direction+1 ]; 
  

  for( unsigned int n=0; n<TImage::ImageDimension; n++ )
  {

    m_Remaining = false;
    
    if( n == m_Direction ) 
    {
      continue;
    }
    
    m_PositionIndex[ n  ]++;
    if( m_PositionIndex[n] <  m_Region.GetSize()[n] )
    {
      m_Position += m_OffsetTable[ n ];
      m_Remaining = true;
      break;
    }
    else 
    {
      m_Position -= m_OffsetTable[ n ] * ( m_Region.GetSize()[n]-1 );
      m_PositionIndex[ n ] = m_BeginIndex[ n ]; 
    }
  }
}

//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template<class TImage>
ImageLinearIterator<TImage>  & 
ImageLinearIterator<TImage>
::operator++()
{
  m_PositionIndex[m_Direction]++;
  m_Position += m_Jump;
  return *this;
}




} // end namespace itk

#endif
