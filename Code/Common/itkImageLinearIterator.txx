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

// #include "itkImageRegionSimpleIterator.h"

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
    if( m_PositionIndex[n] < (int)( m_Region.GetSize()[n] ))
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
//  Test for end of line
//----------------------------------------------------------------------
template<class TImage>
bool 
ImageLinearIterator<TImage>
::IsAtEndOfLine(void) 
{
  return m_PositionIndex[m_Direction] >= (int)m_Region.GetSize()[m_Direction];
}



//----------------------------------------------------------------------
//  Set direction of movement
//----------------------------------------------------------------------
template<class TImage>
void 
ImageLinearIterator<TImage>
::SetDirection(unsigned int direction) 
{
  if( direction >= TImage::ImageDimension )
  {
    throw itk::ExceptionObject();
  }
  m_Direction = direction;
  m_Jump = m_OffsetTable[ m_Direction ];
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
