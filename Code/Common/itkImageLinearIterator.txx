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
template<class TPixel, unsigned int VImageDimension>
void 
ImageLinearIterator<TPixel, VImageDimension>
::NextLine(void)
{

  m_PositionIndex[m_Direction] = m_BeginIndex[m_Direction];   
  m_Position -= m_OffsetTable[ m_Direction+1 ]; 
  

  for( unsigned int n=0; n<VImageDimension; n++ )
  {

    m_Remaining = false;
    
    if( n == m_Direction ) 
    {
      continue;
    }
    
    m_PositionIndex[ n  ]++;
    if( m_PositionIndex[n] < (int)( m_Size[n] ))
    {
      m_Position += m_OffsetTable[ n ];
      m_Remaining = true;
      break;
    }
    else 
    {
      m_Position -= m_OffsetTable[ n+1 ] - m_OffsetTable[ n ];
      m_PositionIndex[ n ] = m_BeginIndex[ n ]; 
    }
  }
}




//----------------------------------------------------------------------
//  Test for end of line
//----------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
bool 
ImageLinearIterator<TPixel, VImageDimension>
::IsAtEndOfLine(void) 
{
  return m_PositionIndex[m_Direction] >= (int)m_Size[m_Direction];
}



//----------------------------------------------------------------------
//  Set direction of movement
//----------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
ImageLinearIterator<TPixel, VImageDimension>
::SetDirection(unsigned int direction) 
{
  if( direction >= VImageDimension )
  {
    throw itk::ExceptionObject();
  }
  m_Direction = direction;
  m_Jump = m_OffsetTable[ m_Direction ];
}
 


//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
ImageLinearIterator<TPixel, VImageDimension>  & 
ImageLinearIterator<TPixel, VImageDimension>
::operator++()
{
  m_PositionIndex[m_Direction]++;
  m_Position += m_Jump;
  return *this;
}




} // end namespace itk
