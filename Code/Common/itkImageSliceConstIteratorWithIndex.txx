/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSliceConstIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageSliceConstIteratorWithIndex_txx
#define _itkImageSliceConstIteratorWithIndex_txx
#include "itkImageSliceConstIteratorWithIndex.h"


namespace itk
{

//----------------------------------------------------------------------
//  Advance to Next Line
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIteratorWithIndex<TImage>
::NextLine(void)
{
  // Move to next line
  m_PositionIndex[ m_Direction_B ]++;
  m_Position += m_LineJump;

  // Move to beginning of line
  m_PositionIndex[ m_Direction_A ] = m_BeginIndex[ m_Direction_A ];   
  m_Position -= m_PixelJump *
       ( m_EndIndex[ m_Direction_A ] - m_BeginIndex[ m_Direction_A ] ); 
}




//----------------------------------------------------------------------
//  Advance to Previous Line
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIteratorWithIndex<TImage>
::PreviousLine(void)
{
  // Move to previous line
  m_PositionIndex[ m_Direction_B ]--;
  m_Position -= m_LineJump;

  // Move to end of line
  m_PositionIndex[ m_Direction_A ] = m_EndIndex[ m_Direction_A ]-1;   
  m_Position += m_PixelJump *
       ( m_EndIndex[ m_Direction_A ] - m_BeginIndex[ m_Direction_A ] ); 
}






//----------------------------------------------------------------------
//  Advance to next slice
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIteratorWithIndex<TImage>
::NextSlice(void)
{

  // Move to beginning of Slice
  m_PositionIndex[m_Direction_B] = m_BeginIndex[m_Direction_B];   
  m_Position -= m_LineJump *
       ( m_EndIndex[ m_Direction_B ] - m_BeginIndex[ m_Direction_B ] ); 
  

  for( unsigned int n=0; n<TImage::ImageDimension; n++ )
  {

    m_Remaining = false;
    
    if( n == m_Direction_B || n == m_Direction_A ) 
    {
      continue;
    }
    
    m_PositionIndex[ n  ]++;
    if( m_PositionIndex[n] < m_EndIndex[n] )
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
//  Go Back to previous slice
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIteratorWithIndex<TImage>
::PreviousSlice(void)
{

  // Move to end of Slice
  m_PositionIndex[m_Direction_B] = m_EndIndex[m_Direction_B] - 1;   
  m_Position += m_LineJump *
       ( m_EndIndex[ m_Direction_B ] - m_BeginIndex[ m_Direction_B ] ); 
  

  for( unsigned int n=0; n<TImage::ImageDimension; n++ )
  {

    m_Remaining = false;
    
    if( n == m_Direction_B || n == m_Direction_A ) 
    {
      continue;
    }
    
    m_PositionIndex[ n  ]--;
    if( m_PositionIndex[n] >= m_BeginIndex[n] )
    {
      m_Position -= m_OffsetTable[ n ];
      m_Remaining = true;
      break;
    }
    else 
    {
      m_Position += m_OffsetTable[ n+1 ] - m_OffsetTable[ n ];
      m_PositionIndex[ n ] = m_EndIndex[ n ] - 1; 
    }
  }
}





//----------------------------------------------------------------------
//  Test for end of line
//----------------------------------------------------------------------
template<class TImage>
bool 
ImageSliceConstIteratorWithIndex<TImage>
::IsAtEndOfLine(void) 
{
  return m_PositionIndex[m_Direction_A] >= m_EndIndex[m_Direction_A];
}




//----------------------------------------------------------------------
//  Test for end of slice
//----------------------------------------------------------------------
template<class TImage>
bool
ImageSliceConstIteratorWithIndex<TImage>
::IsAtEndOfSlice(void) 
{
  return m_PositionIndex[m_Direction_B] >= m_EndIndex[m_Direction_B];
}




//----------------------------------------------------------------------
//  Test for begin of line
//----------------------------------------------------------------------
template<class TImage>
bool 
ImageSliceConstIteratorWithIndex<TImage>
::IsAtReverseEndOfLine(void) 
{
  return m_PositionIndex[m_Direction_A] < m_BeginIndex[m_Direction_A];
}




//----------------------------------------------------------------------
//  Test for begin of slice
//----------------------------------------------------------------------
template<class TImage>
bool
ImageSliceConstIteratorWithIndex<TImage>
::IsAtReverseEndOfSlice(void) 
{
  return m_PositionIndex[m_Direction_B] < m_BeginIndex[m_Direction_B];
}






//----------------------------------------------------------------------
//  Select the fastest changing direction
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIteratorWithIndex<TImage>
::SetFirstDirection(unsigned int direction) 
{
  if( direction >= TImage::ImageDimension )
    {
    itkGenericExceptionMacro(<<"In image of dimension " << TImage::ImageDimension <<" Direction " << direction << " sas selected");
    }
  m_Direction_A = direction;
  m_PixelJump = m_OffsetTable[ m_Direction_A ];
}




//----------------------------------------------------------------------
//  Select the second fastest changing direction
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIteratorWithIndex<TImage>
::SetSecondDirection(unsigned int direction) 
{
  if( direction >= TImage::ImageDimension )
    {
    itkGenericExceptionMacro(<<"In image of dimension " << TImage::ImageDimension <<" Direction " << direction << " sas selected");
    }
  m_Direction_B = direction;
  m_LineJump = m_OffsetTable[ m_Direction_B ];
}



//----------------------------------------------------------------------
//  Advance along a line
//----------------------------------------------------------------------
template<class TImage>
ImageSliceConstIteratorWithIndex<TImage> &
ImageSliceConstIteratorWithIndex<TImage>
::operator++()
{
  m_PositionIndex[ m_Direction_A ]++;
  m_Position += m_PixelJump;
  return *this;
}



//----------------------------------------------------------------------
//  Go back along a line
//----------------------------------------------------------------------
template<class TImage>
ImageSliceConstIteratorWithIndex<TImage> &
ImageSliceConstIteratorWithIndex<TImage>
::operator--()
{
  m_PositionIndex[ m_Direction_A ]--;
  m_Position -= m_PixelJump;
  return *this;
}






} // end namespace itk


#endif
