/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSliceConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/


namespace itk
{

//----------------------------------------------------------------------
//  Advance to Next Line
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIterator<TImage>
::NextLine(void)
{
  // Move to next line
  m_PositionIndex[ m_Direction_B ]++;
  m_Position += m_Jump_B;

  // Move to beginning of line
  m_PositionIndex[ m_Direction_A ] = m_BeginIndex[ m_Direction_A ];   
  m_Position -= m_OffsetTable[ m_Direction_A + 1 ]; 
}




//----------------------------------------------------------------------
//  Advance to next slice
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIterator<TImage>
::NextSlice(void)
{

  // Move to beginning of Slice
  m_PositionIndex[m_Direction_B] = m_BeginIndex[m_Direction_B];   
  m_Position -= m_OffsetTable[ m_Direction_B + 1 ]; 
  

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
//  Test for end of line
//----------------------------------------------------------------------
template<class TImage>
bool 
ImageSliceConstIterator<TImage>
::IsAtEndOfLine(void) 
{
  return m_PositionIndex[m_Direction_A] >= m_EndIndex[m_Direction_A];
}




//----------------------------------------------------------------------
//  Test for end of slice
//----------------------------------------------------------------------
template<class TImage>
bool
ImageSliceConstIterator<TImage>
::IsAtEndOfSlice(void) 
{
  return m_PositionIndex[m_Direction_B] >= m_EndIndex[m_Direction_B];
}





//----------------------------------------------------------------------
//  Select the fastest changing direction
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIterator<TImage>
::SetFirstDirection(unsigned int direction) 
{
  if( direction >= VImageDimension )
  {
    throw ExceptionObject();
  }
  m_Direction_A = direction;
  m_Jump_A = m_OffsetTable[ m_Direction_A ];
}




//----------------------------------------------------------------------
//  Select the second fastest changing direction
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIterator<TImage>
::SetSecondDirection(unsigned int direction) 
{
  if( direction >= VImageDimension )
  {
    throw ExceptionObject();
  }
  m_Direction_B = direction;
  m_Jump_B = m_OffsetTable[ m_Direction_B ];
}



//----------------------------------------------------------------------
//  Advance along a line
//----------------------------------------------------------------------
template<class TImage>
ImageSliceConstIterator<TImage> &
ImageSliceConstIterator<TImage>
::operator++()
{
  m_PositionIndex[ m_Direction_A ]++;
  m_Position += m_Jump_A;
  return *this;
}




} // end namespace itk

