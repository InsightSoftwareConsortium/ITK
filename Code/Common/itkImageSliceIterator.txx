/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSliceIterator.txx
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
template<class TPixel, unsigned int VImageDimension>
void 
ImageSliceIterator<TPixel, VImageDimension>
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
template<class TPixel, unsigned int VImageDimension>
void 
ImageSliceIterator<TPixel, VImageDimension>
::NextSlice(void)
{

  // Move to beginning of Slice
  m_PositionIndex[m_Direction_B] = m_BeginIndex[m_Direction_B];   
  m_Position -= m_OffsetTable[ m_Direction_B + 1 ]; 
  

  for( unsigned int n=0; n<VImageDimension; n++ )
  {

    m_Remaining = false;
    
    if( n == m_Direction_B || n == m_Direction_A ) 
    {
      continue;
    }
    
    m_PositionIndex[ n  ]++;
    if( m_PositionIndex[n] < m_Size[n] )
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
ImageSliceIterator<TPixel, VImageDimension>
::IsAtEndOfLine(void) 
{
  return m_PositionIndex[m_Direction_A] >= m_Size[m_Direction_A];
}




//----------------------------------------------------------------------
//  Test for end of slice
//----------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
bool
ImageSliceIterator<TPixel, VImageDimension>
::IsAtEndOfSlice(void) 
{
  return m_PositionIndex[m_Direction_B] >= m_Size[m_Direction_B];
}





//----------------------------------------------------------------------
//  Select the fastest changing direction
//----------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
ImageSliceIterator<TPixel, VImageDimension>
::SetFirstDirection(unsigned int direction) 
{
  if( direction >= VImageDimension )
  {
    throw itk::ExceptionObject();
  }
  m_Direction_A = direction;
  m_Jump_A = m_OffsetTable[ m_Direction_A ];
}




//----------------------------------------------------------------------
//  Select the second fastest changing direction
//----------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
ImageSliceIterator<TPixel, VImageDimension>
::SetSecondDirection(unsigned int direction) 
{
  if( direction >= VImageDimension )
  {
    throw itk::ExceptionObject();
  }
  m_Direction_B = direction;
  m_Jump_B = m_OffsetTable[ m_Direction_B ];
}



//----------------------------------------------------------------------
//  Advance along a line
//----------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
ImageSliceIterator<TPixel, VImageDimension> &
ImageSliceIterator<TPixel, VImageDimension>
::operator++()
{
  m_PositionIndex[ m_Direction_A ]++;
  m_Position += m_Jump_A;
  return *this;
}




} // end namespace itk
