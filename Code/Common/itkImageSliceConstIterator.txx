/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSliceConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkImageSliceConstIterator_txx
#define _itkImageSliceConstIterator_txx


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
//  Advance to Previous Line
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIterator<TImage>
::PreviousLine(void)
{
  // Move to previous line
  m_PositionIndex[ m_Direction_B ]--;
  m_Position -= m_Jump_B;

  // Move to end of line
  m_PositionIndex[ m_Direction_A ] = m_EndIndex[ m_Direction_A ]-1;   
  m_Position += m_OffsetTable[ m_Direction_A + 1 ]; 
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
//  Go Back to previous slice
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIterator<TImage>
::PreviousSlice(void)
{

  // Move to end of Slice
  m_PositionIndex[m_Direction_B] = m_EndIndex[m_Direction_B] - 1;   
  m_Position += m_OffsetTable[ m_Direction_B + 1 ]; 
  

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
//  Test for begin of line
//----------------------------------------------------------------------
template<class TImage>
bool 
ImageSliceConstIterator<TImage>
::IsAtBeginOfLine(void) 
{
  return m_PositionIndex[m_Direction_A] < m_BeginIndex[m_Direction_A];
}




//----------------------------------------------------------------------
//  Test for begin of slice
//----------------------------------------------------------------------
template<class TImage>
bool
ImageSliceConstIterator<TImage>
::IsAtBeginOfSlice(void) 
{
  return m_PositionIndex[m_Direction_B] < m_BeginIndex[m_Direction_B];
}






//----------------------------------------------------------------------
//  Select the fastest changing direction
//----------------------------------------------------------------------
template<class TImage>
void 
ImageSliceConstIterator<TImage>
::SetFirstDirection(unsigned int direction) 
{
  if( direction >= TImage::ImageDimension )
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
  if( direction >= TImage::ImageDimension )
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



//----------------------------------------------------------------------
//  Go back along a line
//----------------------------------------------------------------------
template<class TImage>
ImageSliceConstIterator<TImage> &
ImageSliceConstIterator<TImage>
::operator--()
{
  m_PositionIndex[ m_Direction_A ]--;
  m_Position -= m_Jump_A;
  return *this;
}






} // end namespace itk


#endif
