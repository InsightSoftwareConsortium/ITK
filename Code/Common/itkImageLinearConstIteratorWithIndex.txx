/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageLinearConstIteratorWithIndex.txx
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
#ifndef _itkImageLinearConstIteratorWithIndex_txx
#define _itkImageLinearConstIteratorWithIndex_txx

#include "itkSimpleImageRegionIterator.h"

namespace itk
{




//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
void 
ImageLinearConstIteratorWithIndex<TImage>
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
    if( m_PositionIndex[n] <  m_EndIndex[n] )
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
//  Pass to the last pixel on the previous line
//----------------------------------------------------------------------
template<class TImage>
void 
ImageLinearConstIteratorWithIndex<TImage>
::PreviousLine(void)
{

  m_PositionIndex[m_Direction] = m_EndIndex[m_Direction]-1;   
  m_Position += m_OffsetTable[ m_Direction+1 ]; 
  

  for( unsigned int n=0; n<TImage::ImageDimension; n++ )
  {

    m_Remaining = false;
    
    if( n == m_Direction ) 
    {
      continue;
    }
    
    m_PositionIndex[ n  ]--;
    if( m_PositionIndex[ n ] >=  m_BeginIndex[ n ] )
    {
      m_Position -= m_OffsetTable[ n ];
      m_Remaining = true;
      break;
    }
    else 
    {
      m_Position += m_OffsetTable[ n ] * ( m_Region.GetSize()[n]-1 );
      m_PositionIndex[ n ] = m_EndIndex[ n ] - 1; 
    }
  }
}





//----------------------------------------------------------------------
//  Test for end of line
//----------------------------------------------------------------------
template<class TImage>
bool 
ImageLinearConstIteratorWithIndex<TImage>
::IsAtEndOfLine(void) 
{
  return m_PositionIndex[m_Direction] >= m_EndIndex[m_Direction];
}




//----------------------------------------------------------------------
//  Test for begin of line
//----------------------------------------------------------------------
template<class TImage>
bool 
ImageLinearConstIteratorWithIndex<TImage>
::IsAtBeginOfLine(void) 
{
  return m_PositionIndex[m_Direction] < m_BeginIndex[m_Direction];
}



//----------------------------------------------------------------------
//  Set direction of movement
//----------------------------------------------------------------------
template<class TImage>
void 
ImageLinearConstIteratorWithIndex<TImage>
::SetDirection(unsigned int direction) 
{
  if( direction >= TImage::ImageDimension )
  {
    throw ExceptionObject();
  }
  m_Direction = direction;
  m_Jump = m_OffsetTable[ m_Direction ];
}
 




//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template<class TImage>
ImageLinearConstIteratorWithIndex<TImage>  & 
ImageLinearConstIteratorWithIndex<TImage>
::operator++()
{
  m_PositionIndex[m_Direction]++;
  m_Position += m_Jump;
  return *this;
}




//----------------------------------------------------------------------
//  Advance along the line backwards
//----------------------------------------------------------------------
template<class TImage>
ImageLinearConstIteratorWithIndex<TImage>  & 
ImageLinearConstIteratorWithIndex<TImage>
::operator--()
{
  m_PositionIndex[m_Direction]--;
  m_Position -= m_Jump;
  return *this;
}




} // end namespace itk

#endif
