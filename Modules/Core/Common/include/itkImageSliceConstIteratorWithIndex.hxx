/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageSliceConstIteratorWithIndex_hxx
#define itkImageSliceConstIteratorWithIndex_hxx

#include "itkImageSliceConstIteratorWithIndex.h"

namespace itk
{
//----------------------------------------------------------------------
//  Advance to Next Line
//----------------------------------------------------------------------
template< typename TImage >
void
ImageSliceConstIteratorWithIndex< TImage >
::NextLine(void)
{
  // Move to next line
  this->m_PositionIndex[m_Direction_B]++;
  this->m_Position += m_LineJump;

  // Move to beginning of line
  this->m_PositionIndex[m_Direction_A] = this->m_BeginIndex[m_Direction_A];
  this->m_Position -= m_PixelJump
                      * ( this->m_EndIndex[m_Direction_A] - this->m_BeginIndex[m_Direction_A] );
}

//----------------------------------------------------------------------
//  Advance to Previous Line
//----------------------------------------------------------------------
template< typename TImage >
void
ImageSliceConstIteratorWithIndex< TImage >
::PreviousLine(void)
{
  // Move to previous line
  this->m_PositionIndex[m_Direction_B]--;
  this->m_Position -= m_LineJump;

  // Move to end of line
  this->m_PositionIndex[m_Direction_A] = this->m_EndIndex[m_Direction_A] - 1;
  this->m_Position += m_PixelJump
                      * ( this->m_EndIndex[m_Direction_A] - this->m_BeginIndex[m_Direction_A] );
}

//----------------------------------------------------------------------
//  Go to the first pixel of the current slice
//----------------------------------------------------------------------
template< typename TImage >
void
ImageSliceConstIteratorWithIndex< TImage >
::GoToBeginOfSlice(void)
{
  // Move to beginning of Slice
  this->m_PositionIndex[m_Direction_B] = this->m_BeginIndex[m_Direction_B];
  this->m_Position -= m_LineJump
                      * ( this->m_EndIndex[m_Direction_B] - this->m_BeginIndex[m_Direction_B] );
}

//----------------------------------------------------------------------
//  Advance to next slice
//----------------------------------------------------------------------
template< typename TImage >
void
ImageSliceConstIteratorWithIndex< TImage >
::NextSlice(void)
{
  // Move to beginning of Slice
  this->m_Position -= m_LineJump
                      * ( this->m_PositionIndex[m_Direction_B] - this->m_BeginIndex[m_Direction_B] );
  this->m_PositionIndex[m_Direction_B] = this->m_BeginIndex[m_Direction_B];

  for ( unsigned int n = 0; n < TImage::ImageDimension; n++ )
    {
    this->m_Remaining = false;

    if ( n == m_Direction_B || n == m_Direction_A )
      {
      continue;
      }

    this->m_PositionIndex[n]++;
    if ( this->m_PositionIndex[n] < this->m_EndIndex[n] )
      {
      this->m_Position += this->m_OffsetTable[n];
      this->m_Remaining = true;
      break;
      }
    else
      {
      this->m_Position -= this->m_OffsetTable[n + 1] - this->m_OffsetTable[n];
      this->m_PositionIndex[n] = this->m_BeginIndex[n];
      }
    }
}

//----------------------------------------------------------------------
//  Go Back to previous slice
//----------------------------------------------------------------------
template< typename TImage >
void
ImageSliceConstIteratorWithIndex< TImage >
::PreviousSlice(void)
{
  // Move to end of Slice
  this->m_PositionIndex[m_Direction_B] = this->m_EndIndex[m_Direction_B] - 1;
  this->m_Position += m_LineJump
                      * ( this->m_EndIndex[m_Direction_B] - this->m_BeginIndex[m_Direction_B] );

  for ( unsigned int n = 0; n < TImage::ImageDimension; n++ )
    {
    this->m_Remaining = false;

    if ( n == m_Direction_B || n == m_Direction_A )
      {
      continue;
      }

    this->m_PositionIndex[n]--;
    if ( this->m_PositionIndex[n] >= this->m_BeginIndex[n] )
      {
      this->m_Position -= this->m_OffsetTable[n];
      this->m_Remaining = true;
      break;
      }
    else
      {
      this->m_Position += this->m_OffsetTable[n + 1] - this->m_OffsetTable[n];
      this->m_PositionIndex[n] = this->m_EndIndex[n] - 1;
      }
    }
}

//----------------------------------------------------------------------
//  Test for end of line
//----------------------------------------------------------------------
template< typename TImage >
bool
ImageSliceConstIteratorWithIndex< TImage >
::IsAtEndOfLine(void)
{
  return this->m_PositionIndex[m_Direction_A] >= this->m_EndIndex[m_Direction_A];
}

//----------------------------------------------------------------------
//  Test for end of slice
//----------------------------------------------------------------------
template< typename TImage >
bool
ImageSliceConstIteratorWithIndex< TImage >
::IsAtEndOfSlice(void)
{
  return this->m_PositionIndex[m_Direction_B] >= this->m_EndIndex[m_Direction_B];
}

//----------------------------------------------------------------------
//  Test for begin of line
//----------------------------------------------------------------------
template< typename TImage >
bool
ImageSliceConstIteratorWithIndex< TImage >
::IsAtReverseEndOfLine(void)
{
  return this->m_PositionIndex[m_Direction_A] < this->m_BeginIndex[m_Direction_A];
}

//----------------------------------------------------------------------
//  Test for begin of slice
//----------------------------------------------------------------------
template< typename TImage >
bool
ImageSliceConstIteratorWithIndex< TImage >
::IsAtReverseEndOfSlice(void)
{
  return this->m_PositionIndex[m_Direction_B] < this->m_BeginIndex[m_Direction_B];
}

//----------------------------------------------------------------------
//  Select the fastest changing direction
//----------------------------------------------------------------------
template< typename TImage >
void
ImageSliceConstIteratorWithIndex< TImage >
::SetFirstDirection(unsigned int direction)
{
  if ( direction >= TImage::ImageDimension )
    {
    itkGenericExceptionMacro(
      << "In image of dimension " << TImage::ImageDimension << " Direction " << direction << " sas selected");
    }
  m_Direction_A = direction;
  m_PixelJump = this->m_OffsetTable[m_Direction_A];
}

//----------------------------------------------------------------------
//  Select the second fastest changing direction
//----------------------------------------------------------------------
template< typename TImage >
void
ImageSliceConstIteratorWithIndex< TImage >
::SetSecondDirection(unsigned int direction)
{
  if ( direction >= TImage::ImageDimension )
    {
    itkGenericExceptionMacro(
      << "In image of dimension " << TImage::ImageDimension << " Direction " << direction << " sas selected");
    }
  m_Direction_B = direction;
  m_LineJump = this->m_OffsetTable[m_Direction_B];
}

//----------------------------------------------------------------------
//  Advance along a line
//----------------------------------------------------------------------
template< typename TImage >
ImageSliceConstIteratorWithIndex< TImage > &
ImageSliceConstIteratorWithIndex< TImage >
::operator++()
{
  this->m_PositionIndex[m_Direction_A]++;
  this->m_Position += m_PixelJump;
  return *this;
}

//----------------------------------------------------------------------
//  Go back along a line
//----------------------------------------------------------------------
template< typename TImage >
ImageSliceConstIteratorWithIndex< TImage > &
ImageSliceConstIteratorWithIndex< TImage >
::operator--()
{
  this->m_PositionIndex[m_Direction_A]--;
  this->m_Position -= m_PixelJump;
  return *this;
}
} // end namespace itk

#endif
