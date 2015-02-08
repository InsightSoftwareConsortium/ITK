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
#ifndef itkImageLinearConstIteratorWithIndex_hxx
#define itkImageLinearConstIteratorWithIndex_hxx

#include "itkImageLinearConstIteratorWithIndex.h"

namespace itk
{
//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageLinearConstIteratorWithIndex< TImage >
::ImageLinearConstIteratorWithIndex(
  const ImageType *ptr,
  const RegionType & region):
  ImageConstIteratorWithIndex< TImage >(ptr, region)
{
  this->SetDirection(0);
}

//----------------------------------------------------------------------
//  Go to the last pixel of the current line
//----------------------------------------------------------------------
template< typename TImage >
void
ImageLinearConstIteratorWithIndex< TImage >
::GoToReverseBeginOfLine(void)
{
  OffsetValueType distanceToEnd =
    this->m_EndIndex[m_Direction] - this->m_PositionIndex[m_Direction] - 1;

  this->m_Position += m_Jump * distanceToEnd;
  this->m_PositionIndex[m_Direction] = this->m_EndIndex[m_Direction] - 1;
}

//----------------------------------------------------------------------
//  Go to the first pixel of the current line
//----------------------------------------------------------------------
template< typename TImage >
void
ImageLinearConstIteratorWithIndex< TImage >
::GoToBeginOfLine(void)
{
  OffsetValueType distanceToBegin =
    this->m_PositionIndex[m_Direction] - this->m_BeginIndex[m_Direction];

  this->m_Position -= m_Jump * distanceToBegin;

  this->m_PositionIndex[m_Direction] = this->m_BeginIndex[m_Direction];
}

//----------------------------------------------------------------------
//  Pass to the past last pixel of the current line
//----------------------------------------------------------------------
template< typename TImage >
void
ImageLinearConstIteratorWithIndex< TImage >
::GoToEndOfLine(void)
{
  OffsetValueType distanceToEnd =
    this->m_EndIndex[m_Direction] - this->m_PositionIndex[m_Direction];

  this->m_Position += m_Jump * distanceToEnd;

  this->m_PositionIndex[m_Direction] = this->m_EndIndex[m_Direction];
}
} // end namespace itk

#endif
