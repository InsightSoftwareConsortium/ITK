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
#ifndef itkImageRegionConstIteratorWithOnlyIndex_hxx
#define itkImageRegionConstIteratorWithOnlyIndex_hxx

#include "itkImageRegionConstIteratorWithOnlyIndex.h"

namespace itk
{
//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template< typename TImage >
ImageRegionConstIteratorWithOnlyIndex< TImage > &
ImageRegionConstIteratorWithOnlyIndex< TImage >
::operator++()
{
  this->m_Remaining = false;
  for ( unsigned int in = 0; in < TImage::ImageDimension; in++ )
    {
    this->m_PositionIndex[in]++;
    if ( this->m_PositionIndex[in] < this->m_EndIndex[in] )
      {
      this->m_Remaining = true;
      break;
      }
    else
      {
      this->m_PositionIndex[in] = this->m_BeginIndex[in];
      }
    }

  return *this;
}

//----------------------------------------------------------------------
//  Advance along the line in reverse direction
//----------------------------------------------------------------------
template< typename TImage >
ImageRegionConstIteratorWithOnlyIndex< TImage > &
ImageRegionConstIteratorWithOnlyIndex< TImage >
::operator--()
{
  this->m_Remaining = false;
  for ( unsigned int in = 0; in < TImage::ImageDimension; in++ )
    {
    if ( this->m_PositionIndex[in] > this->m_BeginIndex[in] )
      {
      this->m_PositionIndex[in]--;
      this->m_Remaining = true;
      break;
      }
    else
      {
      this->m_PositionIndex[in] = this->m_EndIndex[in] - 1;
      }
    }

  return *this;
}
} // end namespace itk

#endif
