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
#ifndef __itkImageRegionExclusionConstIteratorWithIndex_hxx
#define __itkImageRegionExclusionConstIteratorWithIndex_hxx

#include "itkImageRegionExclusionConstIteratorWithIndex.h"

namespace itk
{
//----------------------------------------------------------------------
//  Set the region to exclude from the walk
//----------------------------------------------------------------------
template< class TImage >
void
ImageRegionExclusionConstIteratorWithIndex< TImage >
::SetExclusionRegion(const RegionType & region)
{
  if ( !this->m_Region.IsInside(region) )
    {
    itkGenericExceptionMacro(<< "Attempt to set a exclusion region that is NOT contained inside the iterator region");
    }
  m_ExclusionRegion      = region;
  m_ExclusionBegin       = m_ExclusionRegion.GetIndex();
  SizeType exclusionSize = m_ExclusionRegion.GetSize();

  for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
    {
    m_ExclusionEnd[i] = m_ExclusionBegin[i] + exclusionSize[i];
    }
}

//----------------------------------------------------------------------
//  Set the region to exclude from the walk to a region that is inset
//  one pixel from the boundary of the region
//----------------------------------------------------------------------
template< class TImage >
void
ImageRegionExclusionConstIteratorWithIndex< TImage >
::SetExclusionRegionToInsetRegion()
{
  RegionType excludeRegion;

  excludeRegion = this->m_Region;
  for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
    {
    if ( excludeRegion.GetSize()[i] >= 2 )
      {
      // region is large enough to inset, adjust size and index
      excludeRegion.SetSize(i, excludeRegion.GetSize()[i] - 2);
      excludeRegion.SetIndex(i, excludeRegion.GetIndex()[i] + 1);
      }
    else
      {
      // region is not large enough to inset, set exclusion size to
      // zero and keep the index the same.
      excludeRegion.SetSize(i, 0);
      }
    }
  this->SetExclusionRegion(excludeRegion);
}

//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template< class TImage >
ImageRegionExclusionConstIteratorWithIndex< TImage > &
ImageRegionExclusionConstIteratorWithIndex< TImage >
::operator++()
{
  this->m_Remaining = false;
  for ( unsigned int in = 0; in < TImage::ImageDimension; in++ )
    {
    this->m_PositionIndex[in]++;

    // if entering the exclusion region... jump over it
    if ( m_ExclusionRegion.IsInside(this->m_PositionIndex) )
      {
      this->m_PositionIndex[in]  = m_ExclusionEnd[in];
      this->m_Position += this->m_OffsetTable[in] * m_ExclusionRegion.GetSize()[in];
      }

    if ( this->m_PositionIndex[in] < this->m_EndIndex[in] )
      {
      this->m_Position += this->m_OffsetTable[in];
      this->m_Remaining = true;
      break;
      }
    else
      {
      this->m_Position -= this->m_OffsetTable[in] * ( static_cast< OffsetValueType >( this->m_Region.GetSize()[in] ) - 1 );
      this->m_PositionIndex[in] = this->m_BeginIndex[in];
      }
    }

  if ( !this->m_Remaining ) // It will not advance here otherwise
    {
    this->m_Position = this->m_End;
    }

  return *this;
}

//----------------------------------------------------------------------
//  Advance along the line in reverse direction
//----------------------------------------------------------------------
template< class TImage >
ImageRegionExclusionConstIteratorWithIndex< TImage > &
ImageRegionExclusionConstIteratorWithIndex< TImage >
::operator--()
{
  this->m_Remaining = false;
  for ( unsigned int in = 0; in < TImage::ImageDimension; in++ )
    {
    if ( this->m_PositionIndex[in] > this->m_BeginIndex[in] )
      {
      this->m_PositionIndex[in]--;
      this->m_Position -= this->m_OffsetTable[in];

      // if entering the exclusion region... jump over it
      if ( m_ExclusionRegion.IsInside(this->m_PositionIndex) )
        {
        this->m_PositionIndex[in]  = m_ExclusionBegin[in] - 1;
        this->m_Position -= this->m_OffsetTable[in] * m_ExclusionRegion.GetSize()[in];
        }

      this->m_Remaining = true;
      break;
      }
    else
      {
      this->m_PositionIndex[in]--;
      this->m_Position += this->m_OffsetTable[in] * ( static_cast< OffsetValueType >( this->m_Region.GetSize()[in] ) - 1 );
      this->m_PositionIndex[in] = this->m_EndIndex[in] - 1;
      }
    }

  if ( !this->m_Remaining ) // It will not advance here otherwise
    {
    this->m_Position = this->m_End;
    }

  return *this;
}
} // end namespace itk

#endif
