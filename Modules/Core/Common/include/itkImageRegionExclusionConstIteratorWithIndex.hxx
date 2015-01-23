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
#ifndef itkImageRegionExclusionConstIteratorWithIndex_hxx
#define itkImageRegionExclusionConstIteratorWithIndex_hxx

#include "itkImageRegionExclusionConstIteratorWithIndex.h"

namespace itk
{
template< typename TImage >
ImageRegionExclusionConstIteratorWithIndex< TImage >
::ImageRegionExclusionConstIteratorWithIndex() : Superclass()
{}

template< typename TImage >
ImageRegionExclusionConstIteratorWithIndex< TImage >
::ImageRegionExclusionConstIteratorWithIndex(const ImageType *ptr,
                                             const RegionType & region) :
Superclass(ptr, region)
{}

template< typename TImage >
ImageRegionExclusionConstIteratorWithIndex< TImage >
::ImageRegionExclusionConstIteratorWithIndex(const Superclass & it)
{
  Superclass::operator=(it);
}
//----------------------------------------------------------------------
//  Set the region to exclude from the walk
//----------------------------------------------------------------------
template< typename TImage >
void
ImageRegionExclusionConstIteratorWithIndex< TImage >
::SetExclusionRegion(const RegionType & region)
{
  // Crop the exclusion region so that it lies entirely within the
  // iterator region.
  m_ExclusionRegion = region;
  m_ExclusionRegion.Crop( this->m_Region );

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
template< typename TImage >
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
// Move to beginning of region
//----------------------------------------------------------------------
template< typename TImage >
void
ImageRegionExclusionConstIteratorWithIndex< TImage >
::GoToBegin()
{
  // Check whether exclusion region covers the entire region
  if ( m_ExclusionRegion == this->m_Region )
    {
    this->m_Position = this->m_End;
    this->m_Remaining = false;
    return;
    }

  this->Superclass::GoToBegin();

  // If inside the exclusion region to begin with, jump past it.
  for ( unsigned int in = 0; in < TImage::ImageDimension; in++ )
    {
    if ( m_ExclusionRegion.IsInside( this->m_PositionIndex ) )
      {
      if ( m_ExclusionRegion.GetSize()[in] == this->m_Region.GetSize()[in] )
        {
        this->m_PositionIndex[in] = this->m_BeginIndex[in];
        }
      else
        {
        this->m_PositionIndex[in] = m_ExclusionEnd[in];
        this->m_Position += this->m_OffsetTable[in] * m_ExclusionRegion.GetSize()[in];
        }
      }
    }
}

//----------------------------------------------------------------------
// Move to the end of the region
//----------------------------------------------------------------------
template< typename TImage >
void
ImageRegionExclusionConstIteratorWithIndex< TImage >
::GoToReverseBegin()
{
  // Check whether exclusion region covers the entire region
  if ( m_ExclusionRegion == this->m_Region )
    {
    this->m_Position = this->m_End;
    this->m_Remaining = false;
    return;
    }

  this->Superclass::GoToReverseBegin();

  // If inside the exclusion region to begin with, jump past it.
  for ( unsigned int in = 0; in < TImage::ImageDimension; in++ )
    {
    if ( m_ExclusionRegion.IsInside( this->m_PositionIndex ) )
      {
      if ( m_ExclusionRegion.GetSize()[in] == this->m_Region.GetSize()[in] )
        {
        // The line will be skipped entirely.
        this->m_PositionIndex[in] = this->m_EndIndex[in] - 1;
        }
      else
        {
        this->m_PositionIndex[in] = m_ExclusionBegin[in] - 1;
        this->m_Position -= this->m_OffsetTable[in] * m_ExclusionRegion.GetSize()[in];
        }
      }
    }
}

//----------------------------------------------------------------------
//  Advance along the line, skipping the exclusion region
//----------------------------------------------------------------------
template< typename TImage >
ImageRegionExclusionConstIteratorWithIndex< TImage > &
ImageRegionExclusionConstIteratorWithIndex< TImage >
::operator++()
{
  this->Superclass::operator++();

  while ( m_ExclusionRegion.IsInside( this->m_PositionIndex ) && this->m_Remaining )
    {
    // Entered the exclusion region, so jump across it
    this->m_Position += this->m_OffsetTable[0] *
      ( static_cast< OffsetValueType >( m_ExclusionRegion.GetSize()[0] ) );
    this->m_PositionIndex[0] = m_ExclusionEnd[0];
    if ( this->m_PositionIndex[0] == this->m_EndIndex[0] )
      {
      this->m_Position -= this->m_OffsetTable[0];
      this->Superclass::operator++();
      }
    }

  return *this;
}

//----------------------------------------------------------------------
//  Advance along the line in reverse direction, skipping exclusion region
//----------------------------------------------------------------------
template< typename TImage >
ImageRegionExclusionConstIteratorWithIndex< TImage > &
ImageRegionExclusionConstIteratorWithIndex< TImage >
::operator--()
{
  this->Superclass::operator--();

  while ( m_ExclusionRegion.IsInside( this->m_PositionIndex ) && this->m_Remaining )
    {
    // Entered the exclusion region, so jump across it
    this->m_Position -= this->m_OffsetTable[0] *
      ( static_cast< OffsetValueType >( m_ExclusionRegion.GetSize()[0] ) );
    this->m_PositionIndex[0] = m_ExclusionBegin[0] - 1;
    if ( this->m_PositionIndex[0] < this->m_BeginIndex[0] )
      {
      this->m_Position += this->m_OffsetTable[0];
      this->Superclass::operator--();
      }
    }

  return *this;
}


} // end namespace itk

#endif
