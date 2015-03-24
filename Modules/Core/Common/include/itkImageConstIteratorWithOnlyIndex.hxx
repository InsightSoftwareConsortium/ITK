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
#ifndef itkImageConstIteratorWithOnlyIndex_hxx
#define itkImageConstIteratorWithOnlyIndex_hxx

#include "itkImageConstIteratorWithOnlyIndex.h"

namespace itk
{
//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageConstIteratorWithOnlyIndex< TImage >
::ImageConstIteratorWithOnlyIndex()
{
  m_Remaining = false;
}

//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageConstIteratorWithOnlyIndex< TImage >
::ImageConstIteratorWithOnlyIndex(const Self & it)
{
  m_Image = it.m_Image;     // copy the smart pointer

  m_PositionIndex     = it.m_PositionIndex;
  m_BeginIndex        = it.m_BeginIndex;
  m_EndIndex          = it.m_EndIndex;
  m_Region            = it.m_Region;

  std::copy(it.m_OffsetTable,
            it.m_OffsetTable+ImageDimension + 1,
            m_OffsetTable);

  m_Remaining   = it.m_Remaining;
}

//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageConstIteratorWithOnlyIndex< TImage >
::ImageConstIteratorWithOnlyIndex(const TImage *ptr, const RegionType & region)
{
  m_Image = ptr;

  m_BeginIndex        = region.GetIndex();
  m_PositionIndex     = m_BeginIndex;
  m_Region            = region;

  std::copy(m_Image->GetOffsetTable(),
            m_Image->GetOffsetTable()+ImageDimension + 1,
            m_OffsetTable);

  // Compute the end offset
  m_Remaining = false;
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    SizeValueType size = region.GetSize()[i];
    if ( size > 0 )
      {
      m_Remaining = true;
      }
    m_EndIndex[i] = m_BeginIndex[i] + static_cast< OffsetValueType >( size );
    }

  GoToBegin();
}

//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template< typename TImage >
ImageConstIteratorWithOnlyIndex< TImage > &
ImageConstIteratorWithOnlyIndex< TImage >
::operator=(const Self & it)
{
  if(this != *it)
    {
    m_Image = it.m_Image;     // copy the smart pointer

    m_BeginIndex        = it.m_BeginIndex;
    m_EndIndex          = it.m_EndIndex;
    m_PositionIndex     = it.m_PositionIndex;
    m_Region            = it.m_Region;

    std::copy(it.m_OffsetTable,
              it.m_OffsetTable+ImageDimension + 1,
              m_OffsetTable);

    m_Remaining   = it.m_Remaining;
    }
  return *this;
}

//----------------------------------------------------------------------------
// GoToBegin() is the first pixel in the region.
//----------------------------------------------------------------------------
template< typename TImage >
void
ImageConstIteratorWithOnlyIndex< TImage >
::GoToBegin()
{
  // Set the position at begin

  m_PositionIndex  = m_BeginIndex;

  if ( m_Region.GetNumberOfPixels() > 0 )
    {
    m_Remaining = true;
    }
  else
    {
    m_Remaining = false;
    }
}

//----------------------------------------------------------------------------
// GoToReverseBegin() is the last pixel in the region.
//----------------------------------------------------------------------------
template< typename TImage >
void
ImageConstIteratorWithOnlyIndex< TImage >
::GoToReverseBegin()
{
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    m_PositionIndex[i]  = m_EndIndex[i] - 1;
    }

  if ( m_Region.GetNumberOfPixels() > 0 )
    {
    m_Remaining = true;
    }
  else
    {
    m_Remaining = false;
    }
}

#if !defined(ITK_LEGACY_REMOVE)
//----------------------------------------------------------------------------
// Begin() is the first pixel in the region.
//----------------------------------------------------------------------------
template< typename TImage >
ImageConstIteratorWithOnlyIndex< TImage >
ImageConstIteratorWithOnlyIndex< TImage >
::Begin() const
{
  Self it(*this);

  it.GoToBegin();
  return it;
}

//----------------------------------------------------------------------------
// End() is the last pixel in the region.  DEPRECATED
//----------------------------------------------------------------------------
template< typename TImage >
ImageConstIteratorWithOnlyIndex< TImage >
ImageConstIteratorWithOnlyIndex< TImage >
::End() const
{
  Self it(*this);

  it.GoToReverseBegin();
  return it;
}
#endif

} // end namespace itk

#endif
