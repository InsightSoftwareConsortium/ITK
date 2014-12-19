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
#ifndef itkImageRegionConstIterator_hxx
#define itkImageRegionConstIterator_hxx

#include "itkImageRegionConstIterator.h"

namespace itk
{

//----------------------------------------------------------------------------
// Increment when the fastest moving direction has reached its bound.
// This method should *ONLY* be invoked from the operator++() method.
template< typename TImage >
void
ImageRegionConstIterator< TImage >
::Increment()
{
  // We have reached the end of the span (row), need to wrap around.

  // First back up one pixel, because we are going to use a different
  // algorithm to compute the next pixel
  --this->m_Offset;

  // Get the index of the last pixel on the span (row)
  typename ImageIterator< TImage >::IndexType
  ind = this->m_Image->ComputeIndex( static_cast< OffsetValueType >( this->m_Offset ) );

  const typename ImageIterator< TImage >::IndexType &
  startIndex = this->m_Region.GetIndex();
  const typename ImageIterator< TImage >::SizeType &
  size = this->m_Region.GetSize();

  // Increment along a row, then wrap at the end of the region row.

  // Check to see if we are past the last pixel in the region
  // Note that ++ind[0] moves to the next pixel along the row.
  ++ind[0];
  bool done = ( ind[0] == startIndex[0] + static_cast< IndexValueType >( size[0] ) );
  for ( unsigned int i = 1; done && i < ImageIteratorDimension; i++ )
    {
    done = ( ind[i] == startIndex[i] + static_cast< IndexValueType >( size[i] ) - 1 );
    }

  // if the iterator is outside the region (but not past region end) then
  // we need to wrap around the region
  unsigned int d = 0;
  if ( !done )
    {
    while ( ( ( d + 1 ) < ImageIteratorDimension )
            &&  static_cast< SizeValueType >( ind[d] - startIndex[d] ) >= size[d] )
      {
      ind[d] = startIndex[d];
      ind[++d]++;
      }
    }
  this->m_Offset = this->m_Image->ComputeOffset(ind);
  m_SpanEndOffset = this->m_Offset + static_cast< OffsetValueType >( size[0] );
  m_SpanBeginOffset = this->m_Offset;
}

//----------------------------------------------------------------------------
// Decrement when the fastest moving direction has reached its bound.
// This method should *ONLY* be invoked from the operator--() method.
template< typename TImage >
void
ImageRegionConstIterator< TImage >
::Decrement()
{
  // We have pasted the beginning of the span (row), need to wrap around.

  // First move forward one pixel, because we are going to use a different
  // algorithm to compute the next pixel
  this->m_Offset++;

  // Get the index of the first pixel on the span (row)
  typename ImageIterator< TImage >::IndexType
  ind = this->m_Image->ComputeIndex( static_cast< IndexValueType >( this->m_Offset ) );

  const typename ImageIterator< TImage >::IndexType &
  startIndex = this->m_Region.GetIndex();
  const typename ImageIterator< TImage >::SizeType &
  size = this->m_Region.GetSize();

  // Deccrement along a row, then wrap at the beginning of the region row.

  // Check to see if we are past the first pixel in the region
  // Note that --ind[0] moves to the previous pixel along the row.
  bool done = ( --ind[0] == startIndex[0] - 1 );
  for ( unsigned int i = 1; done && i < ImageIteratorDimension; i++ )
    {
    done = ( ind[i] == startIndex[i] );
    }

  // if the iterator is outside the region (but not past region begin) then
  // we need to wrap around the region
  unsigned int dim = 0;
  if ( !done )
    {
    while ( ( ( dim + 1 ) < ImageIteratorDimension )
            && ( ind[dim] < startIndex[dim] ) )
      {
      ind[dim] = startIndex[dim] + static_cast< IndexValueType >( size[dim] ) - 1;
      ind[++dim]--;
      }
    }
  this->m_Offset = this->m_Image->ComputeOffset(ind);
  m_SpanEndOffset = this->m_Offset + 1;
  m_SpanBeginOffset = m_SpanEndOffset - static_cast< OffsetValueType >( size[0] );
}

#if !defined(ITK_LEGACY_REMOVE)
//----------------------------------------------------------------------------
// Begin() is the first pixel in the region.
template< typename TImage >
ImageRegionConstIterator< TImage >
ImageRegionConstIterator< TImage >
::Begin() const
{
  // Copy the current iterator
  Self it(*this);

  // Set the iterator to the beginning of the region
  it.GoToBegin();

  return it;
}

//----------------------------------------------------------------------------
// End() is one pixel past the last pixel in the current region.
// The index of this pixel is
//          [m_StartIndex[0] + m_Size[0],
//           m_StartIndex[1] + m_Size[1]-1, ...,
//           m_StartIndex[VImageDimension-2] + m_Size[VImageDimension-2]-1,
//           m_StartIndex[VImageDimension-1] + m_Size[VImageDimension-1]-1]
//
template< typename TImage >
ImageRegionConstIterator< TImage >
ImageRegionConstIterator< TImage >
::End() const
{
  // Copy the current iterator
  Self it(*this);

  // Set the iterator to the end of the region
  it.GoToEnd();

  return it;
}
#endif
} // end namespace itk

#endif
