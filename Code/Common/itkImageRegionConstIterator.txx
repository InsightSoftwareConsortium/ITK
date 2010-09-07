/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageRegionConstIterator_txx
#define __itkImageRegionConstIterator_txx

#include "itkImageRegionConstIterator.h"

#ifdef __INTEL_COMPILER
/* pointless comparision of unsigned integer with zero */
#pragma warning(disable: 186)
#endif

namespace itk
{
//----------------------------------------------------------------------------
// Begin() is the first pixel in the region.
template< class TImage >
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
template< class TImage >
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

//----------------------------------------------------------------------------
// Increment when the fastest moving direction has reached its bound.
// This method should *ONLY* be invoked from the operator++() method.
template< class TImage >
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
  ind = this->m_Image->ComputeIndex( static_cast< typename Superclass::OffsetValueType >( this->m_Offset ) );

  const typename ImageIterator< TImage >::IndexType &
  startIndex = this->m_Region.GetIndex();
  const typename ImageIterator< TImage >::SizeType &
  size = this->m_Region.GetSize();

  // Increment along a row, then wrap at the end of the region row.

  // Check to see if we are past the last pixel in the region
  // Note that ++ind[0] moves to the next pixel along the row.
  ++ind[0];
  bool done = ( ind[0] == startIndex[0] + static_cast< typename Superclass::IndexValueType >( size[0] ) );
  for ( unsigned int i = 1; done && i < ImageIteratorDimension; i++ )
    {
    done = ( ind[i] == startIndex[i] + static_cast< typename Superclass::IndexValueType >( size[i] ) - 1 );
    }

  // if the iterator is outside the region (but not past region end) then
  // we need to wrap around the region
  unsigned int dim = 0;
  if ( !done )
    {
    while ( ( ( dim + 1 ) < ImageIteratorDimension )
            && ( ind[dim] > startIndex[dim] +  static_cast< typename Superclass::IndexValueType >( size[dim] ) - 1 ) )
      {
      ind[dim] = startIndex[dim];
      ind[++dim]++;
      }
    }
  this->m_Offset = this->m_Image->ComputeOffset(ind);
  m_SpanEndOffset = this->m_Offset + static_cast< long >( size[0] );
  m_SpanBeginOffset = this->m_Offset;
}

//----------------------------------------------------------------------------
// Decrement when the fastest moving direction has reached its bound.
// This method should *ONLY* be invoked from the operator--() method.
template< class TImage >
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
  ind = this->m_Image->ComputeIndex( static_cast< typename Superclass::IndexValueType >( this->m_Offset ) );

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
      ind[dim] = startIndex[dim] + static_cast< typename Superclass::IndexValueType >( size[dim] ) - 1;
      ind[++dim]--;
      }
    }
  this->m_Offset = this->m_Image->ComputeOffset(ind);
  m_SpanEndOffset = this->m_Offset + 1;
  m_SpanBeginOffset = m_SpanEndOffset - static_cast< long >( size[0] );
}
} // end namespace itk

#endif
