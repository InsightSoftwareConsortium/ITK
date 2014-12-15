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
#ifndef itkImageScanlineConstIterator_hxx
#define itkImageScanlineConstIterator_hxx

#include "itkImageScanlineConstIterator.h"

namespace itk
{

template< typename TImage >
void
ImageScanlineConstIterator< TImage >
::Increment()
{
  // increment to the next scanline

  // Get the index of the last pixel on the span (row)
  IndexType ind = this->m_Image->ComputeIndex( static_cast< OffsetValueType >( m_SpanEndOffset -1 ) );

  const IndexType &startIndex = this->m_Region.GetIndex();
  const SizeType &size = this->m_Region.GetSize();

  // Check to see if we are past the last pixel in the region
  // Note that ++ind[0] moves to the next pixel along the row.
  ++ind[0];
  bool done = ( ind[0] == startIndex[0] + static_cast< IndexValueType >( size[0] ) );
  for ( unsigned int i = 1; done && i < ImageIteratorDimension; ++i )
    {
    done = ( ind[i] == startIndex[i] + static_cast< IndexValueType >( size[i] ) - 1 );
    }

 // if the iterator is outside the region (but not past region begin) then
  // we need to wrap around the region
  unsigned int dim = 0;
  if ( !done )
    {
    while ( ( ( dim + 1 ) < ImageIteratorDimension )
            && ( ind[dim] > startIndex[dim] +  static_cast< IndexValueType >( size[dim] ) - 1 ) )
      {
      ind[dim] = startIndex[dim];
      ind[++dim]++;
      }
    }

  this->m_Offset = this->m_Image->ComputeOffset(ind);
  m_SpanEndOffset = this->m_Offset + static_cast< OffsetValueType >( size[0] );
  m_SpanBeginOffset = this->m_Offset;
}

} // end namespace itk

#endif
