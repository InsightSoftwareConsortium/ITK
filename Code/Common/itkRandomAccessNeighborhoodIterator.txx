/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomAccessNeighborhoodIterator.txx
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
#ifndef _itkRandomAccessNeighborhoodIterator_txx
#define _itkRandomAccessNeighborhoodIterator_txx
namespace itk {
  
template<class TImage, class TAccessor, class TDerefAccessor>
RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor>
RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor>
::Begin() const
{
  //Copy the current iterator
  Self it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TImage, class TAccessor, class TDerefAccessor>
RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor>
RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor>
::End() const 
{
  IndexType endIndex;
  
  // Copy the current iterator
  Self it( *this );

  // Calculate the end index
  for (unsigned int i = 0; i< Dimension; ++i)
    {
      endIndex.m_Index[i] = m_Bound[i] -1;
    }
  it.SetLocation( endIndex );

  ++it;

  return it;
}


template<class TImage, class TAccessor, class TDerefAccessor>
RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor> &
RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor>
::operator+=(const OffsetType & idx)
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->end();
  unsigned long accumulator = 0;
  const unsigned long* stride = this->GetImagePointer()->GetOffsetTable();

  // Offset from the increment in the lowest dimension
  accumulator += idx[0];
  
  // Offsets from the stride lengths in each dimension.
  //
  // Because the image offset table is based on its buffer size and not its
  // requested region size, we don't have to worry about adding in the wrapping
  // offsets. 
  for (i = 1; i< Dimension; ++i)
    {
      accumulator += idx[i] * stride[i];
    }

  // Increment pointers.
  for (it = this->begin(); it < _end; ++it)
    {
      (*it) += accumulator;
    }
  if (m_OutputBuffer)
    {
      m_OutputBuffer += accumulator;
    }

  // Update loop counter values
  m_Loop += idx;

  return *this;
}

template<class TImage, class TAccessor, class TDerefAccessor>
RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor> &
RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor> 
::operator-=(const OffsetType & idx)
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->end();
  unsigned long accumulator = 0;
  const unsigned long* stride = this->GetImagePointer()->GetOffsetTable();

  // Offset from the increment in the lowest dimension
  accumulator += idx[0];
  
  // Offsets from the stride lengths in each dimension.
  //
  // Because the image offset table is based on its buffer size and not its
  // requested region size, we don't have to worry about adding in the wrapping
  // offsets. 
  for (i = 1; i< Dimension; ++i)
    {
      accumulator += idx[i] * stride[i];
    }

  // Increment pointers.
  for (it = this->begin(); it < _end; ++it)
    {
      (*it) -= accumulator;
    }
  if (m_OutputBuffer)
    {
      m_OutputBuffer -= accumulator;
    }

  // Update loop counter values
  m_Loop -= idx;

  return *this;
}


} // namespace itk

#endif
