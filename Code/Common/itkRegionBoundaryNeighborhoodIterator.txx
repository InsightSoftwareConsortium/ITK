/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionBoundaryNeighborhoodIterator.txx
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
#ifndef _itkRegionBoundaryNeighborhoodIterator_txx
#define _itkRegionBoundaryNeighborhoodIterator_txx
namespace itk {

template<class TImage, class TAllocator, class TBoundaryCondition,
    class TDerefAllocator>
void
RegionBoundaryNeighborhoodIterator<TImage, TAllocator, TBoundaryCondition,
  TDerefAllocator>
::SetBound(const SizeType& size)
{
  Superclass::SetBound(size);
  m_InnerStride = (m_Bound[0] - m_StartIndex[0]) - 2*this->GetRadius(0);
}

template<class TImage, class TAllocator, class TBoundaryCondition,
    class TDerefAllocator>
const NeighborhoodIterator<TImage, TAllocator, TDerefAllocator> &
RegionBoundaryNeighborhoodIterator<TImage, TAllocator, TBoundaryCondition,
  TDerefAllocator>
::operator++()
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->end();

  if (! this->InBounds())
    {
      NeighborhoodIterator<TImage, TAllocator, TDerefAllocator>::operator++();
    }

  while(this->InBounds())
    {
      // Increment pointers.
      for (it = this->begin(); it < _end; ++it)
        {
          (*it)+= m_InnerStride;;
        }
      if (m_OutputBuffer)
        {
          m_OutputBuffer += m_InnerStride;
        }
      
      // Check loop bounds, wrap & add pointer offsets if needed.
      for (i=0; i < Dimension; ++i)
        {
          if (i==0)
            {
              m_Loop[0] += m_InnerStride;
            }
          else
            {
              m_Loop[i]++;
            }
          
          if ( m_Loop[i] == m_Bound[i] )
            {
              m_Loop[i]= m_StartIndex[i];
              for (it = this->begin(); it < _end; ++it)
                {
                  (*it) += m_WrapOffset[i];
                }
              if (m_OutputBuffer)
                {
                  m_OutputBuffer += m_WrapOffset[i]
                                  + m_OutputWrapOffsetModifier[i];
                }
            }        
          else break;
        }
    }
  return *this; 
}

template<class TImage, class TAllocator, class TBoundaryCondition,
  class TDerefAllocator>
const NeighborhoodIterator<TImage, TAllocator, TDerefAllocator> &
RegionBoundaryNeighborhoodIterator<TImage, TAllocator, TBoundaryCondition,
  TDerefAllocator>
::operator--()
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->end();

  if (! this->InBounds())
    {
      NeighborhoodIterator<TImage, TAllocator,
        TDerefAllocator>::operator--();
    }

  while(this->InBounds())
    {
      // Decrement pointers.
      for (it = this->begin(); it < _end; ++it)
        {
          (*it)-= m_InnerStride;;
        }
      if (m_OutputBuffer)
        {
          m_OutputBuffer -= m_InnerStride;
        }
      
      // Check loop bounds, wrap & add pointer offsets if needed.
      for (i=0; i < Dimension; ++i)
        {
          if (i==0)
            {
              m_Loop[0] -= m_InnerStride;
            }
          
          if ( m_Loop[i] == m_StartIndex[i] )
            {
              m_Loop[i]= m_Bound[i] - 1;
              for (it = this->begin(); it < _end; ++it)
                {
                  (*it) -= m_WrapOffset[i];
                }
              if (m_OutputBuffer)
                {
                  m_OutputBuffer -= m_WrapOffset[i]
                    + m_OutputWrapOffsetModifier[i];
                }
            }        
          else
            {
              m_Loop[i]--;
              break;
            }
        }
    }
  return *this; 
}

template<class TImage, class TAllocator, class TBoundaryCondition,
  class TDerefAllocator>
RegionBoundaryNeighborhoodIterator<TImage, TAllocator, TBoundaryCondition,
  TDerefAllocator>
RegionBoundaryNeighborhoodIterator<TImage, TAllocator, TBoundaryCondition,
  TDerefAllocator>
::Begin() const
{
  //Copy the current iterator
  Self it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TImage, class TAllocator, class TBoundaryCondition,
  class TDerefAllocator>
RegionBoundaryNeighborhoodIterator<TImage, TAllocator, TBoundaryCondition,
  TDerefAllocator>
RegionBoundaryNeighborhoodIterator<TImage, TAllocator, TBoundaryCondition,
  TDerefAllocator>
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

} // namespace itk

#endif
