/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionNeighborhoodIterator.txx
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
#ifndef _itkRegionNeighborhoodIterator_txx
#define _itkRegionNeighborhoodIterator_txx
namespace itk {
  
template<class TImage> 
typename 
RegionNeighborhoodIterator<TImage>::NeighborhoodType
RegionNeighborhoodIterator<TImage>
::GetNeighborhood()
{
  NeighborhoodType ans;
  typename NeighborhoodType::Iterator ans_it;
  Iterator this_it;
  const Iterator _end = this->end();

  ans.SetRadius( this->GetRadius() );

  for (ans_it = ans.begin(), this_it = this->begin();
       this_it < _end; ans_it++, this_it++)
    {
      *ans_it = **this_it;
    }

  return ans;
}

template<class TImage>
void
RegionNeighborhoodIterator<TImage>
::SetNeighborhood(NeighborhoodType &N)
{
  Iterator this_it;
  const Iterator _end = this->end();
  typename NeighborhoodType::Iterator N_it;
  N_it = N.begin();
  
  for (this_it = this->begin(); this_it < _end; this_it++, N_it++)
    {
      **this_it = *N_it;
    }

}
    
template<class TImage>
void RegionNeighborhoodIterator<TImage>
::SetBound(const SizeType& size)
{
  const unsigned long *offset     = m_Image->GetOffsetTable();
  SizeType bufferSize = m_Image->GetBufferedRegion().GetSize();

  // Set the bounds and the wrapping offsets
  for (unsigned int i=0; i<Dimension; ++i)
    {
      m_Bound[i]      = m_StartIndex[i]+size[i];
      m_WrapOffset[i] = (bufferSize[i] - (m_Bound[i] - m_StartIndex[i]))
                        * offset[i];
    }  
}

template<class TImage>
RegionNeighborhoodIterator<TImage> 
RegionNeighborhoodIterator<TImage> 
::Begin() const
{
  //Copy the current iterator
  RegionNeighborhoodIterator it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TImage>
RegionNeighborhoodIterator<TImage>
RegionNeighborhoodIterator<TImage>
::End() const
{
  IndexType endIndex;
  
  // Copy the current iterator
  RegionNeighborhoodIterator it( *this );

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
