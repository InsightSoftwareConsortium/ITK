/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionNonBoundaryNeighborhoodIterator.txx
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
#ifndef _itkRegionNonBoundaryNeighborhoodIterator_txx
#define _itkRegionNonBoundaryNeighborhoodIterator_txx
namespace itk {

template< class TImage>
RegionNonBoundaryNeighborhoodIterator<TImage>
::RegionNonBoundaryNeighborhoodIterator(const SizeType &radius, ImageType *ptr,
                                        const RegionType &region)
{
  RegionType cropped;
  SizeType szc;
  Index<Dimension> idxc;
  for (unsigned int i = 0; i< Dimension; ++i)
    {
      szc[i] = ptr->GetRequestedRegion().GetSize()[i] - radius[i]*2;
      idxc[i]= ptr->GetRequestedRegion().GetIndex()[i]+ radius[i];
    }
  cropped.SetSize(szc);
  cropped.SetIndex(idxc);
  
  this->Initialize(radius, ptr, cropped);
}

template<class TImage>
RegionNonBoundaryNeighborhoodIterator<TImage>
RegionNonBoundaryNeighborhoodIterator<TImage>
::Begin() const
{
  //Copy the current iterator
  RegionNonBoundaryNeighborhoodIterator it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TImage>
RegionNonBoundaryNeighborhoodIterator<TImage>
RegionNonBoundaryNeighborhoodIterator<TImage>
::End() const
{
  IndexType endIndex;
  
  // Copy the current iterator
  RegionNonBoundaryNeighborhoodIterator it( *this );

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
