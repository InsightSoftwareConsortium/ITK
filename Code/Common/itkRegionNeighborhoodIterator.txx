/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
namespace itk {
  
template<class TPixel, unsigned int VDimension, class TAllocator,
    class TDerefAllocator> 
typename 
RegionNeighborhoodIterator<TPixel, VDimension, TAllocator, 
  TDerefAllocator>::NeighborhoodType
RegionNeighborhoodIterator<TPixel, VDimension, TAllocator, TDerefAllocator>
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

template<class TPixel, unsigned int VDimension, class TAllocator,
    class TDerefAllocator>
void
RegionNeighborhoodIterator<TPixel, VDimension, TAllocator, TDerefAllocator>
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
    
template<class TPixel, unsigned int VDimension, class TAllocator,
  class TDerefAllocator>
void RegionNeighborhoodIterator<TPixel, VDimension, TAllocator,
  TDerefAllocator>
::SetBound(const SizeType& size)
{
  const unsigned long *offset     = m_Image->GetOffsetTable();
  SizeType bufferSize = m_Image->GetBufferedRegion().GetSize();

  // Set the bounds and the wrapping offsets
  for (unsigned int i=0; i<VDimension; ++i)
    {
      m_Bound[i]      = m_StartIndex[i]+size[i];
      m_WrapOffset[i] = (bufferSize[i] - (m_Bound[i] - m_StartIndex[i]))
                        * offset[i];
    }  
}

template<class TPixel, unsigned int VDimension, class TAllocator,
  class TDerefAllocator>
RegionNeighborhoodIterator<TPixel, VDimension, TAllocator, TDerefAllocator> 
RegionNeighborhoodIterator<TPixel, VDimension, TAllocator, TDerefAllocator> 
::Begin() const
{
  //Copy the current iterator
  RegionNeighborhoodIterator it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TPixel, unsigned int VDimension, class TAllocator,
  class TDerefAllocator>
RegionNeighborhoodIterator<TPixel, VDimension, TAllocator, TDerefAllocator>
RegionNeighborhoodIterator<TPixel, VDimension, TAllocator, TDerefAllocator>
::End() const
{
  IndexType endIndex;
  
  // Copy the current iterator
  RegionNeighborhoodIterator it( *this );

  // Calculate the end index
  for (unsigned int i = 0; i< VDimension; ++i)
    {
      endIndex.m_Index[i] = m_Bound[i] -1;
    }
  it.SetLocation( endIndex );

  ++it;
  return it;
}

} // namespace itk
