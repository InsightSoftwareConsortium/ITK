/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionNonBoundaryNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
namespace itk {

template< class TPixel, unsigned int VDimension, class TAllocator,
    class TDerefAllocator>
RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension, TAllocator,
  TDerefAllocator>
::RegionNonBoundaryNeighborhoodIterator(const SizeType &radius, ImageType *ptr,
                                        const RegionType &region)
{
  RegionType cropped;
  SizeType szc;
  Index<VDimension> idxc;
  for (unsigned int i = 0; i< VDimension; ++i)
    {
      szc[i] = ptr->GetRequestedRegion().GetSize()[i] - radius[i]*2;
      idxc[i]= ptr->GetRequestedRegion().GetIndex()[i]+ radius[i];
    }
  cropped.SetSize(szc);
  cropped.SetIndex(idxc);
  
  this->Initialize(radius, ptr, cropped);
}

template<class TPixel, unsigned int VDimension, class TAllocator,
    class TDerefAllocator>
RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension, TAllocator,
  TDerefAllocator>
RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension, TAllocator,
  TDerefAllocator>
::Begin() const
{
  //Copy the current iterator
  RegionNonBoundaryNeighborhoodIterator it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TPixel, unsigned int VDimension, class TAllocator,
  class TDerefAllocator>
RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension, TAllocator,
  TDerefAllocator>
RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension, TAllocator,
  TDerefAllocator>
::End() const
{
  IndexType endIndex;
  
  // Copy the current iterator
  RegionNonBoundaryNeighborhoodIterator it( *this );

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
