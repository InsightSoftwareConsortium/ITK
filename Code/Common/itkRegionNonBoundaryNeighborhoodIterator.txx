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
#ifndef _itkRegionNonBoundaryNeighborhoodIterator_txx
#define _itkRegionNonBoundaryNeighborhoodIterator_txx
namespace itk {

template< class TImage, class TAllocator, class TDerefAllocator>
RegionNonBoundaryNeighborhoodIterator<TImage, TAllocator,
  TDerefAllocator>
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

template<class TImage, class TAllocator, class TDerefAllocator>
RegionNonBoundaryNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
RegionNonBoundaryNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::Begin() const
{
  //Copy the current iterator
  RegionNonBoundaryNeighborhoodIterator it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TImage, class TAllocator,  class TDerefAllocator>
RegionNonBoundaryNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
RegionNonBoundaryNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
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
