/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAlgorithm.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImageRegionSimpleIterator.h"

namespace itk
{
  
namespace NeighborhoodAlgorithm
{

template < class TPixel, unsigned long VDimension>
void
DoUnsynchedInnerProduct(Image<TPixel, VDimension> *ip,
                      Image<TPixel, VDimension> *op,
                      Neighborhood<TPixel, VDimension> &oper)
{
  typedef Image<TPixel, VDimension> Image;
  typedef Index<VDimension> Index;
  const unsigned long *radius = oper.GetRadius();
  const unsigned long *regionsize = ip->GetRegionSize();
  const Index regionstartindex = ip->GetRegionStartIndex();
  int i; 
  unsigned long bound[VDimension];
  Index start;

  // Apply operator to non-boundary pixels using a fast Neighborhood iterator
  // and an image iterator
  for ( i = 0; i < Image::ImageDimension; ++i)
    {
      start[i] = regionstartindex[i] + radius[i];
      bound[i] = regionsize[i] - radius[i]*2;
    }

  RegionNeighborhoodIterator<TPixel, VDimension> nbi(radius, ip, start, bound);
  ImageRegionSimpleIterator<TPixel, VDimension> rsi(op, start, bound);
  rsi.Begin();
 
  DoUnsynchedInnerProduct< RegionNeighborhoodIterator<TPixel, VDimension>,
    ImageRegionSimpleIterator<TPixel, VDimension>, 
    Neighborhood<TPixel, VDimension> >( nbi, rsi, oper );

  // Apply operator to boundary pixels using bounds checking boundary iterators
  for ( i = 0; i < Image::ImageDimension; ++i)
    {
      start[i] = regionstartindex[i];
      bound[i] = regionsize[i];
    }

  RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
    bi( radius, ip, start, bound);
  RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
    obi( radius, ip, start, bound);

  const RegionBoundaryNeighborhoodIterator<TPixel, VDimension> biEnd= bi.End();
  for (bi = bi.Begin(), obi = obi.Begin();
       bi < biEnd; ++bi, ++obi)
    {
      *(obi.CenterPointer()) = bi.InnerProduct(oper);
    }

}

template < class TPixel, unsigned long VDimension>
void
DoSynchedInnerProduct(Image<TPixel, VDimension> *ip,
                      Image<TPixel, VDimension> *op,
                      Neighborhood<TPixel, VDimension> &oper)

{
  typedef Image<TPixel, VDimension> Image;
  typedef Index<VDimension> Index;
  const unsigned long *radius = oper.GetRadius();
  const unsigned long *regionsize = ip->GetRegionSize();
  const Index regionstartindex = ip->GetRegionStartIndex();
  int i; 
  unsigned long bound[VDimension];
  Index start;

  // Apply operator to non-boundary pixels using fast Neighborhood iterator
  for ( i = 0; i < Image::ImageDimension; ++i)
    {
      start[i] = regionstartindex[i] + radius[i];
      bound[i] = regionsize[i] - radius[i]*2;
    }

  RegionNeighborhoodIterator<TPixel, VDimension> nbi(radius, ip, start, bound);
  nbi.SetOutputBuffer( op->GetBufferPointer() + op->ComputeOffset(start) );
  
  DoSynchedInnerProduct< RegionNeighborhoodIterator<TPixel, VDimension>,
    Neighborhood<TPixel, VDimension> >( nbi, oper );
  
  // Apply operator to boundary pixels using bounds checking Boundary iterator
  for ( i = 0; i < Image::ImageDimension; ++i)
    {
      start[i] = regionstartindex[i];
      bound[i] = regionsize[i];
    }

  RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
    bi( radius, ip, start, bound);
  bi.SetOutputBuffer( op->GetBufferPointer() );
  
  DoSynchedInnerProduct<RegionBoundaryNeighborhoodIterator<TPixel, VDimension>,
    Neighborhood<TPixel, VDimension> >( bi, oper );
}

} // end namespace NeighborhoodAlgorithm
} // end namespace itk
