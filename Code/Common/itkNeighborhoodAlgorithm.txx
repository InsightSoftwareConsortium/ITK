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
#include "itkImageRegion.h"

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
  typedef Size<VDimension> Size;
  typedef ImageRegion<VDimension> Region;

  const Size&   hoodRadius  = oper.GetRadius();
  const Region& imageRegion = ip->GetRequestedRegion();

  int i; 
  Size  size;
  Index start;

  // Apply operator to non-boundary pixels using a fast Neighborhood iterator
  // and an image iterator
  for ( i = 0; i < Image::ImageDimension; ++i)
    {
      start[i]  = imageRegion.GetIndex()[i] + hoodRadius[i];
      size[i]   = imageRegion.GetSize()[i]  - hoodRadius[i] * 2;
    }
  Region iterRegion;
  iterRegion.SetSize(size);
  iterRegion.SetIndex(start);

  RegionNeighborhoodIterator<TPixel, VDimension>
    nbi(hoodRadius, ip, iterRegion);
  ImageRegionSimpleIterator<TPixel, VDimension> rsi(op, iterRegion);
  rsi.Begin();
 
  DoUnsynchedInnerProduct< RegionNeighborhoodIterator<TPixel, VDimension>,
    ImageRegionSimpleIterator<TPixel, VDimension>, 
    Neighborhood<TPixel, VDimension> >( nbi, rsi, oper );

  // Apply operator to boundary pixels using bounds checking boundary iterators
  RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
    bi(hoodRadius, ip, imageRegion);
  RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
    obi(hoodRadius, ip, imageRegion);

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
  typedef ImageRegion<VDimension> Region;
  typedef Size<VDimension> Size;

  const Size&   hoodRadius  = oper.GetRadius();
  const Region& imageRegion = ip->GetRequestedRegion();

  int i; 
  unsigned long bound[VDimension];
  Index start;
  Size  size;

  // Apply operator to non-boundary pixels using fast Neighborhood iterator
  for ( i = 0; i < Image::ImageDimension; ++i)
    {
      start[i] = imageRegion.GetIndex()[i] + hoodRadius[i];
      size[i]  = imageRegion.GetSize()[i]  - hoodRadius[i] * 2;
    }
  Region iterRegion;
  iterRegion.SetSize(size);
  iterRegion.SetIndex(start);

  RegionNeighborhoodIterator<TPixel, VDimension>
    nbi(hoodRadius, ip, iterRegion);
  nbi.SetOutputBuffer( op->GetBufferPointer() + op->ComputeOffset(start) );
  
  DoSynchedInnerProduct< RegionNeighborhoodIterator<TPixel, VDimension>,
    Neighborhood<TPixel, VDimension> >( nbi, oper );
  
  // Apply operator to boundary pixels using bounds checking Boundary iterator
  RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
    bi( hoodRadius, ip, imageRegion);
  bi.SetOutputBuffer( op->GetBufferPointer() );
  
  DoSynchedInnerProduct<RegionBoundaryNeighborhoodIterator<TPixel, VDimension>,
    Neighborhood<TPixel, VDimension> >( bi, oper );
}

} // end namespace NeighborhoodAlgorithm
} // end namespace itk
