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
namespace itk
{
  
namespace NeighborhoodAlgorithm
{

template < class TPixel, unsigned long VDimension>
void
DoInnerProductOverRegion(Image<TPixel, VDimension> *ip,
                         Image<TPixel, VDimension> *op,
                         Neighborhood<TPixel, VDimension> &oper)

{
  typedef Image<TPixel, VDimension> Image;
  typedef Index<VDimension> Index;
  typedef RegionNeighborhoodIterator<TPixel, VDimension> NonBoundaryIterator;
  typedef RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
    BoundaryIterator;

  const unsigned long *radius = oper.GetRadius();
  const unsigned long *regionsize = ip->GetRegionSize();
  const Index regionstartindex = ip->GetRegionStartIndex();

  int i; 
  unsigned long bound[VDimension];
  Index start;

  // Apply operator to non-boundary pixels using fast Neighborhood iterator
  for ( i = 0; i<VDimension; ++i)
    {
      start[i] = regionstartindex[i] + radius[i];
      bound[i] = regionsize[i] - radius[i]*2;
    }
  NonBoundaryIterator nbi(radius, ip, start, bound);
  const NonBoundaryIterator nbiEnd = nbi.End();
  nbi.SetOutputBuffer(op->GetBufferPointer() + op->ComputeOffset(start));
  for (nbi = nbi.Begin(); nbi < nbiEnd; ++nbi)
    {
      //*( nbi.GetOutputBuffer() ) += 1;
           *( nbi.GetOutputBuffer() ) = nbi.InnerProduct(oper);
    }

  // Apply operator to boundary pixels using bounds checking Boundary iterator
  for ( i = 0; i<VDimension; ++i)
    {
      start[i] = regionstartindex[i];
      bound[i] = regionsize[i];
    }
  BoundaryIterator bi(radius, ip, start, bound);
  const BoundaryIterator biEnd = bi.End();
  bi.SetOutputBuffer(op->GetBufferPointer() + op->ComputeOffset(start));
  for (bi = bi.Begin(); bi < biEnd; ++bi)
    {
      //*( bi.GetOutputBuffer() ) +=1;
      *( bi.GetOutputBuffer() ) = bi.InnerProduct(oper);
    }
 
}

template < class TPixel, unsigned long VDimension>
void
DoInnerProductOverRegion(Image<TPixel, VDimension> *ip,
                         Image<TPixel, VDimension> *op,
                         NeighborhoodOperatorList<TPixel, VDimension> &oper)

{
  typedef Image<TPixel, VDimension> Image;
  typedef Index<VDimension> Index;
  typedef RegionNeighborhoodIterator<TPixel, VDimension> NonBoundaryIterator;
  typedef RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
    BoundaryIterator;

  const unsigned long *radius = oper.GetRadius();
  const unsigned long *regionsize = ip->GetRegionSize();
  const Index regionstartindex = ip->GetRegionStartIndex();

  int i; 
  unsigned long bound[VDimension];
  Index start;

  // Apply operator to non-boundary pixels using fast Neighborhood iterator
  for ( i = 0; i<VDimension; ++i)
    {
      start[i] = regionstartindex[i] + radius[i];
      bound[i] = regionsize[i] - radius[i]*2;
    }
  NonBoundaryIterator nbi(radius, ip, start, bound);
  const NonBoundaryIterator nbiEnd = nbi.End();
  nbi.SetOutputBuffer(op->GetBufferPointer() + op->ComputeOffset(start));
  for (nbi = nbi.Begin(); nbi < nbiEnd; ++nbi)
    {
      *( nbi.GetOutputBuffer() ) = nbi.InnerProduct(oper);
    }

  // Apply operator to boundary pixels using bounds checking Boundary iterator
  for ( i = 0; i<VDimension; ++i)
    {
      start[i] = regionstartindex[i];
      bound[i] = regionsize[i];
    }
  BoundaryIterator bi(radius, ip, start, bound);
  const BoundaryIterator biEnd = bi.End();
  bi.SetOutputBuffer(op->GetBufferPointer() + op->ComputeOffset(start));
  for (bi = bi.Begin(); bi < biEnd; ++bi)
    {
      *( bi.GetOutputBuffer() ) = bi.InnerProduct(oper);
    }
 
}

} // end namespace NeighborhoodAlgorithm
} // end namespace itk
