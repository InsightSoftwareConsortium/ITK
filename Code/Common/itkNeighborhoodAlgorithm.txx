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
#include "itkImageRegionIterator.h"
#include "itkImageRegion.h"
#include "itkDerivativeOperator.h"

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
  typedef Image<TPixel, VDimension> ImageType;
  typedef Index<VDimension> IndexType;
  typedef Size<VDimension> SizeType;
  typedef ImageRegion<VDimension> RegionType;

  const SizeType&   hoodRadius  = oper.GetRadius();
  const RegionType& imageRegion = ip->GetRequestedRegion();

  int i; 
  SizeType  size;
  IndexType start;

  // Apply operator to non-boundary pixels using a fast Neighborhood iterator
  // and an image iterator
  for ( i = 0; i < ImageType::ImageDimension; ++i)
    {
    start[i]  = imageRegion.GetIndex()[i] + hoodRadius[i];
    size[i]   = imageRegion.GetSize()[i]  - hoodRadius[i] * 2;
    }
  RegionType iterRegion;
  iterRegion.SetSize(size);
  iterRegion.SetIndex(start);

  RegionNeighborhoodIterator<TPixel, VDimension>
    nbi(hoodRadius, ip, iterRegion);
  ImageRegionIterator<TPixel, VDimension> rsi(op, iterRegion);
  rsi = rsi.Begin();
 
  DoUnsynchedInnerProduct< RegionNeighborhoodIterator<TPixel, VDimension>,
    ImageRegionIterator<TPixel, VDimension>, 
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
      // *(obi.CenterPointer()) = bi.InnerProduct(oper);
      ScalarTraits<TPixel>::SetScalar(*(obi.CenterPointer()),
                                      bi.InnerProduct(oper) );
    }

}

template < class TPixel, unsigned long VDimension>
void
DoSynchedInnerProduct(Image<TPixel, VDimension> *ip,
                      Image<TPixel, VDimension> *op,
                      Neighborhood<TPixel, VDimension> &oper)

{
  typedef Image<TPixel, VDimension> ImageType;
  typedef Index<VDimension> IndexType;
  typedef ImageRegion<VDimension> RegionType;
  typedef Size<VDimension> SizeType;

  const SizeType&   hoodRadius  = oper.GetRadius();
  const RegionType& imageRegion = ip->GetRequestedRegion();

  int i; 
  unsigned long bound[VDimension];
  IndexType start;
  SizeType  size;

  // Apply operator to non-boundary pixels using fast Neighborhood iterator
  for ( i = 0; i < ImageType::ImageDimension; ++i)
    {
      start[i] = imageRegion.GetIndex()[i] + hoodRadius[i];
      size[i]  = imageRegion.GetSize()[i]  - hoodRadius[i] * 2;
    }
  RegionType iterRegion;
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


template<class TPixel, unsigned int VDimension>
typename Neighborhood<TPixel, VDimension>::TPixelScalarValueType
InnerProduct(Neighborhood<TPixel, VDimension> &n,
             std::valarray<typename Neighborhood<TPixel,
             VDimension>::TPixelScalarValueType> &v, 
             VectorComponentDataAccessor<TPixel, typename Neighborhood<TPixel,
             VDimension>::TPixelScalarValueType> &accessor)
{
  typedef typename Neighborhood<TPixel, VDimension>::TPixelScalarValueType
    ExternalType;
  
  ExternalType sum  = NumericTraits<ExternalType>::Zero; 
  
  ExternalType *it;
  TPixel *this_it;
  
  const ExternalType *itEnd = &(v[v.size()]);
  for (it = &(v[0]), this_it = n.Begin(); it < itEnd;
       ++it, ++this_it)
    {
      sum += *it * accessor.Get(*this_it);
    }
  
  return sum;
}

template <class TNeighborhoodIterator, class TInternalType, class TExternalType>
TExternalType
AverageGradientMagnitudeSquared(typename TNeighborhoodIterator::ImageType *ip,
                        typename TNeighborhoodIterator::ImageType::RegionType region,
        VectorComponentDataAccessor<TInternalType, TExternalType> accessor)
{
  typedef  typename TNeighborhoodIterator::ImageType ImageType;

  TExternalType accumulator;
  TExternalType val;
  unsigned long counter;
  TNeighborhoodIterator iterator_list[ImageType::ImageDimension];
   DerivativeOperator<TExternalType, ImageType::ImageDimension>
    operator_list[ImageType::ImageDimension];

  // Set up the derivative operators and their iterators
  for (int i = 0; i < ImageType::ImageDimension; ++i)
    {
      operator_list[i].SetOrder(1);
      operator_list[i].SetDirection(i);
      operator_list[i].CreateDirectional();
      iterator_list[i] =
        RegionNeighborhoodIterator<TInternalType, ImageType::ImageDimension>
                                   (operator_list[i].GetRadius(), ip, region);
      iterator_list[i] = iterator_list[i].Begin();
    }

  // Now do the actual processing
  accumulator = NumericTraits<TExternalType>::Zero;
  counter     = 0;
  const RegionNeighborhoodIterator<TInternalType, ImageType::ImageDimension>
                                         iterator_end = iterator_list[0].End();
  for (iterator_list[0] = iterator_list[0].Begin();
       iterator_list[0] < iterator_end; ++counter)
    {
      for (int i = 0; i < ImageType::ImageDimension; ++i)
        {
          val = iterator_list[i].InnerProduct(operator_list[i], accessor);
          accumulator += val * val;
          ++iterator_list[i];
        }
    }

    return (accumulator / counter);

}



} // end namespace NeighborhoodAlgorithm
} // end namespace itk
