/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAlgorithm.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkNeighborhoodAlgorithm_h
#define __itkNeighborhoodAlgorithm_h

#include "itkNeighborhoodOperator.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkImage.h"
#include "itkExceptionObject.h"
#include "itkVectorComponentDataAccessor.h"

namespace itk
{
  
namespace NeighborhoodAlgorithm
{
  
  /**
 * Walks across the region defined in the iterator argument and
 * calculates the inner product of each pixel neighborhood
 * with the operator.  Writes scalar output to the corresponding
 * pixel in the output image.
 *
 * Assumes that the output image region, buffer, and image sizes and starting
 * indicies match those of the input image.
 *
 * This function handles boundary conditions.
 */
template < class TPixel, unsigned long VDimension>
void
DoUnSynchedInnerProduct(Image<TPixel, VDimension> *in,
                      Image<TPixel, VDimension> *out,
                      Neighborhood<TPixel, VDimension> &op);

/**
 * Walks across the region defined in the iterator argument and
 * calculates the inner product of each pixel neighborhood
 * with the operator.  Writes scalar output to the corresponding
 * pixel in the output image.
 *
 * The output image buffer size and
 * starting index must match the output image region size and
 * starting index ( output region must be contiguous in memory ).
 * Output region size and starting index must match input region
 * size and starting index.
 *
 * This function does not handle boundary conditions.
 */
template < class TNeighborhoodIterator, class TImageIterator,
    class TNeighborhood>
void
DoUnsynchedInnerProduct(TNeighborhoodIterator &it, TImageIterator op,
                        TNeighborhood &oper)
{
  const TNeighborhoodIterator itEnd = it.End();
  for (it = it.Begin(); it < itEnd; ++it, ++op)
    {
      //*op = it.InnerProduct(oper);
      ScalarTraits<typename TNeighborhoodIterator::ScalarValueType>
        ::SetScalar(*op, it.InnerProduct(oper));
    }
}
  
/**
 * Walks across the region defined in the iterator argument and
 * calculates the inner product of each pixel neighborhood
 * with the operator.  Writes scalar output to the corresponding
 * pixel in the output image.
 *
 * Assumes that the output image region, buffer, and image sizes and starting
 * indicies match those of the input image.
 *
 * This function does not handle boundary conditions.
 */
template < class TNeighborhoodIterator, class TNeighborhood>
void
DoSynchedInnerProduct(TNeighborhoodIterator &it, TNeighborhood &oper)
{
  const TNeighborhoodIterator itEnd = it.End();
  for (it = it.Begin(); it < itEnd; ++it)
    {
      // *( it.GetOutputBuffer() ) = it.InnerProduct(oper);
      ScalarTraits<typename TNeighborhoodIterator::ScalarValueType>
        ::SetScalar(*( it.GetOutputBuffer() ), it.InnerProduct(oper) );
    }
}

/**
 * Walks across the region defined in the iterator argument and
 * calculates the inner product of each pixel neighborhood
 * with the operator.  Writes scalar output to the corresponding
 * pixel in the output image.
 *
 * Assumes that the output image region, buffer, and image sizes and starting
 * indicies match those of the input image.
 *
 * This function handles boundary conditions.
 */
template < class TPixel, unsigned long VDimension>
void
DoSynchedInnerProduct(Image<TPixel, VDimension> *in,
                      Image<TPixel, VDimension> *out,
                      Neighborhood<TPixel, VDimension> &op);
  

/**
 *
 */
template<class TPixel, unsigned int VDimension>
typename Neighborhood<TPixel, VDimension>::ScalarValueType
InnerProduct(Neighborhood<TPixel, VDimension> &,
             std::valarray<typename Neighborhood<TPixel,
             VDimension>::ScalarValueType> &v,
             VectorComponentDataAccessor<TPixel, typename Neighborhood<TPixel,
             VDimension>::ScalarValueType> &accessor);


/**
 *
 */
template <class TNeighborhoodIterator, class TInternalType, class TExternalType>
TExternalType
AverageGradientMagnitudeSquared(typename TNeighborhoodIterator::ImageType *,
                                typename
                                TNeighborhoodIterator::ImageType::Region,
           VectorComponentDataAccessor<TInternalType, TExternalType> accessor);


} // end namespace NeighborhoodAlgorithm
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodAlgorithm.txx"
#endif

#endif
