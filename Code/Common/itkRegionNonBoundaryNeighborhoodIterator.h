/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionNonBoundaryNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegionNonBoundaryNeighborhoodIterator_h
#define __itkRegionNonBoundaryNeighborhoodIterator_h

#include "itkRegionNeighborhoodIterator.h"

namespace itk {

/**
 * \class RegionNonBoundaryNeighborhoodIterator
 * \brief Subclass of NeighborhoodIterator designed for fast iteration
 * over the non-boundary pixels of an itk::Image region of interest.
 *
 * RegionNonBoundaryNeighborhoodIterator is the simple iterator
 * optimized for fast iteration of a pixel neighborhood over an
 * image's non-boundary pixels.  It performs no bounds checking and does
 * not handle boundary conditions on dereferencing.  The non-boundary pixels
 * are determined at construction from the region information contained
 * in the image.
 * 
 * \sa NeighborhoodIterator
 * \sa RegionBoundaryNeighborhoodIterator
 * \sa SmartRegionNeighborhoodIterator
 * \sa Neighborhood
 * \sa NeighborhoodAlgorithm
 */
 
template<class TPixel, unsigned int VDimension =2>
class RegionNonBoundaryNeighborhoodIterator
 : public RegionNeighborhoodIterator<TPixel, VDimension>
{
public:
  /** 
   * Standard "Self" typedef support.
   */
  typedef RegionNonBoundaryNeighborhoodIterator Self;
  
  /**
   * Standard Superclass typedef
   */
  typedef RegionNeighborhoodIterator<TPixel, VDimension> Superclass;

  /**
   * Image typedef support.
   */
  typedef Image<TPixel, VDimension> ImageType;

  /**
   * Region typedef support.
   */
  typedef ImageRegion<VDimension> RegionType;
  
  /**
   * Size object typedef support
   */
  typedef typename NeighborhoodBase<TPixel,VDimension>::SizeType SizeType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegionNonBoundaryNeighborhoodIterator, RegionNeighborhoodIterator);
  
  /**
   * Default constructor
   */
  RegionNonBoundaryNeighborhoodIterator() {};

  /**
  * Constructor establishes a neighborhood of iterators of a specified
  * dimension to walk a particular image and a particular region of
  * that image.
  */
  RegionNonBoundaryNeighborhoodIterator(const SizeType &, ImageType *,
                                        const RegionType &);

  /**
   * Return an iterator for the beginning of the region.
   */
  Self Begin() const;

  /**
   * Return an iterator for the end of the region.
   */
  Self End() const; 

  /**
   * 
   */
  void SetEnd()
  {
    m_EndPointer = this->End().operator[](this->size()>>1);
  }
  
  /**
   *
   */
  void SetToBegin()
  {
    *this = this->Begin();
  }

  /**
   * Assignment operator
   */
  Self &operator=(const Self& orig)
  {
    Superclass::operator=(orig);
    return *this;
  }
  
  
};
  
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionNonBoundaryNeighborhoodIterator.txx"
#endif

#endif 

