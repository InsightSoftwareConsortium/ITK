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
 
template<class TImage,
  class TAllocator
    = NeighborhoodAllocator<typename ImageTraits<TImage>::InternalPixelType *>,
  class TDerefAllocator
    = NeighborhoodAllocator<typename ImageTraits<TImage>::PixelType>
  >
class ITK_EXPORT RegionNonBoundaryNeighborhoodIterator
  : public RegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
{
public:

  /** 
   * Standard "Self" & Superclass typedef support.
   */
  typedef RegionNonBoundaryNeighborhoodIterator Self;
  typedef RegionNeighborhoodIterator<TImage, TAllocator,
    TDerefAllocator> Superclass;

  /**
   * Extract image type information.
   */
  typedef typename Superclass::InternalPixelType InternalPixelType;
  typedef typename Superclass::PixelType PixelType;
  enum {Dimension = Superclass::Dimension };
  
  /**
   * Some common itk object typedefs
   */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::SizeType SizeType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::IndexType IndexType;

  /**
   * Scalar data type typedef support
   */
  typedef typename Superclass::ScalarValueType ScalarValueType;

  /**
   * Default constructor
   */
  RegionNonBoundaryNeighborhoodIterator() {};

  /**
   * Copy constructor
   */
  RegionNonBoundaryNeighborhoodIterator(const Self& other)
    : RegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>(other)
  {}
  
  /**
   * Assignment operator
   */
  Self &operator=(const Self& orig)
  {
    Superclass::operator=(orig);
    return *this;
  }
  
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
  {    m_EndPointer = this->End().operator[](this->Size()>>1);  }
  
  /**
   *
   */
  void SetToBegin()
  {    *this = this->Begin();  }

};
  
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionNonBoundaryNeighborhoodIterator.txx"
#endif

#endif 

