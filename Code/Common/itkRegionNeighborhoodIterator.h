/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegionNeighborhoodIterator_h
#define __itkRegionNeighborhoodIterator_h

#include "itkNeighborhoodIterator.h"
#include "itkVectorComponentDataAccessor.h"

namespace itk {

/**
 * \class RegionNeighborhoodIterator
 * \brief Subclass of NeighborhoodIterator designed for fast iteration
 * over an itk::Image region of interest that does no bounds checking.
 *
 * RegionNeighborhoodIterator is the simplest of the NeighborhoodIterators.
 * It is optimized for fast iteration of a pixel neighborhood over an
 * image.  It performs no bounds checking and does not handle boundary
 * conditions on dereferencing.
 *
 * RegionNeighborhoodIterator only maintains counters for loop position and
 * upper bounds, and so it is "unaware" when it is overlapping a region
 * boundary.
 * You can only safely use this iterator on regions sufficiently contained
 * within the itk::Image buffer.
 *
 * \sa NeighborhoodIterator
 * \sa RegionBoundaryNeighborhoodIterator
 * \sa SmartRegionNeighborhoodIterator
 * \sa Neighborhood
 * \sa NeighborhoodAlgorithm
 */
 
template<class TPixel, unsigned int VDimension =2>
class RegionNeighborhoodIterator
 : public NeighborhoodIterator<TPixel, VDimension>
{
public:
  /** 
   * Standard "Self" typedef support.
   */
  typedef RegionNeighborhoodIterator Self;
  
  /**
   * Standard Superclass typedef
   */
  typedef NeighborhoodIterator<TPixel, VDimension> Superclass;
  
  /**
   * itk::Image typedef support.
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
   * Scalar data type typedef support
   */
  typedef typename ScalarTraits<TPixel>::ScalarValueType ScalarValueType;

  /**
   * itk::Neighborhood typedef support
   */
  typedef Neighborhood<TPixel, VDimension> NeighborhoodType;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegionNeighborhoodIterator, NeighborhoodIterator);
  
  /**
   * Default constructor
   */
  RegionNeighborhoodIterator() {};

  /**
  * Constructor establishes a neighborhood of iterators of a specified
  * dimension to walk a particular image and a particular region of
  * that image.
  */
  RegionNeighborhoodIterator(const SizeType &radius,
                             ImageType *ptr,
                             const RegionType &region)
  {
    this->Initialize(radius, ptr, region);
  }

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
   * Returns a Neighborhood object with values of the image pixels that
   * are referenced by the NeighborhoodIterator's internal pointers.
   * \sa SetPixelValues
   * \sa Neighborhood
   */
  Neighborhood<TPixel, VDimension> GetNeighborhood();

  /**
   * Sets the values in the referenced image to the values contained in
   * a Neighborhood object.  This method assumes that the Neighborhood object
   * argument and the NeighborhoodPointer are equal size.
   */
  void SetNeighborhood(NeighborhoodType &);

  /**
   * Prints information about the neighborhood pointer structure to
   * std::cout for debugging purposes.
   */
  void PrintSelf();


  /**
   * Calculates the inner product of the neighborhood of referenced pixel
   * values with a valarray (and therefore also any NeighborhoodBase subclass).
   * Returns a scalar value.  Innerproduct is re-implemented for
   * the NeighborhoodPointer to avoid the penalty of creating a temporary
   * Neighborhood when such a step is not needed by an algorithm.
   *
   * \sa Neighborhood
   * \sa SlicedInnerProduct
   */
  ScalarValueType InnerProduct(std::valarray<TPixel> &);
  ScalarValueType InnerProduct(std::valarray<ScalarValueType> &,
                                     VectorComponentDataAccessor<TPixel,
                                     ScalarValueType> &);
  ScalarValueType SlicedInnerProduct(const std::slice &s,
                                           std::valarray<TPixel> &v);

  ScalarValueType SlicedInnerProduct(const std::slice &,
                                           std::valarray<ScalarValueType> &,
              VectorComponentDataAccessor<TPixel, ScalarValueType> &);

  /**
   * Assignment operator
   */
  Self &operator=(const Self& orig)
  {
    NeighborhoodIterator<TPixel, VDimension>::operator=(orig);
    return *this;
  }
  
protected:
  /**
   * Sets the loop upper boundaries for iteration.
   */
  void SetBound(const SizeType &);

};
  
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionNeighborhoodIterator.txx"
#endif

#endif 

