/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmartRegionNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkSmartRegionNeighborhoodIterator_h
#define __itkSmartRegionNeighborhoodIterator_h

#include "itkNeighborhoodIterator.h"

namespace itk {

/**
 * \class SmartRegionNeighborhoodIterator
 * \brief Subclass of NeighborhoodIterator designed for iteration
 * over an itk::Image region of interest that performs bounds
 * checking and resolves boundary conditions.
 *
 * SmartRegionNeighborhoodIterator is a subclass of NeighborhoodIterator
 * that has the ability to detect when it is overlapping region of interest
 * boundaries.  This iterator will not attempt to dereference memory outside
 * the region of interest.  Pixel values outside the region of interest
 * are defined by the boundary condition.
 *
 * Bounds checking is performed and boundary conditions are resolved on
 * dereferencing, favoring iteration efficiency.
 *
 * \sa RegionBoundaryNeighborhoodIterator
 * \sa RegionNeighborhoodIterator
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 */
template<class TPixel, unsigned int VDimension = 2>
class SmartRegionNeighborhoodIterator
  :  public NeighborhoodIterator<TPixel, VDimension>
{
public:
  /** 
   * Standard "Self" typdef.
   */
  typedef SmartRegionNeighborhoodIterator  Self;

  /**
   *
   */
  typedef NeighborhoodIterator<TPixel, VDimension>  Superclass;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(SmartRegionNeighborhoodIterator, NeighborhoodIterator);

  /**
   * Default constructor.
   */
  SmartRegionNeighborhoodIterator() {};
  
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
   * itk::Neighborhood typedef support
   */
  typedef Neighborhood<TPixel, VDimension> NeighborhoodType;
  
  /**
   * Scalar data type typedef support
   */
  typedef typename ScalarTraits<TPixel>::ScalarValueType ScalarValueType;

  /**
   * Constructor establishes a neighborhood of iterators of a specified
   * dimension to walk a particular image and a particular region of
   * that image.
   */ 
  SmartRegionNeighborhoodIterator(const SizeType& radius,
                                  ImageType *ptr,
                                  const RegionType& region)
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
  virtual void SetEnd()
  {
    m_EndPointer = this->End().operator[](this->size()>>1);
  }
  /**
   *
   */
  virtual void SetToBegin()
  {
    *this = this->Begin();
  }

  /**
   * "Dereferences" the iterator. Returns the Neighborhood of values in the
   * itk::Image at the position of the iterator.
   */
  Neighborhood<TPixel, VDimension> GetNeighborhood();

  /**
   * Returns the pixel value referenced by a linear array location.
   */
  virtual TPixel GetPixel(unsigned long i)
  {
    if (this->InBounds())
      {
        return Superclass::GetPixel(i);
      }
    else
      {
        return (this->GetNeighborhood())[i];
      }
  }
  
  /**
   * Sets the values in the itk::Image at the iterator location to the values
   * contained in a Neighborhood.
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
   * the NeighborhoodIterator's to avoid the penalty of creating a temporary
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
    Superclass::operator=(orig);
    memcpy(m_InnerBoundsLow, orig.m_InnerBoundsLow, sizeof(long int) *
           VDimension);
    memcpy(m_InnerBoundsHigh, orig.m_InnerBoundsHigh, sizeof(long int) *
           VDimension);
    memcpy(m_InBounds, orig.m_InBounds, sizeof(bool) * VDimension);
    return *this;
  }

  
protected:
  /**
   * Sets loop boundaries for iteration.
   */
  void SetBound(const SizeType&);

  /**
   * Returns false if the iterator overlaps region boundaries, true
   * otherwise.  Also updates an internal boolean array indicating
   * which of the iterator's sides are out of bounds.
   */
  bool InBounds();

  /**
   * Lower threshold of in-bounds loop counter values.
   */
  long int m_InnerBoundsLow[VDimension];
  
  /**
   * Upper threshold of in-bounds loop counter values.
   */
  long int m_InnerBoundsHigh[VDimension];
  
  /**
   * Denotes which of the iterators dimensional sides spill outside
   * region of interest boundaries.
   */
  bool m_InBounds[VDimension];

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSmartRegionNeighborhoodIterator.txx"
#endif

#endif 

