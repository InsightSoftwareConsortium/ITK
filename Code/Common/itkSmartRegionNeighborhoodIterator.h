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
   * Index, Image, & Neighborhood  typedef support. While these were already
   * typdef'ed in the superclass, they need to be redone here for this subclass
   * to compile properly with gcc. Note that we have to rescope back to
   * itk:: so that it is not confused with ImageIterator::.
   */
  typedef itk::Image<TPixel, VDimension>        Image;
  typedef itk::Index<VDimension>                Index;
  typedef itk::Neighborhood<TPixel, VDimension> Neighborhood;
  typedef itk::ImageRegion<VDimension>          Region;
  typedef itk::Size<VDimension>                 Size;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(SmartRegionNeighborhoodIterator, NeighborhoodIterator);

  /**
   * Constructor establishes a neighborhood of iterators of a specified
   * dimension to walk a particular image and a particular region of
   * that image.
   */ 
  SmartRegionNeighborhoodIterator(const Size& radius,
                                  Image *ptr,
                                  const Region& region)
    : NeighborhoodIterator<TPixel, VDimension>(radius, ptr, region)
  {
    this->SetBound(region.GetSize());
  }

  /**
   * Return an iterator for the beginning of the region.
   */
  Self Begin();

  /**
   * Return an iterator for the end of the region.
   */
  Self End();

  /**
   * "Dereferences" the iterator. Returns the Neighborhood of values in the
   * itk::Image at the position of the iterator.
   */
  Neighborhood GetNeighborhood();

  /**
   * Sets the values in the itk::Image at the iterator location to the values
   * contained in a Neighborhood.
   */
  void SetNeighborhood(Neighborhood &);

  /**
   * Prints information about the neighborhood pointer structure to
   * std::cout for debugging purposes.
   */
  void Print();

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
  TPixelScalarValueType InnerProduct(std::valarray<TPixel> &);
  TPixelScalarValueType SlicedInnerProduct(const std::slice &s,
                                           std::valarray<TPixel> &v);

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
  void SetBound(const Size&);

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

private:
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSmartRegionNeighborhoodIterator.txx"
#endif

#endif 

