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
template<class TPixel, unsigned int VDimension = 2,
  class TAllocator = NeighborhoodAllocator<TPixel *>,
  class TDerefAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT SmartRegionNeighborhoodIterator
  :  public NeighborhoodIterator<TPixel, VDimension, TAllocator,
  TDerefAllocator>
{
public:
  /** 
   * Standard "Self" & Superclass typdef.
   */
  typedef SmartRegionNeighborhoodIterator Self;
  typedef NeighborhoodIterator<TPixel, VDimension, TAllocator, TDerefAllocator>
  Superclass;

  /**
   * Some common itk object typedefs
   */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::SizeType SizeType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;

  /**
   * Scalar data type typedef support
   */
  typedef typename Superclass::ScalarValueType ScalarValueType;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(SmartRegionNeighborhoodIterator, NeighborhoodIterator);

  /**
   * Default constructor.
   */
  SmartRegionNeighborhoodIterator() {};

  /**
   * Copy constructor
   */
  SmartRegionNeighborhoodIterator(const Self& orig)
    : NeighborhoodIterator<TPixel, VDimension, TAllocator,
    TDerefAllocator>(orig)
  {
    memcpy(m_InnerBoundsLow, orig.m_InnerBoundsLow, sizeof(long int) *
           VDimension);
    memcpy(m_InnerBoundsHigh, orig.m_InnerBoundsHigh, sizeof(long int) *
           VDimension);
    memcpy(m_InBounds, orig.m_InBounds, sizeof(bool) * VDimension);
  }
  
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
 
  /**
   * Constructor establishes a neighborhood of iterators of a specified
   * dimension to walk a particular image and a particular region of
   * that image.
   */ 
  SmartRegionNeighborhoodIterator(const SizeType& radius,
                                  ImageType *ptr,
                                  const RegionType& region)
  {  this->Initialize(radius, ptr, region);  }

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
  {    m_EndPointer = this->End().operator[](this->Size()>>1);  }
  /**
   *
   */
  virtual void SetToBegin()
  {    *this = this->Begin();  }

  /**
   * "Dereferences" the iterator. Returns the Neighborhood of values in the
   * itk::Image at the position of the iterator.
   */
  NeighborhoodType GetNeighborhood();

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
  virtual void PrintSelf(std::ostream &, Indent) const;

  /**
   * Returns false if the iterator overlaps region boundaries, true
   * otherwise.  Also updates an internal boolean array indicating
   * which of the iterator's sides are out of bounds.
   */
  bool InBounds();
  
protected:
  /**
   * Sets loop boundaries for iteration.
   */
  void SetBound(const SizeType&);

  /**
   * Lower threshold of in-bounds loop counter values.
   */
  unsigned long int m_InnerBoundsLow[VDimension];
  
  /**
   * Upper threshold of in-bounds loop counter values.
   */
  unsigned long int m_InnerBoundsHigh[VDimension];
  
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

