/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionBoundaryNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegionBoundaryNeighborhoodIterator_h
#define __itkRegionBoundaryNeighborhoodIterator_h

#include "itkSmartRegionNeighborhoodIterator.h"

namespace itk {

/**
 * \class RegionBoundaryNeighborhoodIterator
 * \brief A "smart" neighborhood region iterator that is constrained
 * to pixels on a region of interest boundary.
 *
 * RegionBoundaryNeighborhoodIterator traverses the pixels on region
 * boundaries. Region boundaries are defined as pixels whose neighborhood
 * overlaps the edge of the region.  RegionBoundaryNeighborhoodIterator
 * is a subclass of SmartRegionNeighborhoodIterator and inherits
 * functionality to resolve boundary conditions.
 *
 * RegionBoundaryNeighborhoodIterator will visit all the pixels in
 * a region where the neighborhood described by the iterator overlaps
 * the edge of the region.  The order in which it visits the pixels
 * is not guaranteed.  See NeighborhoodIterator documentation for
 * ways of guaranteeing spatial fidelity between input and output.
 *
 */
template<class TPixel, unsigned int VDimension = 2>
class ITK_EXPORT RegionBoundaryNeighborhoodIterator
  :  public SmartRegionNeighborhoodIterator<TPixel, VDimension>
{
public:
  /** 
   * Standard "Self" typedef support.
   */
  typedef RegionBoundaryNeighborhoodIterator Self;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegionBoundaryNeighborhoodIterator,
               SmartRegionNeighborhoodIterator);
  
  /**
   * Default constructor.
   */
  RegionBoundaryNeighborhoodIterator() {};
  
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
   * Constructor establishes a neighborhood of iterators of a specified
   * dimension to walk a particular image and a particular region of
   * that image.
   */ 
  RegionBoundaryNeighborhoodIterator(const SizeType& radius,
                                     ImageType * ptr,
                                     const RegionType& region)
  {
    this->Initialize(radius, ptr, region);
  }

  /**
   * Overridden from itkNeighborhoodPointerBase because this
   * iterator follows a different path across a region.
   */ 
  const NeighborhoodIterator<TPixel, VDimension> &operator++();  

  /**
   * Overridden from itkNeighborhoodPointerBase because this
   * iterator follows a different path across a region.
   */ 
  const NeighborhoodIterator<TPixel, VDimension> &operator--();  

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
   * Print some debugging information.
   */
  void PrintSelf();

  /**
   * Assignment operator
   */
  Self &operator=(const Self& orig)
  {
    Superclass::operator=(orig);
    m_InnerStride = orig.m_InnerStride;
    return *this;
  }
  
protected:
  /**
   * Sets the loop boundaries for iteration.
   */
  void SetBound(const SizeType&);

  /**
   * The iterator strides needed to move between inner boundary pixels
   * at opposite ends of a dimensional side.
   */
  unsigned long  m_InnerStride;
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionBoundaryNeighborhoodIterator.txx"
#endif

#endif 

