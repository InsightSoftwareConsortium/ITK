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
class RegionBoundaryNeighborhoodIterator
  :  public SmartRegionNeighborhoodIterator<TPixel, VDimension>
{
public:
  /** 
   * Standard "Self" typedef support.
   */
  typedef RegionBoundaryNeighborhoodIterator Self;

  /**
   * Index, Image, & Neighborhood  typedef support. While these were already
   * typdef'ed in the superclass, they need to be redone here for this subclass
   * to compile properly with gcc. Note that we have to rescope back to
   * itk:: so that it is not confused with ImageIterator::.
   */
  typedef itk::Image<TPixel, VDimension>        Image;
  typedef itk::Index<VDimension>                Index;
  typedef itk::Neighborhood<TPixel, VDimension> Neighborhood;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegionBoundaryNeighborhoodIterator,
               SmartRegionNeighborhoodIterator);

   /**
   * Constructor establishes a neighborhood of iterators of a specified
   * dimension to walk a particular image and a particular region of
   * that image.
   */ 
  RegionBoundaryNeighborhoodIterator(const unsigned long radius[VDimension],
                                     Image * ptr,
                                     const Index &start,
                                     const unsigned long bound[VDimension])
    : m_InnerStride(0),
      SmartRegionNeighborhoodIterator<TPixel, VDimension>(radius, ptr,
                                                          start, bound)
  { this->SetBound(bound); }

  /**
   * Overloaded from itkNeighborhoodPointerBase because this
   * iterator follows a different path across a region.
   */ 
  const NeighborhoodIterator<TPixel, VDimension> &operator++();  

  /**
   * Return an iterator for the beginning of the region.
   */
  Self Begin();

  /**
   * Return an iterator for the end of the region.
   */
  Self End();

  /**
   * Print some debugging information.
   */
  void Print();
  
protected:
  /**
   * Sets the loop boundaries for iteration.
   */
  void SetBound(const unsigned long []);

private:
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

