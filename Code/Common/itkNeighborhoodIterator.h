/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkNeighborhoodIterator_h
#define __itkNeighborhoodIterator_h

#include <vector>
#include <cstring>
#include <iostream>
#include "itkImage.h"
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkMacro.h"
#include "itkNeighborhoodBase.h"

namespace itk {

/**
 * \class NeighborhoodIterator
 * \brief A virtual object that defines the basic API and common functionality
 * for all flavors of NeighborhoodIterators.
 *
 * NeighborhoodIterator is a virtual object that defines the API and 
 * common functions for all types of NeighborhoodIterators.  A
 * NeighborhoodIterator is a neighborhood of pointers to itk::Image pixels
 * that can be moved sequentially across the image.  A NeighborhoodIterator
 * can be "dereferenced" to obtain a Neighborhood of values.  Certain
 * Neighborhood operations on itk::Image pixel values can be applied directly
 * on a NeighborhoodIterator for efficiency.
 *
 * Dereferencing is handled differently by each subclass.
 * SmartRegionNeighborhoodIterator's, for example, perform bounds checking
 * and handle boundary conditions during dereferencing, while
 * RegionNeighborhoodIterator's do not.
 *
 * The API for creating and manipulating a NeighborhoodIterator mimics
 * that of the itk::ImageIterators.  Like the itk::ImageIterator, a
 * NeighborhoodIterator is defined on a region of interest in an itk::Image.
 * Iteration is constrained within that region of interest.  "Smart"
 * NeighborhoodIterators exist for handling boundary conditions and for
 * constraining iteration to the boundaries of a region.
 *
 * Each subclass of a NeighborhoodIterator also defines its own mechanism for
 * iteration through an image.  In general, the Iterator does not directly
 * keep track of its spatial location in the image, but uses a set of
 * internal loop variables and offsets to trigger wraps at itk::Image region
 * boundaries, and to identify the end of the itk::Image region.
 *
 * It is important to note that NeighborhoodIterators are "forward iterators".
 * They move only in one direction.  Furthermore, there is no guaranteed
 * path that a NeighborhoodIterator will follow across an image memory
 * buffer.  When a NeighborhoodIterator is incremented by one, for example,
 * it cannot be assumed that its internal pointers have been incremented by
 * one.
 *
 * There are two easy ways to spatially synchronize the output of
 * neighborhood operations with the movements of a NeighborhoodIterator:
 *
 * (1) A NeighborhoodIterator can be given a pointer to an output buffer that
 * is moved in synch with the center position of the NeighborhoodIterator.
 * The output buffer is assumed to be the same size as the itk::Image buffer
 * on which the NeighborhoodIterator is defined.  Scalar output can be written
 * directly to the synchronized pointer location.
 *
 * (2) Like NeighborhoodIterators are guaranteed to follow the same path
 * through itk::Image buffers with equal region sizes and region start
 * locations.  In other words, you can write output through a second
 * NeighborhoodIterator defined on the output buffer that you increment
 * in synch with the original.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa ImageIterator
 * \sa RegionNeighborhoodIterator
 * \sa SmartRegionNeighborhoodIterator
 * \sa RegionBoundaryNeighborhoodIterator
 */
template<class TPixel, unsigned int VDimension = 2>
class NeighborhoodIterator
  :  public NeighborhoodBase<TPixel *, VDimension>
{
public:
  /** 
   * Standard "Self" typedef support.
   */
  typedef NeighborhoodIterator Self;

  /**
   * itk::Image typedef support.
   */
  typedef Image<TPixel, VDimension> Image;

  /**
   * itk::Size typedef support
   */
  typedef itk::Size<VDimension> Size;

  /**
   * Region typedef support.
   */
  typedef ImageRegion<VDimension> Region;
  
  /**
   * itk::Index typedef support.
   */
  typedef Index<VDimension> Index;
  
  /**
   * itk::Neighborhood typedef support
   */
  typedef Neighborhood<TPixel, VDimension> Neighborhood;
  
  /**
   * itk::NeighborhoodBase typedef support
   */
  typedef NeighborhoodBase<TPixel *, VDimension> NeighborhoodBase;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(NeighborhoodIterator, NeighborhoodBase);

 /**
  * Constructor establishes a neighborhood of iterators of a specified
  * dimension to walk a particular image and a particular region of
  * that image.
  */
  NeighborhoodIterator(const Size &radius,
                       Image * ptr,
                       const Region &region
                       )
    : m_OutputBuffer(0)
  {
    this->SetRadius(radius);
    m_Image = ptr;
    m_Buffer = m_Image->GetBufferPointer();
    this->SetStartIndex(region.GetIndex());
    this->SetLocation(region.GetIndex());
  }
  
  /**
   * Increments the pointers in the NeighborhoodIterator,
   * wraps across boundaries automatically, accounting for
   * the disparity in the buffer size and the region size of the
   * image.
   *
   * This function may be overridden by a subclass.
   */
  virtual const Self &operator++();  

  /**
   * Virtual function that replaces the pixel values in the image
   * neighborhood "pointed to" by the NeighborhoodIterator with
   * the pixel values contained in a Neighborhood.
   *
   * This method must be defined by each subclass according to how
   * that subclass handles boundary conditions.
   */
  virtual void SetNeighborhood(Neighborhood &) = 0;

  /**
   * Virtual function that "dereferences" a NeighborhoodPointer,
   * returning a Neighborhood of pixel values.
   *
   * This function must be implemented separately by each
   * subclass according to how that subclass handles boundary
   * conditions.
   */
  virtual Neighborhood GetNeighborhood() = 0;

  /**
   * Returns a const pointer to an internal array of offsets that 
   * provide support for regions of interest.  An offset for each dimension
   * is necessary to shift pointers when wrapping around region edges because
   * region memory is not necessarily contiguous within the buffer.
   */
  const unsigned long* GetWrapOffset() const
  {
    return m_WrapOffsets;
  }
  
  /**
   * Returns the internal offset associated with wrapping around a single
   * dimension's region boundary in the itk::Image.  An offset for each
   * dimension is necessary to shift pointers when wrapping around region
   * edges because region memory is not necessarily contiguous within the
   * buffer.
   */
  unsigned long GetWrapOffset(unsigned int n) const
  {
    return m_WrapOffsets[n];
  }

  /**
   * Returns a const pointer to an internal array of upper loop bounds used
   * during iteration.
   */
  const unsigned long* GetBound() const
  {
    return m_Bound;
  }

  /**
   * Returns the internal loop bound used to define the edge of a single
   * dimension in the itk::Image region.
   */
  unsigned long GetBound(unsigned int n) const
  {
    return m_Bound[n];
  }

  /**
   * Returns a boolean == comparison of the memory addresses of the center
   * elements of two NeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator==(const Self &it)
  {
    return  it[this->size()>>1] == (*this)[this->size()>>1];
  }
  
  /**
   * Returns a boolean != comparison of the memory addresses of the center
   * elements of two NeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator!=(const Self &it)
  {
    return  it[this->size()>>1] != (*this)[this->size()>>1];
  }
  
  /**
   * Returns a boolean < comparison of the memory addresses of the center
   * elements of two NeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator<(const Self &it)
  {
    return  (*this)[this->size()>>1] < it[this->size()>>1];
  }
 
  /**
   * Returns a boolean < comparison of the memory addresses of the center
   * elements of two NeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator<=(const Self &it)
  {
    return  (*this)[this->size()>>1] <= it[this->size()>>1];
  }
  
  /**
   * Returns a boolean > comparison of the memory addresses of the center
   * elements of two NeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator>(const Self &it)
  {
    return  (*this)[this->size()>>1] > it[this->size()>>1];
  }

  /**
   * Returns a boolean >= comparison of the memory addresses of the center
   * elements of two NeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator>=(const Self &it)
  {
    return  (*this)[this->size()>>1] >= it[this->size()>>1];
  }

  /**
   * Returns the value referenced at the center of the NeighborhoodIterator.
   */
  TPixel Center() const
  {
    return *(this->operator[]((this->size())>>1));
  }

  /**
   * Returns the central pointer of the neighborhood.
   */
  TPixel *CenterPointer() const
  {
    return (this->operator[]((this->size())>>1));
  }
  
  /**
   * Sets the internal pointer to a memory buffer that is incremented
   * in synch with the center of the NeighborhoodIterator.  This
   * internal pointer can be used to guarantees spatial fidelity between
   * inputs and outputs to an algorithm that uses NeighborhoodIterators.
   * \sa GetOutputBuffer
   */
  void SetOutputBuffer(TPixel *i)
  {
    m_OutputBuffer = i; 
  }

  /**
   * Returns the current memory location of the internal output
   * pointer that is synchronized with the iterator.
   * \sa SetOutputBuffer
   */
  TPixel *GetOutputBuffer() const
  {
    return m_OutputBuffer;
  }
  
  /**
   * This method positions the iterator at an indexed location in the
   * image. SetLocation should _NOT_ be used to update the position of the
   * iterator during iteration, only for initializing it to a position
   * prior to iteration.  This method is not optimized for speed.
   */
  void SetLocation( const Index& position )
  {
    this->SetLoop(position);
    this->SetPixelPointers(position);
  }

  
protected:
  /**
   * Default method for setting the coordinate location of the iterator.
   * Loop indicies correspond to the actual Image region index.
   * This correspondance is a coincidental feature that will not
   * necessarily be supported.
   */
  virtual void SetLoop( const Index& position )
  {
    memcpy(m_Loop, position.m_Index, sizeof(long) * VDimension);
  }
  
  /**
   * Default method for setting the index of the first pixel in the
   * iteration region.
   */
  virtual void SetStartIndex( const Index& start)
  {
    m_StartIndex = start;
  }

  /**
   * Virtual method for setting internal loop boundaries.  This
   * method must be defined in each subclass because
   * each subclass may handle loop boundaries differently.
   */
  virtual void SetBound(const Size &) = 0;

  /**
   * Default method for setting the values of the internal pointers
   * to itk::Image memory buffer locations.  This method should
   * generally only be called when the iterator is initialized.
   * \sa SetLocation
   */
  virtual void SetPixelPointers(const Index &);

  /**
   * The internal array of offsets that provide support for regions of interest.
   * An offset for each dimension is necessary to shift pointers when wrapping
   * around region edges because region memory is not necessarily contiguous
   * within the buffer.
   */
  unsigned long m_WrapOffset[VDimension];

  /**
   * An array of upper looping boundaries used during iteration.
   */
  unsigned long m_Bound[VDimension];

  /**
   * Array of loop counters used during iteration.
   */
  long m_Loop[VDimension];

  /**
   * A pointer to an output buffer that, if set, is moved in synch with
   * the center position of the NeighborhoodIterator.
   *
   * The output buffer is assumed to be the same size as the itk::Image buffer
   * on which the NeighborhoodIterator is defined.  Scalar output can be written
   * directly to the synchronized pointer location. 
   */
  TPixel *m_OutputBuffer;

  /**
   * The pointer to the itk::Image on which this NeighborhoodIterator is
   * defined on.
   */
  SmartPointer<Image> m_Image;

  /**
   * Shortcut to the data buffer of the itk::Image on which this
   * NeighborhoodIterator is defined.
   */
  TPixel *m_Buffer;

  /**
   * The starting index for iteration within the itk::Image region
   * on which this NeighborhoodIterator is defined.
   */
  Index  m_StartIndex;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodIterator.txx"
#endif

#endif 
