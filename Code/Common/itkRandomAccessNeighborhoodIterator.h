/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomAccessNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRandomAccessNeighborhoodIterator_h
#define __itkRandomAccessNeighborhoodIterator_h

#include "itkRegionNeighborhoodIterator.h"
#include "itkIndex.h"

namespace itk {

/**
 * \class RandomAccessNeighborhoodIterator
 * \brief A random access iterator that maintains pointers to arbitrarily-sized
 * neighborhoods of values in an image.
 * 
 * This is a neighborhood  iterator (maintains pointers to a neighborhood of
 * values in an itk::Image) that provides both increment  and decrement operators 
 * (subclasses RegionNeighborhoodIterator), and that also provides
 * constant-time methods for moving forward and backward in arbitrary-sized
 * steps along any dimension.
 * 
 * RandomAccessNeighborhoodIterator only maintains counters for loop position and
 * upper bounds, and so it is "unaware" when it is overlapping a region
 * boundary. You can only safely use this iterator on regions sufficiently contained
 * within the itk::Image buffer.  Adding an itk::Index with a length exceeding
 * the distance to any image boundary will have undefined results.
 *
 * \sa NeighborhoodIterator
 * \sa RegionNeighborhoodIterator
 * \sa RandomAccessBoundaryNeighborhoodIterator
 * \sa SmartRegionNeighborhoodIterator
 * \sa Neighborhood
 * \sa NeighborhoodAlgorithm
 */
template<class TPixel, unsigned int VDimension = 2>
class RandomAccessNeighborhoodIterator
  :  public RegionNeighborhoodIterator<TPixel, VDimension>
{
public:
  /** 
   * Standard "Self" typedef support.
   */
  typedef RandomAccessNeighborhoodIterator Self;

  /**
   * Standard Superclass typedef
   */
  typedef RegionNeighborhoodIterator<TPixel, VDimension> Superclass;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RandomAccessNeighborhoodIterator, RegionNeighborhoodIterator);

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
   * Scalar data type typedef support
   */
  typedef typename ScalarTraits<TPixel>::ScalarValueType ScalarValueType;

  /**
   * itk::Neighborhood typedef support
   */
  typedef Neighborhood<TPixel, VDimension> NeighborhoodType;
  
  /**
   * Default constructor
   */
  RandomAccessNeighborhoodIterator()
    : RegionNeighborhoodIterator<TPixel, VDimension>() {};
  
  /**
  * Constructor establishes a neighborhood of iterators of a specified
  * dimension to walk a particular image and a particular region of
  * that image.
  */
  RandomAccessNeighborhoodIterator(const SizeType &radius,
                             ImageType *ptr,
                             const RegionType &region)
    : RegionNeighborhoodIterator<TPixel, VDimension>(radius, ptr, region) { }

  /**
   * Return an iterator for the beginning of the region.
   */
  Self Begin();

  /**
   * Return an iterator for the end of the region.
   */
  Self End();

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
   * Assignment operator
   */
  Self &operator=(const Self& orig)
  {
    Superclass::operator=(orig);
    return *this;
  }

  /**
   * Addition of an itk::Index
   */
  Self &operator+=(const Index<VDimension> &);

  /**
   * Subtraction of an itk::Index
   */
  Self &operator-=(const Index<VDimension> &);

  /**
   * Distance between two iterators
   */
  Index<VDimension> operator-(const Self& b)
  {
    Index<VDimension> idx;
    for (int i = 0; i < VDimension; ++i) idx[i] = m_Loop[i] - b.m_Loop[i];
    return idx;
  }
  
};

template<class TPixel, unsigned int VDimension>
inline RandomAccessNeighborhoodIterator<TPixel, VDimension>
operator+(const RandomAccessNeighborhoodIterator<TPixel, VDimension> &it,
          const Index<VDimension> &ind)
{
  RandomAccessNeighborhoodIterator<TPixel, VDimension> ret;
  ret = it;
  ret += ind;
  return ret;
}

template<class TPixel, unsigned int VDimension>
inline RandomAccessNeighborhoodIterator<TPixel, VDimension>
operator+(const Index<VDimension> &ind,
          const RandomAccessNeighborhoodIterator<TPixel, VDimension> &it)
{
  return (it + n);
}

template<class TPixel, unsigned int VDimension>
inline RandomAccessNeighborhoodIterator<TPixel, VDimension>
operator-(const RandomAccessNeighborhoodIterator<TPixel, VDimension> &it,
          const Index<VDimension> &ind)
{
  RandomAccessNeighborhoodIterator<TPixel, VDimension> ret;
  ret = it;
  ret -= ind;
  return ret;
}

template<class TPixel, unsigned int VDimension>
inline RandomAccessNeighborhoodIterator<TPixel, VDimension>
operator-(const Index<VDimension> &ind,
          const RandomAccessNeighborhoodIterator<TPixel, VDimension> &it)
{
  return (it - n);
}

  
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomAccessNeighborhoodIterator.txx"
#endif

#endif 

