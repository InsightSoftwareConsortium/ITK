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
template<class TPixel, unsigned int VDimension = 2,
  class TAllocator = NeighborhoodAllocator<TPixel *>,
  class TDerefAllocator = NeighborhoodAllocator<TPixel> >
class RandomAccessNeighborhoodIterator
  :  public RegionNeighborhoodIterator<TPixel, VDimension, TAllocator,
  TDerefAllocator> 
{
public:
  /** 
   * Standard "Self" & Superclass typedef support.
   */
  typedef RandomAccessNeighborhoodIterator Self;
  typedef RegionNeighborhoodIterator<TPixel, VDimension, TAllocator,
    TDerefAllocator> Superclass;
  
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
   * Default constructor
   */
  RandomAccessNeighborhoodIterator() {};
  
  /**
  * Constructor establishes a neighborhood of iterators of a specified
  * dimension to walk a particular image and a particular region of
  * that image.
  */
  RandomAccessNeighborhoodIterator(const SizeType &radius,
                             ImageType *ptr,
                             const RegionType &region)
    : RegionNeighborhoodIterator<TPixel, VDimension, TAllocator,
    TDerefAllocator>(radius, ptr, region) { }

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
   * Copy constructor
   */
  RandomAccessNeighborhoodIterator( const Self &other)
    : RegionNeighborhoodIterator<TPixel, VDimension, TAllocator,
    TDerefAllocator>(other)
   { }
  
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
  
  /**
   * Standard print method.
   */
  virtual void PrintSelf(std::ostream &os, Indent i) const
  {
    os << i << "RandomAccessNeighborhoodIterator" << std::endl;
    Superclass::PrintSelf(os, i.GetNextIndent());
  }
};

template<class TPixel, unsigned int VDimension, class TAccessor,
    class TDerefAccessor>
inline RandomAccessNeighborhoodIterator<TPixel, VDimension, TAccessor,
  TDerefAccessor>
operator+(const RandomAccessNeighborhoodIterator<TPixel, VDimension, TAccessor, 
          TDerefAccessor> &it, const Index<VDimension> &ind)
{
  RandomAccessNeighborhoodIterator<TPixel, VDimension, TAccessor,
    TDerefAccessor> ret;
  ret = it;
  ret += ind;
  return ret;
}

template<class TPixel, unsigned int VDimension, class TAccessor,
  class TDerefAccessor>
inline RandomAccessNeighborhoodIterator<TPixel, VDimension, TAccessor,
  TDerefAccessor>
operator+(const Index<VDimension> &ind,
          const RandomAccessNeighborhoodIterator<TPixel, VDimension, TAccessor, 
          TDerefAccessor> &it)
{  return (it + ind); }

template<class TPixel, unsigned int VDimension, class TAccessor,
  class TDerefAccessor>
inline RandomAccessNeighborhoodIterator<TPixel, VDimension, TAccessor,
  TDerefAccessor>
operator-(const RandomAccessNeighborhoodIterator<TPixel, VDimension, TAccessor, 
          TDerefAccessor> &it,
          const Index<VDimension> &ind)
{
  RandomAccessNeighborhoodIterator<TPixel, VDimension, TAccessor,
    TDerefAccessor> ret;
  ret = it;
  ret -= ind;
  return ret;
}

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomAccessNeighborhoodIterator.txx"
#endif

#endif 

