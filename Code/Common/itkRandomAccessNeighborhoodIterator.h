/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomAccessNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
template<class TImage,
  class TAllocator =
    NeighborhoodAllocator<ITK_TYPENAME TImage::InternalPixelType *>,
  class TDerefAllocator =
    NeighborhoodAllocator<ITK_TYPENAME TImage::PixelType>
  >
class RandomAccessNeighborhoodIterator
  :  public RegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator> 
{
public:
  /** 
   * Standard "Self" & Superclass typedef support.
   */
  typedef RandomAccessNeighborhoodIterator Self;
  typedef RegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
   Superclass;

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
  typedef typename Superclass::OffsetType OffsetType;
  
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
    : RegionNeighborhoodIterator<TImage, TAllocator,
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
    : RegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>(other)
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
   * Addition of an itk::Offset.  Note that this method does not do any bounds
   * checking.  Adding an offset that moves the iterator out of its assigned
   * region will produce undefined results.
   */
  Self &operator+=(const OffsetType &);

  /**
   * Subtraction of an itk::Offset. Note that this method does not do any bounds
   * checking.  Subtracting an offset that moves the iterator out of its
   * assigned region will produce undefined results.
   */
  Self &operator-=(const OffsetType &);

  /**
   * Distance between two iterators
   */
  OffsetType operator-(const Self& b)
  {  return m_Loop - b.m_Loop;  }
  
  /**
   * Standard print method.
   */
  virtual void PrintSelf(std::ostream &os, Indent i) const
  {
    os << i << "RandomAccessNeighborhoodIterator" << std::endl;
    Superclass::PrintSelf(os, i.GetNextIndent());
  }
};

template<class TImage, class TAccessor, class TDerefAccessor>
inline RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor>
operator+(const RandomAccessNeighborhoodIterator<TImage, TAccessor, 
          TDerefAccessor> &it,
          const typename RandomAccessNeighborhoodIterator<TImage, TAccessor,
          TDerefAccessor>::OffsetType &ind)
{
  RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor> ret;
  ret = it;
  ret += ind;
  return ret;
}

template<class TImage, class TAccessor, class TDerefAccessor>
inline RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor>
operator+(const typename RandomAccessNeighborhoodIterator<TImage, TAccessor,
          TDerefAccessor>::OffsetType &ind,
          const RandomAccessNeighborhoodIterator<TImage, TAccessor, 
          TDerefAccessor> &it)
{  return (it + ind); }

template<class TImage, class TAccessor, class TDerefAccessor>
inline RandomAccessNeighborhoodIterator<TImage, TAccessor,
  TDerefAccessor>
operator-(const RandomAccessNeighborhoodIterator<TImage, TAccessor, 
          TDerefAccessor> &it,
          const typename RandomAccessNeighborhoodIterator<TImage, TAccessor,
          TDerefAccessor>::OffsetType &ind)
{
  RandomAccessNeighborhoodIterator<TImage, TAccessor, TDerefAccessor> ret;
  ret = it;
  ret -= ind;
  return ret;
}

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomAccessNeighborhoodIterator.txx"
#endif

#endif 
