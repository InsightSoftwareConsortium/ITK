/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstRandomAccessNeighborhoodIterator.h
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
#ifndef __itkConstRandomAccessNeighborhoodIterator_h
#define __itkConstRandomAccessNeighborhoodIterator_h

#include "itkConstNeighborhoodIterator.h"

namespace itk {

/**
 * \class ConstRandomAccessNeighborhoodIterator
 *
 * \brief Const version of RandomAccessNeighborhoodIterator, defining iteration
 * of a local N-dimensional neighborhood of pixels across an itk::Image.
 *
 * ConstRandomAccessNeighborhoodIterator implements the read-only methods of
 * RandomAccessNeighborhoodIterator.  It serves as a base class from which
 * other iterators are derived. See RandomAccessNeighborhoodIterator for more
 * complete information.
 *
 * \sa Neighborhood \sa ImageIterator \sa NeighborhoodIterator
 * \sa SmartNeighborhoodIterator \sa RandomAccessNeighborhoodIterator
 * */
template<class TImage>
class ITK_EXPORT ConstRandomAccessNeighborhoodIterator
  :  public ConstNeighborhoodIterator<TImage>
{
public:
  
  /** 
   * Standard "Self" & Superclass typedef support.
   */
  typedef ConstRandomAccessNeighborhoodIterator Self;
  typedef ConstNeighborhoodIterator<TImage> Superclass;

  /**
   * Extract typedefs from superclass
   */
  typedef typename Superclass::InternalPixelType InternalPixelType;
  typedef typename Superclass::PixelType  PixelType;
  typedef typename Superclass::SizeType   SizeType;
  typedef typename Superclass::ImageType  ImageType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::IndexType  IndexType;
  typedef typename Superclass::OffsetType OffsetType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  enum {Dimension = Superclass::Dimension };
  typedef typename Superclass::Iterator      Iterator;
  typedef typename Superclass::ConstIterator ConstIterator;
  typedef typename Superclass::ImageBoundaryConditionPointerType
   ImageBoundaryConditionPointerType;
  typedef typename Superclass::ScalarValueType ScalarValueType;
  
  /**
   * Default constructor
   */
  ConstRandomAccessNeighborhoodIterator()
    : Superclass()
    {}

  /**
   * Virtual destructor
   */
  virtual ~ConstRandomAccessNeighborhoodIterator() {}
  
  /**
   * Copy constructor
   */
  ConstRandomAccessNeighborhoodIterator( const
                                         ConstRandomAccessNeighborhoodIterator
                                         &orig )
    : Superclass(orig)
    {}
  
  /**
   * Constructor which establishes the region size, neighborhood, and image
   * over which to walk.
   */
  ConstRandomAccessNeighborhoodIterator(const SizeType &radius,
                       const ImageType * ptr,
                       const RegionType &region
                                        )
    : Superclass(radius, ptr, region)
    {}

  /**
   * Assignment operator
   */
  Self &operator=(const Self& orig)
    {  return Superclass::operator=(orig);  }

  /**
   * Standard itk print method
   */
  virtual void PrintSelf(std::ostream &, Indent) const;

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
  
};

template<class TImage>
inline ConstRandomAccessNeighborhoodIterator<TImage>
operator+(const ConstRandomAccessNeighborhoodIterator<TImage> &it,
          const typename ConstRandomAccessNeighborhoodIterator<TImage>
          ::OffsetType &ind)
{
  ConstRandomAccessNeighborhoodIterator<TImage> ret;
  ret = it;
  ret += ind;
  return ret;
}

template<class TImage>
inline ConstRandomAccessNeighborhoodIterator<TImage>
operator+(const typename ConstRandomAccessNeighborhoodIterator<TImage>
          ::OffsetType &ind,
          const ConstRandomAccessNeighborhoodIterator<TImage> &it)
{  return (it + ind); }

template<class TImage>
inline ConstRandomAccessNeighborhoodIterator<TImage>
operator-(const ConstRandomAccessNeighborhoodIterator<TImage> &it,
          const typename ConstRandomAccessNeighborhoodIterator<TImage>
          ::OffsetType &ind)
{
  ConstRandomAccessNeighborhoodIterator<TImage> ret;
  ret = it;
  ret -= ind;
  return ret;
}


  
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstRandomAccessNeighborhoodIterator.txx"
#endif

#endif
