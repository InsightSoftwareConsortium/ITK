/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstNeighborhoodIterator.h
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
#ifndef __itkConstNeighborhoodIterator_h
#define __itkConstNeighborhoodIterator_h

#include <vector>
#include <string.h>
#include <iostream>
#include "itkImage.h"
#include "itkIndex.h"
#include "itkOffset.h"
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkMacro.h"
#include "itkNeighborhood.h"
#include "itkImageBoundaryCondition.h"

namespace itk {

/** \class ConstNeighborhoodIterator
 *
 * \brief Const version of NeighborhoodIterator, defining iteration of a local
 * N-dimensional neighborhood of pixels across an itk::Image.
 *
 * ConstNeighborhoodIterator implements the read-only methods of
 * NeighborhoodIterator.  It serves as a base class from which other iterators
 * are derived. See NeighborhoodIterator for more complete information.
 *
 * \sa Neighborhood \sa ImageIterator \sa NeighborhoodIterator
 * \sa SmartNeighborhoodIterator \sa RandomAccessNeighborhoodIterator
 **/
template<class TImage>
class ITK_EXPORT ConstNeighborhoodIterator
  :  public Neighborhood<ITK_TYPENAME TImage::InternalPixelType *,
                                           TImage::ImageDimension>
{
public:
  /**
   * Extract image type information.
   */
  typedef typename TImage::InternalPixelType InternalPixelType;
  typedef typename TImage::PixelType PixelType;
  enum {Dimension = TImage::ImageDimension };
  
  /** 
   * Standard "Self" & Superclass typedef support.
   */
  typedef ConstNeighborhoodIterator Self;
  typedef Neighborhood<InternalPixelType *, Dimension> Superclass;

  /**
   * Typedef support for common objects
   */
  typedef typename Superclass::SizeType SizeType;
  typedef TImage ImageType;
  typedef typename TImage::RegionType RegionType;
  typedef Index<Dimension> IndexType;
  typedef Offset<Dimension> OffsetType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef Neighborhood<PixelType, Dimension> NeighborhoodType;
  
  /**
   * Support for internal iterator types.  Only const iteration is supported
   * in this class.
   */
  typedef typename Superclass::ConstIterator ConstIterator;

  /**
   * Typedef for generic boundary condition pointer
   */
  typedef ImageBoundaryCondition<ImageType> *ImageBoundaryConditionPointerType;

  /**
   * Scalar data type typedef support
   */
  typedef typename ScalarTraits<PixelType>::ScalarValueType ScalarValueType;
  
  /**
   * Default constructor
   */
  ConstNeighborhoodIterator();

  /**
   * Virtual destructor
   */
  virtual ~ConstNeighborhoodIterator() {}
  
  /**
   * Copy constructor
   */
  ConstNeighborhoodIterator( const ConstNeighborhoodIterator & );
  
  /**
   * Constructor which establishes the region size, neighborhood, and image
   * over which to walk.
   */
  ConstNeighborhoodIterator(const SizeType &radius,
                       const ImageType * ptr,
                       const RegionType &region
                       )
    {      this->Initialize(radius, ptr, region);  }

  /**
   * Assignment operator
   */
  Self &operator=(const Self& orig);
  
  /**
   * Standard itk print method
   */
  virtual void PrintSelf(std::ostream &, Indent) const;

  /**
   * Computes the internal, N-d offset of a pixel array position n from 
   * (0,0, ..., 0) in the "upper-left" corner of the neighborhood.
   */
  OffsetType ComputeInternalIndex(unsigned int n) const;
  
  /**
   * Returns the pixel referenced at the center of the ConstNeighborhoodIterator.
   */
  PixelType GetCenterPixel() const
    {    return *( this->GetCenterPointer() );  }

  /**
   * Returns the array of upper loop bounds used during iteration.
   */
  IndexType GetBound() const
    {    return m_Bound;   }

  /**
   * Returns the loop bound used to define the edge of a single
   * dimension in the itk::Image region.
   */
  long GetBound(unsigned int n) const
    {    return m_Bound[n];  }
  
  /**
   * Returns the pointer to the center pixel of the neighborhood.
   */
  const InternalPixelType *GetCenterPointer() const
    {    return (this->operator[]((this->Size())>>1));  }
  
  /**
   * Returns a smartpointer to the image on which this iterator operates.
   */
  typename ImageType::ConstPointer GetImagePointer()
    { return m_ConstImage; }
 
  /**
   * Returns the N-dimensional index of the iterator's position in
   * the image.
   */
  virtual IndexType GetIndex() const
    { return m_Loop;  }
  
  /**
   * Virtual function that "dereferences" a ConstNeighborhoodIterator,
   * returning a Neighborhood of pixel values.
   */
  virtual NeighborhoodType GetNeighborhood() const;

  /**
   * Returns the current memory location of the internal output
   * pointer that is synchronized with the iterator.
   * \sa SetOutputBuffer
   */
  InternalPixelType *GetOutputBuffer() const
    {    return m_OutputBuffer;  }
  
  /**
   * Returns the offsets that will be used to adjust for differences in input
   * and output buffer sizes.
   */
  OffsetType GetOutputWrapOffsetModifier() const
    {  return m_OutputWrapOffsetModifier;  }

  /**
   * Returns the pixel value referenced at a linear array location.
   */
  virtual PixelType GetPixel(const unsigned long i) const
    {  return *(this->operator[](i));  }

  /**
   *  Returns the region of iteration.
   */
  RegionType GetRegion() const
    { return m_Region; }
  
  /**
   * Returns the N-dimensional starting index of the iterator's position on
   * the image.
   */
  IndexType GetBeginIndex() const
    { return m_BeginIndex; }
  
  /**
   * Returns the offsets used to wrap across dimensional boundaries.
   */
  OffsetType GetWrapOffset() const
    {  return m_WrapOffsets;  }

  /**
   * Returns the internal offset associated with wrapping around a single
   * dimension's region boundary in the itk::Image.  An offset for each
   * dimension is necessary to shift pointers when wrapping around region
   * edges because region memory is not necessarily contiguous within the
   * buffer.
   */
  long GetWrapOffset(unsigned int n) const
    {    return m_WrapOffsets[n];   }

  /**
   * Virtual method for rewinding the iterator to its beginning pixel.
   * This is useful for writing functions which take neighborhood iterators
   * of arbitrary type and must use virtual functions.
   */
  virtual void GoToBegin();
  
  /**
   * Virtual method for sending the iterator to one past the last pixel in its
   * region.
   */
  virtual void GoToEnd();
  
  /**
   * Initializes the iterator to walk a particular image and a particular
   * region of that image.
   */
  virtual void Initialize(const SizeType &radius, const ImageType *ptr,
                          const RegionType &region);

  /**
   * Virtual method for determining whether the the iterator is at the
   * beginning of its iteration region.
   */
  virtual bool IsAtBegin() const
    {    return ( this->GetCenterPointer() == m_Begin );   }
  
  /**
   * Virtual method for determining whether the the iterator has reached the
   * end of its iteration region.
   */
  virtual bool IsAtEnd() const
    {
      if ( this->GetCenterPointer() > m_End )
        {std::cout << "PAST END!!!!!" <<  std::endl;
        throw ExceptionObject(); }
      return ( this->GetCenterPointer() == m_End );   }
  
  /**
   * Increments the pointers in the ConstNeighborhoodIterator,
   * wraps across boundaries automatically, accounting for
   * the disparity in the buffer size and the region size of the
   * image.
   */
  virtual const Self &operator++();
  
  /**
   * Decrements the pointers in the ConstNeighborhoodIterator,
   * wraps across boundaries automatically, accounting for
   * the disparity in the buffer size and the region size of the
   * image.
   */
  virtual const Self &operator--();  
 
  /**
   * Returns a boolean == comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator==(const Self &it) const 
    {   return  it.GetCenterPointer() == this->GetCenterPointer();   }
  
  /**
   * Returns a boolean != comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator!=(const Self &it) const
    {    return  it.GetCenterPointer != this->GetCenterPointer();  }
  
  /**
   * Returns a boolean < comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator<(const Self &it) const
    {  return  this->GetCenterPointer() < it.GetCenterPointer();  }

  /**
   * Returns a boolean < comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator<=(const Self &it) const
    {    return  this->GetCenterPointer() <= it.GetCenterPointer;  }
  
  /**
   * Returns a boolean > comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator>(const Self &it) const
    {    return  this->GetCenterPointer() > it.GetCenterPointer();  }

  /**
   * Returns a boolean >= comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored.
   */
  bool operator>=(const Self &it) const
    {    return  this->GetCenterPointer() >= it.GetCenterPointer();  }

  /**
   * Allows a user to override the internal boundary condition. Care should be
   * be taken to ensure that the overriding boundary condition is a persistent
   * object during the time it is referenced.  The overriding condition
   * can be of a different type than the default type as long as it is
   * a subclass of ImageBoundaryCondition.
   *
   * This method is only relevant in iterators that have the capability
   * to handle boundary conditions.
   */
  virtual void OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i)
    { /* default case is do nothing */ }
 
  /**
   * This method positions the iterator at an indexed location in the
   * image. SetLocation should _NOT_ be used to update the position of the
   * iterator during iteration, only for initializing it to a position
   * prior to iteration.  This method is not optimized for speed.
   */
  void SetLocation( const IndexType& position )
    {
      this->SetLoop(position);
      this->SetPixelPointers(position);
    }
  
  /**
   * Sets the internal pointer to a memory buffer that is incremented
   * in synch with the center of the ConstNeighborhoodIterator.  This
   * internal pointer can be used to guarantees spatial fidelity between
   * inputs and outputs to an algorithm that uses ConstNeighborhoodIterators.
   * \sa GetOutputBuffer
   */
  void SetOutputBuffer(InternalPixelType *i)
    {    m_OutputBuffer = i;   }

  /**
   * Sets the offsets that will be used to adjust for any differences in input
   * and output buffer sizes.
   */
  void SetOutputWrapOffsetModifier( const OffsetType &modifiers);

  /**
   * Resets the boundary condition to the internal, default conditions
   * specified by the template parameter.
   */
  virtual void ResetBoundaryCondition()
    { /* default case is do nothing */ }
 
protected:
  /**
   * Computes the internal table of stride lengths.
   */
  void ComputeStrideTable()
    {
      for (unsigned int i = 0; i < Dimension; ++i)
        { m_StrideTable[i] = this->GetStride(i); }
    }
  
  /**
   * Default method for setting the coordinate location of the iterator.
   * Loop indicies correspond to the actual Image region index.
   */
  virtual void SetLoop( const IndexType& p )
    {  m_Loop = p; }
  
  /**
   * Virtual method for setting internal loop boundaries.  This
   * method must be defined in each subclass because
   * each subclass may handle loop boundaries differently.
   */
  virtual void SetBound(const SizeType &);

  /**
   * Default method for setting the values of the internal pointers
   * to itk::Image memory buffer locations.  This method should
   * generally only be called when the iterator is initialized.
   * \sa SetLocation
   */
  virtual void SetPixelPointers(const IndexType &);

  /**
   * Default method for setting the index of the first pixel in the
   * iteration region.
   */
  virtual void SetBeginIndex( const IndexType& start)
    {  m_BeginIndex = start;  }

  /**
   * Default method for setting the index of the first pixel in the
   * iteration region.
   */
  virtual void SetEndIndex();
  
  /**
   * The starting index for iteration within the itk::Image region
   * on which this ConstNeighborhoodIterator is defined.
   */
  IndexType m_BeginIndex;

  /**
   * An array of upper looping boundaries used during iteration.
   */
  IndexType m_Bound;

  /**
   * A pointer to the first pixel in the iteration region.
   */
  const InternalPixelType *m_Begin;
  
  /**
   * The image on which iteration is defined.
   */
  typename ImageType::ConstPointer m_ConstImage;

  /*
   * A pointer to one past the last pixel in the iteration region.
   */
  const InternalPixelType *m_End;

  /**
   * The end index for iteration within the itk::Image region
   * on which this ConstNeighborhoodIterator is defined.
   */
  IndexType m_EndIndex;

  /**
   *
   */
  mutable InternalPixelType *m_ImageBuffer;

  
  /**
   * Array of loop counters used during iteration.
   */
  IndexType m_Loop;
 
  /**
   * A pointer to an output buffer that, if set, is moved in synch with
   * the center position of the ConstNeighborhoodIterator.
   *
   * The output buffer is assumed to be the same size as the itk::Image buffer
   * on which the ConstNeighborhoodIterator is defined.  Scalar output can be
   * written directly to the synchronized pointer location. 
   */
  InternalPixelType *m_OutputBuffer;
  
  /**
   * Modifiers that compensate for the difference in the sizes of the input
   * and output buffers.  m_OutputBuffer will be incremented by these
   * additional values when wrapping across dimensional boundaries during
   * iteration.
   */
  OffsetType m_OutputWrapOffsetModifier;
  
  /**
   * The region over which iteration is defined.
   */
  RegionType m_Region;

  /**
   * A table containing the stride length (step size in memory units)
   * for each dimension.  Computed once at construction.
   */
  unsigned long m_StrideTable[Dimension];

  /**
   * The internal array of offsets that provide support for regions of interest.
   * An offset for each dimension is necessary to shift pointers when wrapping
   * around region edges because region memory is not necessarily contiguous
   * within the buffer.
   */
  OffsetType m_WrapOffset;
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstNeighborhoodIterator.txx"
#endif

#endif
