/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIteratorWithIndex.h
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
#ifndef __itkImageIteratorWithIndex_h
#define __itkImageIteratorWithIndex_h

#include "itkIndex.h"
#include "itkImage.h"
#include <memory>

namespace itk
{

/**
 * \class ImageIteratorWithIndex
 * \brief Multi-dimensional image iterator.
 * 
 * ImageIteratorWithIndex is a templated class to represent a
 * multi-dimensional iterator. ImageIteratorWithIndex is templated
 * over the dimension of the image and the data type of the image.
 *
 * ImageIteratorWithIndex is a base class for image iterators. It
 * provides the basic construction and comparison operations.
 * However, it does not provide mechanisms for moving the iterator.  A
 * subclass of ImageIteratorWithIndex must be used to move the
 * iterator.
 * 
 * ImageIteratorWithIndex is a multi-dimensional iterator, requiring
 * more information be specified before the iterator can be used than
 * conventional iterators.  Whereas the std::vector::iterator from the
 * STL only needs to be passed a pointer to establish the iterator,
 * the multi-dimensional image iterator needs a pointer, the size of
 * the buffer, the size of the region, the start index of the buffer,
 * and the start index of the region. To gain access to this
 * information, ImageIteratorWithIndex holds a reference to the image
 * over which it is traversing.
 *
 * ImageIteratorWithIndex assumes a particular layout of the image
 * data. In particular, the data is arranged in a 1D array as if it
 * were [][][][slice][row][col] with Index[0] = col, Index[1] = row,
 * Index[2] = slice, etc.
 *
 *  */
template<typename TImage>
class ImageIteratorWithIndex {
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageIteratorWithIndex Self;

  /**
   * Dimension of the image the iterator walks.  This enum is needed so that
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks.
   */
  enum { ImageIteratorDimension = TImage::ImageDimension };

  /** 
   * Index typedef support.
   */
  typedef typename TImage::IndexType  IndexType;

  /** 
   * Size typedef support.
   */
  typedef typename TImage::SizeType    SizeType;

  /** 
   * Region typedef support.
   */
  typedef typename TImage::RegionType   RegionType;

  /**
   * Image typedef support.
   */
  typedef TImage   ImageType;

  /** 
   * PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename TImage::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /**
   * Internal Pixel Type
   */
  typedef typename TImage::InternalPixelType   InternalPixelType;

  /**
   * External Pixel Type
   */
  typedef typename TImage::PixelType   PixelType;

  /** 
   *  Accessor type that convert data between internal and external
   *  representations.
   */
  typedef typename TImage::AccessorType     AccessorType;

  /**
   * Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor.
   */
  ImageIteratorWithIndex();

  /**
   * Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted.
   */
  ImageIteratorWithIndex(const Self& it);

  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  ImageIteratorWithIndex(TImage *ptr,
                         const RegionType& region);

  /**
   * operator= is provided to make sure the handle to the image is properly
   * reference counted.
   */
  Self &operator=(const Self& it);
  
  /**
   * Get the dimension (size) of the index.
   */
  static unsigned int GetImageIteratorDimension() 
    {return ImageIteratorDimension;}

  /**
   * Comparison operator. Two iterators are the same if they "point to" the
   * same memory location
   */
  bool
  operator!=(const Self &it) const
    {
    // two iterators are the same if they "point to" the same memory location
    return (m_Position) != (it.m_Position);
    };

  /**
   * Comparison operator. Two iterators are the same if they "point to" the
   * same memory location
   */
  bool
  operator==(const Self &it) const
    {
    // two iterators are the same if they "point to" the same memory location
    return (m_Position) == (it.m_Position);
    };
  
  /**
   * Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location.
   */
  bool
  operator<=(const Self &it) const
    {
    // an iterator is "less than" another if it "points to" a lower
    // memory location
    return (m_Position) <= (it.m_Position);
    };

  /**
   * Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location.
   */
  bool
  operator<(const Self &it) const
    {
    // an iterator is "less than" another if it "points to" a lower
    // memory location
    return (m_Position) < (it.m_Position);
    };

  /**
   * Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location.
   */
  bool
  operator>=(const Self &it) const
    {
    // an iterator is "greater than" another if it "points to" a higher
    // memory location
    return (m_Position) >= (it.m_Position);
    };

  /**
   * Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location.
   */
  bool
  operator>(const Self &it) const
    {
    // an iterator is "greater than" another if it "points to" a higher
    // memory location
    return (m_Position) > (it.m_Position);
    };

  /**
   * Get the index. This provides a read only reference to the index.
   * \sa SetIndex
   */
  const IndexType GetIndex()
    { return m_PositionIndex; }

  /**
   * Get the region that this iterator walks. ImageIterators know the
   * beginning and the end of the region of the image to iterate over.
   */
  const RegionType& GetRegion() const
  { return m_Region; };

  /**
   * Set the index. No bounds checking is performed.
   * \sa GetIndex
   */
  void SetIndex(const IndexType &ind)
    { m_Position = m_Begin + m_Image->ComputeOffset( ind ); 
      m_PositionIndex = ind;  }

  /**
   * Get the pixel value
   */
  PixelType Get(void) const  
    { return m_DataAccessor.Get(*m_Position); }
  
  /**
   * Set the pixel value
   */
  void Set( const PixelType & value) const  
    { m_DataAccessor.Set(*m_Position,value); }

  /**
   * Return a const reference to the pixel 
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors.
   */
  const PixelType & Value(void) const  
    { return *m_Position; }
 
  /**
   * Return a reference to the pixel 
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors.
   */
  PixelType & Value(void) 
    { return *m_Position; }
 
  /**
   * Move an iterator to the beginning of the region.
   * \deprecated Use GoToBegin() instead
   */
  void Begin();


  /**
   * Move an iterator to the beginning of the region.
   */
  void GoToBegin();



  /**
   * Move an iterator to the End of the region.
   * \deprecated Use GoToEnd() instead
   */
  void End();


  /**
   * Move an iterator to the End of the region.
   */
  void GoToEnd();


  /**
   * Is the iterator at the beginning of the region?
   */
  bool IsAtBegin()
    {
    return (m_Position == m_Begin);
    }

  /**
   * Is the iterator at the end of the region?
   */
  bool IsAtEnd()
    {
      return !m_Remaining;
    }
  
   /**
   * Are there data remainning in the region ?
   */
  bool Remaining()
    {
    return m_Remaining;
    }
  
protected: //made protected so other iterators can access 
  SmartPointer<TImage> m_Image;
  
  IndexType    m_PositionIndex;     // Index where we currently are
  IndexType    m_BeginIndex;        // Index to start iterating over
  IndexType    m_EndIndex;          // Index to finish iterating:
                                    // one pixel past the end of each
                                    // row, col, slice, etc....

  RegionType   m_Region;            // region to iterate over

  unsigned long          m_OffsetTable[ImageIteratorDimension+1]; 
  
  InternalPixelType     *m_Position;
  InternalPixelType     *m_Begin;
  InternalPixelType     *m_End;

  bool                   m_Remaining;

  AccessorType           m_DataAccessor;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageIteratorWithIndex.txx"
#endif

#endif 
