/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageConstIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageConstIteratorWithIndex_h
#define __itkImageConstIteratorWithIndex_h

#include "itkIndex.h"
#include "itkImage.h"
#include <memory>

namespace itk
{

/** \class ImageConstIteratorWithIndex
 * \brief Multi-dimensional image iterator.
 * 
 * ImageConstIteratorWithIndex is a templated class to represent a
 * multi-dimensional iterator. ImageConstIteratorWithIndex is templated
 * over the dimension of the image and the data type of the image.
 *
 * ImageConstIteratorWithIndex is a base class for image iterators. It
 * provides the basic construction and comparison operations.
 * However, it does not provide mechanisms for moving the iterator.  A
 * subclass of ImageConstIteratorWithIndex must be used to move the
 * iterator.
 * 
 * ImageConstIteratorWithIndex is a multi-dimensional iterator, requiring
 * more information be specified before the iterator can be used than
 * conventional iterators.  Whereas the std::vector::iterator from the
 * STL only needs to be passed a pointer to establish the iterator,
 * the multi-dimensional image iterator needs a pointer, the size of
 * the buffer, the size of the region, the start index of the buffer,
 * and the start index of the region. To gain access to this
 * information, ImageConstIteratorWithIndex holds a reference to the image
 * over which it is traversing.
 *
 * ImageConstIteratorWithIndex assumes a particular layout of the image
 * data. In particular, the data is arranged in a 1D array as if it
 * were [][][][slice][row][col] with Index[0] = col, Index[1] = row,
 * Index[2] = slice, etc.
 *
 * \ingroup ImageIterators
 */
template<typename TImage>
class ImageConstIteratorWithIndex {
public:
  /** Standard class typedefs. */
  typedef ImageConstIteratorWithIndex Self;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Index typedef support. */
  typedef typename TImage::IndexType          IndexType;
  typedef typename IndexType::IndexValueType  IndexValueType;

  /** Size typedef support. */
  typedef typename TImage::SizeType           SizeType;
  typedef typename SizeType::SizeValueType    SizeValueType;

  /** Region typedef support. */
  typedef typename TImage::RegionType         RegionType;

  /** Image typedef support. */
  typedef TImage   ImageType;

  /** PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename TImage::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;
  
  /** Internal Pixel Type */
  typedef typename TImage::InternalPixelType   InternalPixelType;

  /** External Pixel Type */
  typedef typename TImage::PixelType   PixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  typedef typename TImage::AccessorType     AccessorType;

  /** Type of the Offset taken from the image */
  typedef typename TImage::OffsetType           OffsetType;
  typedef typename OffsetType::OffsetValueType  OffsetValueType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageConstIteratorWithIndex();

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageConstIteratorWithIndex(const Self& it);

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageConstIteratorWithIndex( const TImage *ptr,
                               const RegionType& region );

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &operator=(const Self& it);
  
  /** Get the dimension (size) of the index. */
  static unsigned int GetImageDimension() 
    {return ImageDimension;}

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator!=(const Self &it) const
    {
    // two iterators are the same if they "point to" the same memory location
    return (m_Position) != (it.m_Position);
    };

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator==(const Self &it) const
    {
    // two iterators are the same if they "point to" the same memory location
    return (m_Position) == (it.m_Position);
    };
  
  /** Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location. */
  bool
  operator<=(const Self &it) const
    {
    // an iterator is "less than" another if it "points to" a lower
    // memory location
    return (m_Position) <= (it.m_Position);
    };

  /** Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location. */
  bool
  operator<(const Self &it) const
    {
    // an iterator is "less than" another if it "points to" a lower
    // memory location
    return (m_Position) < (it.m_Position);
    };

  /** Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location. */
  bool
  operator>=(const Self &it) const
    {
    // an iterator is "greater than" another if it "points to" a higher
    // memory location
    return (m_Position) >= (it.m_Position);
    };

  /** Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location. */
  bool
  operator>(const Self &it) const
    {
    // an iterator is "greater than" another if it "points to" a higher
    // memory location
    return (m_Position) > (it.m_Position);
    };

  /** Get the index. This provides a read only reference to the index.
   * \sa SetIndex */
  const IndexType GetIndex()
    { return m_PositionIndex; }

  /** Get the region that this iterator walks. ImageIterators know the
   * beginning and the end of the region of the image to iterate over. */
  const RegionType& GetRegion() const
    { return m_Region; };

  /** Set the index. No bounds checking is performed.
   * \sa GetIndex */
  void SetIndex(const IndexType &ind)
    { m_Position = m_Begin + m_Image->ComputeOffset( ind ); 
      m_PositionIndex = ind;  }

  /** Get the pixel value */
  PixelType Get(void) const  
    { return m_PixelAccessor.Get(*m_Position); }
  
  /** Return a const reference to the pixel 
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  const PixelType & Value(void) const  
    { return *m_Position; }
 
  /** Move an iterator to the beginning of the region.
   * \deprecated Use GoToBegin() instead */
  Self Begin(void) const;

  /** Move an iterator to the beginning of the region. */
  void GoToBegin(void);

  /** Move an iterator to the End of the region.
   * \deprecated Use GoToEnd() instead */
  Self End(void) const;

  /** Move an iterator to the End of the region. */
  void GoToEnd(void);

  /** Is the iterator at the beginning of the region? */
  bool IsAtBegin(void) const
    {
      return !m_Remaining;
    }

  /** Is the iterator at the end of the region? */
  bool IsAtEnd(void) const
    {
      return !m_Remaining;
    }
  
 /** Are there data remainning in the region ? */
  bool Remaining()
    {
    return m_Remaining;
    }
  
protected: //made protected so other iterators can access 
  typename TImage::ConstPointer     m_Image;
  
  IndexType    m_PositionIndex;     // Index where we currently are
  IndexType    m_BeginIndex;        // Index to start iterating over
  IndexType    m_EndIndex;          // Index to finish iterating:
                                    // one pixel past the end of each
                                    // row, col, slice, etc....

  RegionType   m_Region;            // region to iterate over

  unsigned long          m_OffsetTable[ ImageDimension + 1 ]; 
  
  const InternalPixelType     *m_Position;
  const InternalPixelType     *m_Begin;
  const InternalPixelType     *m_End;

  bool                         m_Remaining;

  AccessorType           m_PixelAccessor;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageConstIteratorWithIndex.txx"
#endif

#endif 
