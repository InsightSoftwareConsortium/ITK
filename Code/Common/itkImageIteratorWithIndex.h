/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkImageIteratorWithIndex_h
#define __itkImageIteratorWithIndex_h

#include "itkIndex.h"
#include "itkImage.h"
#include <memory.h>

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
  typedef typename TImage::Index  Index;

  /** 
   * Size typedef support.
   */
  typedef typename TImage::Size    Size;

  /** 
   * Region typedef support.
   */
  typedef typename TImage::Region   Region;

  /**
   * Image typedef support.
   */
  typedef TImage   Image;

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
                         const Region& region);

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
  const Index GetIndex()
    { return m_PositionIndex; }

  /**
   * Get the region that this iterator walks. ImageIterators know the
   * beginning and the end of the region of the image to iterate over.
   */
  const Region& GetRegion() const
  { return m_Region; };

  /**
   * Set the index. No bounds checking is performed.
   * \sa GetIndex
   */
  void SetIndex(const Index &ind)
    { m_Position = m_Begin + m_Image->ComputeOffset( ind ); 
      m_PositionIndex = ind;  }

  /**
   * Get the pixel value
   */
  PixelType Get(void) const  
    { return AccessorType::Get(*m_Position); }
  
  /**
   * Set the pixel value
   */
  void Set( const PixelType & value) const  
    { AccessorType::Set(*m_Position,value); }

  /**
   * Move an iterator to the beginning of the region.
   */
  void Begin();

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
  
  Index          m_PositionIndex;             // Index where we currently are
  Index          m_BeginIndex;                // Index to start iterating over
  Index          m_EndIndex;                  // Index to finish iterating:
                                              // one pixel past the end of each
                                              // row, col, slice, etc....
                               

  Region         m_Region;                    // region to iterate over


  unsigned long          m_OffsetTable[ImageIteratorDimension+1]; 
  
  InternalPixelType     *m_Position;
  InternalPixelType     *m_Begin;
  InternalPixelType     *m_End;

  bool                   m_Remaining;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageIteratorWithIndex.txx"
#endif

#endif 
