/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBufferIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkImageBufferIterator_h
#define __itkImageBufferIterator_h

#include "itkImageIterator.h"

namespace itk
{

/**
 * \class ImageBufferIterator
 * \brief Multi-dimensional image iterator.
 * 
 * ImageBufferIterator is a templated class to represent a multi-dimensional
 * iterator. ImageBufferIterator is templated over the dimension of the image
 * and the data type of the image.
 *
 * ImageBufferIterator performs no bounds checking to make sure that iterator
 * is kept within the bounds of the region or the buffer.  This iterator
 * provides the more powerful and most dangerous level of access to an image.
 *
 * ImageBufferIterator is a multi-dimensional iterator, requiring more
 * information be specified before the iterator can be used than conventional
 * iterators. Whereas the std::vector::iterator from the STL only needs to be
 * passed a pointer to establish the iterator, the multi-dimensional image
 * iterator needs a pointer, the size of the buffer, the size of the region,
 * the start index of the buffer, and the start index of the region. To gain
 * access to this information, ImageBufferIterator holds a reference to the
 * image over which it is traversing.
 *
 * ImageBufferIterator assumes a particular layout of the image data. The
 * data is arranged in a 1D array as if it were [][][][slice][row][col] with
 * Index[0] = col, Index[1] = row, Index[2] = slice, etc.
 *
 */
template<typename TImage>
class ImageBufferIterator : public ImageIterator<TImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageBufferIterator Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageIterator<TImage>  Superclass;

  /**
   * Dimension of the image the iterator walks.  This enum is needed so that
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks.
   */
  enum { ImageIteratorDimension = Superclass::ImageIteratorDimension };

  /** 
   * Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename Superclass::IndexType IndexType;

  /** 
   * Size typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename Superclass::SizeType SizeType;

  /** 
   * Region typedef support.
   */
  typedef typename Superclass::RegionType   RegionType;

  /**
   * Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename Superclass::ImageType ImageType;

  /** 
   * PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename Superclass::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /**
   * Internal Pixel Type
   */
  typedef typename Superclass::InternalPixelType   InternalPixelType;

  /**
   * External Pixel Type
   */
  typedef typename Superclass::PixelType   PixelType;

  /** 
   *  Accessor type that convert data between internal and external
   *  representations.
   */
  typedef typename Superclass::AccessorType     AccessorType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageBufferIterator, ImageIterator);

  /**
   * Default constructor. Needed since we provide a cast constructor.
   */
  ImageBufferIterator() : ImageIterator<TImage>() {}

  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  ImageBufferIterator(ImageType *ptr,
                      const RegionType& region)
    : ImageIterator<TImage>(ptr, region) {}
  
  /**
   * Constructor that can be used to cast from an ImageIterator to an
   * ImageBufferIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageBufferIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * an ImageIterator to a ImageBufferIterator.
   */
  ImageBufferIterator( const ImageIterator<TImage> &it)
    { this->ImageIterator<TImage>::operator=(it); }

  /**
   * Increment an iterator by an index. This models a random access
   * multidimensional iterator. No bounds checking is performed. The iterator
   * is not constrained to the region.
   */
  Self &
  Increment(const IndexType &vec)
  {
    const unsigned long *offsetTable = m_Image->GetOffsetTable();
    
    for (unsigned int i=0; i < ImageIteratorDimension; i++)
      {
	m_Offset += (vec[i] * offsetTable[i]);
      }
    return *this;
  }
  
  /**
   * Increment an iterator by an integer along a particular dimension.
   * No bounds checking is performed. The iterator is not constrained to the
   * region.
   */
  Self &
  Increment(unsigned int dim, long delta)
  {
    const unsigned long *offsetTable = m_Image->GetOffsetTable();

    m_Offset += (delta * offsetTable[dim]);
    return *this;
  }

  /**
   * Increment an iterator by an integer along the fastest moving dimension.
   * No bounds checking is performed. The iterator is not constrained to the
   * region.
   */
  Self &
  Increment(long delta)
  {
    m_Offset += delta;
    return *this;
  }

  /**
   * Decrement an iterator by an index. This models a random access
   * multidimensional iterator. No bounds checking is performed. The iterator
   * is not constrained to the region.
   */
  Self &
  Decrement(const IndexType &vec)
  {
    const unsigned long *offsetTable = m_Image->GetOffsetTable();
    
    for (unsigned int i=0; i < ImageIteratorDimension; i++)
      {
	m_Offset -= (vec[i] * offsetTable[i]);
      }
    return *this;
  }
  
  /**
   * Decrement an iterator by an integer along a particular dimension.
   * No bounds checking is performed. The iterator is not constrained to the
   * region.
   */
  Self &
  Decrement(unsigned int dim, long delta)
  {
    const unsigned long *offsetTable = m_Image->GetOffsetTable();

    m_Offset -= (delta * offsetTable[dim]);
    return *this;
  }

  /**
   * Decrement an iterator by an integer along the fastest moving dimension.
   * No bounds checking is performed. The iterator is not constrained to the
   * region.
   */
  Self &
  Decrement(long delta)
  {
    m_Offset -= delta;
    return *this;
  }

  /**
   * Add an index to this iterator and return a new iterator. No bounds
   * checking is performed. The new iterator is not constrained to the region.
   */
  Self
  Add(const IndexType &vec) const
  {
    Self result( *this ); // copy the original ivars

    const unsigned long *offsetTable = m_Image->GetOffsetTable();
    
    for (unsigned int i=0; i < ImageIteratorDimension; i++)
      {
      result.m_Offset += (vec[i] * offsetTable[i]);
      }

    return result;
  }

  /**
   * Return a new iterator which is offset from this one along a specified
   * dimension. No bounds checking is performed. The iterator is not
   * constrained to the region.
   */
  Self 
  Add(unsigned int dim, long delta) const
  {
    Self result( *this ); // copy the original ivars
    
    const unsigned long *offsetTable = m_Image->GetOffsetTable();

    result.m_Offset += (delta * offsetTable[dim]);
    return result;
  }

  /**
   * Return a new iterator which is offset from this one along the fastest
   * moving dimension. No bounds checking is performed. The iterator is not
   * constrained to the region.
   */
  Self 
  Add(long delta) const
  {
    Self result( *this ); // copy the original ivars
    
    const unsigned long *offsetTable = m_Image->GetOffsetTable();

    result.m_Offset += (delta * offsetTable[dim]);
    return result;
  }


  /**
   * Subtract an index to this iterator and return a new iterator. No bounds
   * checking is performed. The new iterator is not constrained to the region.
   */
  Self
  Subtract(const IndexType &vec) const
  {
    Self result( *this ); // copy the original ivars

    const unsigned long *offsetTable = m_Image->GetOffsetTable();
    
    for (unsigned int i=0; i < ImageIteratorDimension; i++)
      {
      result.m_Offset -= (vec[i] * offsetTable[i]);
      }

    return result;
  }
  
  /**
   * Return a new iterator which is offset from this one along a specified
   * dimension. No bounds checking is performed. The iterator is not
   * constrained to the region.
   */
  Self 
  Subtract(unsigned int dim, long delta) const
  {
    Self result( *this ); // copy the original ivars
    
    const unsigned long *offsetTable = m_Image->GetOffsetTable();

    result.m_Offset -= (delta * offsetTable[dim]);
    return result;
  }

  /**
   * Return a new iterator which is offset from this one along the fastest
   * moving dimension. No bounds checking is performed. The iterator is not
   * constrained to the region.
   */
  Self 
  Subtract(long delta) const
  {
    Self result( *this ); // copy the original ivars
    
    const unsigned long *offsetTable = m_Image->GetOffsetTable();

    result.m_Offset -= (delta * offsetTable[dim]);
    return result;
  }

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageBufferIterator.txx"
#endif

#endif 
