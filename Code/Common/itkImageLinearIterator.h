/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageLinearIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkImageLinearIterator_h
#define __itkImageLinearIterator_h

#include "itkImageIteratorWithIndex.h"

namespace itk
{

/**
 * \class ImageLinearIterator
 * \brief Multi-dimensional image iterator which only walks a region.
 * 
 * ImageLinearIterator is a templated class to represent a multi-dimensional
 * iterator. ImageLinearIterator is templated over the image type
 * ImageLinearIterator is constrained to walk only within the 
 * specified region.
 *
 * ImageLinearIterator is a multi-dimensional iterator, requiring more
 * information be specified before the iterator can be used than conventional
 * iterators. Whereas the std::vector::iterator from the STL only needs to be
 * passed a pointer to establish the iterator, the multi-dimensional image
 * iterator needs a pointer, the size of the buffer, the size of the region,
 * the start index of the buffer, and the start index of the region. To gain
 * access to this information, ImageLinearIterator holds a reference to the
 * image over which it is traversing.
 *
 * ImageLinearIterator assumes a particular layout of the image data. The
 * is arranged in a 1D array as if it were [][][][slice][row][col] with
 * Index[0] = col, Index[1] = row, Index[2] = slice, etc.
 *
 * operator++ provides a simple syntax for walking around a region of
 * a multidimensional image. operator++ iterates across a row, constraining
 * the movement to within a region of image. When the iterator reaches
 * the boundary of the region along a row, the iterator automatically
 * wraps to the next row, starting at the first pixel in the row that is
 * part of the region. This allows for simple processing loops of the form:
 *
 *      for (it = image->RegionBegin(); it != image->RegionEnd(); ++it)
 *         {
 *         *it += 100.0;
 *         }
 *
 */
template<typename TImage>
class ImageLinearIterator : public ImageIteratorWithIndex<TImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageLinearIterator Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageIteratorWithIndex<TImage>  Superclass;

  /** 
   * Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index.
   */
  typedef typename TImage::IndexType   IndexType;

  /**
   * Region typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Region back to itk::ImageRegion so that is
   * it not confused with ImageIterator::Index.
   */
  typedef typename TImage::RegionType RegionType;
  
  /**
   * Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index.
   */
  typedef TImage ImageType;

  /** 
   * PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename TImage::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /**
   * Default constructor. Needed since we provide a cast constructor.
   */
  ImageLinearIterator() : ImageIteratorWithIndex<TImage>() {}
  
  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  ImageLinearIterator(ImageType *ptr,
                      const RegionType& region)
    : ImageIteratorWithIndex<TImage>( ptr, region ) {}

  /**
   * Constructor that can be used to cast from an ImageIterator to an
   * ImageLinearIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageLinearIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageLinearIterator.
   */
  ImageLinearIterator( const ImageIteratorWithIndex<TImage> &it)
    { this->ImageIteratorWithIndex<TImage>::operator=(it); }

  /**
   * Go to the next line
   * \sa operator++
   * \sa EndOfLine
   * \sa End
   */
  inline void NextLine(void);

  /**
   * Test if the index is at the end of line
   */
  inline bool IsAtEndOfLine(void);


  /**
   * Set the direction of movement
   */
  inline void SetDirection(unsigned int direction) ;


  /**
   * Increment (prefix) the selected dimension.
   * No bounds checking is performed. 
   * \sa GetIndex
   */
  Self & operator++();


private:
    unsigned long  m_Jump;
    unsigned int   m_Direction;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageLinearIterator.txx"
#endif

#endif 
