/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageLinearConstIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageLinearConstIteratorWithIndex_h
#define __itkImageLinearConstIteratorWithIndex_h

#include "itkImageConstIteratorWithIndex.h"

namespace itk
{

/** \class ImageLinearConstIteratorWithIndex
 * \brief Multi-dimensional image iterator which only walks a region.
 * 
 * ImageLinearConstIteratorWithIndex is a templated class to represent a multi-dimensional
 * iterator. ImageLinearConstIteratorWithIndex is templated over the image type
 * ImageLinearConstIteratorWithIndex is constrained to walk only within the 
 * specified region.
 *
 * ImageLinearConstIteratorWithIndex is a multi-dimensional iterator, requiring more
 * information be specified before the iterator can be used than conventional
 * iterators. Whereas the std::vector::iterator from the STL only needs to be
 * passed a pointer to establish the iterator, the multi-dimensional image
 * iterator needs a pointer, the size of the buffer, the size of the region,
 * the start index of the buffer, and the start index of the region. To gain
 * access to this information, ImageLinearConstIteratorWithIndex holds a reference to the
 * image over which it is traversing.
 *
 * ImageLinearConstIteratorWithIndex assumes a particular layout of the image data. The
 * is arranged in a 1D array as if it were [][][][slice][row][col] with
 * Index[0] = col, Index[1] = row, Index[2] = slice, etc.
 *
 * operator++ provides a simple syntax for walking around a region of
 * a multidimensional image. operator++ iterates across a preselected direction 
 * constraining the movement to within a region of image. The user can verify
 * when the iterator reaches the boundary of the region along this direction, 
 * by calling the IsAtEndOfLine() method. Then it is possible to pass to the
 * next line starting at the first pixel in the row that is part of the region 
 * by calling the NextLine() method.
 *
 * This is the typical use of this iterator in a loop:
 *
 * \code
 *  
 * ImageLinearConstIteratorWithIndex<ImageType> it( image, image->GetRequestedRegion() );
 * 
 * it.SetDirection(2);
 * it.GoToBegin();
 * while( !it.IsAtEnd() )
 * {
 *   while( !it.IsAtEndOfLine() )
 *   {
 *      value = it.Get();  // it.Set() doesn't exist in the Const Iterator
 *      ++it;
 *   }
 *   it.NextLine();
 *  } 
 *
 *  \endcode
 *
 * \example  Common/itkImageLinearIteratorTest.cxx
 *
 * \ingroup ImageIterators
 */
template<typename TImage>
class ITK_EXPORT ImageLinearConstIteratorWithIndex : public ImageConstIteratorWithIndex<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageLinearConstIteratorWithIndex Self;
  typedef ImageConstIteratorWithIndex<TImage>  Superclass;
  
  /** Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index. */
  typedef typename TImage::IndexType   IndexType;

  /** Region typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Region back to itk::ImageRegion so that is
   * it not confused with ImageIterator::Index. */
  typedef typename TImage::RegionType RegionType;
  
  /** Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index. */
  typedef TImage ImageType;

  /** PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename TImage::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;
  
  /** Default constructor. Needed since we provide a cast constructor. */
  ImageLinearConstIteratorWithIndex() : ImageConstIteratorWithIndex<TImage>() {}
  
  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageLinearConstIteratorWithIndex(const ImageType *ptr,
                      const RegionType& region)
    : ImageConstIteratorWithIndex<TImage>( ptr, region ) {}

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageLinearConstIteratorWithIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageLinearConstIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageLinearConstIteratorWithIndex. */
  ImageLinearConstIteratorWithIndex( const ImageConstIteratorWithIndex<TImage> &it)
    { this->ImageConstIteratorWithIndex<TImage>::operator=(it); }

  /** Go to the next line.
   * \sa operator++  \sa operator-- \sa EndOfLine \sa PreviousLine \sa End */
  inline void NextLine(void);

  /** Go to the previous line.
   * \sa operator++ \sa operator-- \sa EndOfLine \sa NextLine \sa End */
  inline void PreviousLine(void);

  /** Go to the beginning pixel of the current line.
   * \sa GoToReverseBeginOfLine \sa operator++ \sa operator-- \sa NextLine \sa EndOfLine */
  void GoToBeginOfLine(void);

  /** Go to the past end pixel of the current line.
   * \sa GoToBeginOfLine \sa operator++ \sa operator-- \sa NextLine \sa EndOfLine */
  void GoToEndOfLine(void);

  /** Test if the index is at the end of line */
  inline bool IsAtEndOfLine(void);

  /** Test if the index is at the begin of line */
  inline bool IsAtReverseEndOfLine(void);

  /** Set the direction of movement */
  inline void SetDirection(unsigned int direction) ;

  /** Increment (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator-- */
  Self & operator++();

  /** Decrement (prefix) the selected dimension.
   * No bounds checking is performed.  \sa GetIndex \sa operator++ */
  Self & operator--();

private:
    unsigned long  m_Jump;
    unsigned int   m_Direction;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageLinearConstIteratorWithIndex.txx"
#endif

#endif 
