/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageLinearIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageLinearIteratorWithIndex_h
#define __itkImageLinearIteratorWithIndex_h

#include "itkImageIteratorWithIndex.h"

namespace itk
{

/** \class ImageLinearIteratorWithIndex
 * \brief Multi-dimensional image iterator which only walks a region.
 * 
 * ImageLinearIteratorWithIndex is a templated class to represent a multi-dimensional
 * iterator. ImageLinearIteratorWithIndex is templated over the image type
 * ImageLinearIteratorWithIndex is constrained to walk only within the 
 * specified region and along a line parallel to one of the coordinate axis.
 *
 * ImageLinearIteratorWithIndex is a multi-dimensional iterator, requiring more
 * information be specified before the iterator can be used than conventional
 * iterators. Whereas the std::vector::iterator from the STL only needs to be
 * passed a pointer to establish the iterator, the multi-dimensional image
 * iterator needs a pointer, the size of the buffer, the size of the region,
 * the start index of the buffer, and the start index of the region. To gain
 * access to this information, ImageLinearIteratorWithIndex holds a reference to the
 * image over which it is traversing.
 *
 * ImageLinearIteratorWithIndex assumes a particular layout of the image data. The
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
 * ImageLinearIteratorWithIndex<ImageType> it( image, image->GetRequestedRegion() );
 * 
 * it.SetDirection(2);
 * it.GoToBegin();
 * while( !it.IsAtEnd() )
 * {
 *   while( !it.IsAtEndOfLine() )
 *   {
 *      it.Set( 100.0 );
 *      ++it;
 *   }
 *   it.NextLine();
 *  } 
 *
 *  \endcode
 *
 * \example  Common/itkImageLinearIteratorWithIndexTest.cxx
 *
 *
 * \ingroup ImageIterators
 *
 *
 */
template<typename TImage>
class ImageLinearIteratorWithIndex : public ImageIteratorWithIndex<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageLinearIteratorWithIndex Self;
  typedef ImageIteratorWithIndex<TImage>  Superclass;
  
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
  ImageLinearIteratorWithIndex() : ImageIteratorWithIndex<TImage>() {}
  
  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageLinearIteratorWithIndex(ImageType *ptr,
                      const RegionType& region)
    : ImageIteratorWithIndex<TImage>( ptr, region ) {}

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageLinearIteratorWithIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageLinearIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageLinearIteratorWithIndex. */
  ImageLinearIteratorWithIndex( const ImageIteratorWithIndex<TImage> &it)
    { this->ImageIteratorWithIndex<TImage>::operator=(it); }

  /** Go to the next line inside the defined region
   * and be positioned at the starting point on this line
   * \sa operator++ \sa operator-- \sa PreviousLine \sa EndOfLine \sa End */
  void NextLine(void);

  /** Go to the previous line inside the defined region
   * and be positioned at the last point on this line.
   * \sa operator++ \sa operator-- \sa NextLine \sa EndOfLine \sa End */
  void PreviousLine(void);

  /** Go to the beginning pixel of the current line.
   * \sa GoToEndOfLine \sa operator++ \sa operator-- \sa NextLine \sa EndOfLine */
  void GoToBeginOfLine(void);

  /** Go to the past end pixel of the current line.
   * \sa GoToBeginOfLine \sa operator++ \sa operator-- \sa NextLine \sa EndOfLine */
  void GoToEndOfLine(void);

  /** Test if the index is at the end of line on the predefined region */
  inline bool IsAtEndOfLine(void) 
    { return m_PositionIndex[m_Direction] >= m_EndIndex[m_Direction]; }

  /** Test if the index is at the begin of line on the predefined region */
  inline bool IsAtBeginOfLine(void) 
    {
    return m_PositionIndex[m_Direction] < m_BeginIndex[m_Direction];
    }

  /** Set the direction of movement */
  inline void SetDirection(unsigned int direction)
    {
    if( direction >= TImage::ImageDimension )
      {
    itkGenericExceptionMacro(<<"In image of dimension " << TImage::ImageDimension <<" Direction " << direction << " sas selected");
      }
    m_Direction = direction;
    m_Jump = m_OffsetTable[ m_Direction ];
    }

  /** Increment (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator-- */
  Self & operator++();

  /** Decrement (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator++ */
  Self & operator--();

private:
    unsigned long  m_Jump;
    unsigned int   m_Direction;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageLinearIteratorWithIndex.txx"
#endif

#endif 



