/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageLinearIterator.h
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
 * specified region and along a line parallel to one of the coordinate axis.
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
 * ImageLinearIterator<ImageType> it( image, image->GetRequestedRegion() );
 * 
 * it.SetDirection(2);
 * it.Begin();
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
   * Go to the next line inside the defined region
   * and be positioned at the starting point on this line
   * \sa operator++
   * \sa EndOfLine
   * \sa End
   */
  void NextLine(void);

  /**
   * Test if the index is at the end of line on the predefined region
   */
  inline bool IsAtEndOfLine(void) 
    {
    return m_PositionIndex[m_Direction] >= m_Region.GetSize()[m_Direction];
    }

  /**
   * Set the direction of movement
   */
  inline void SetDirection(unsigned int direction)
    {
    if( direction >= TImage::ImageDimension )
      {
      throw ExceptionObject();
      }
    m_Direction = direction;
    m_Jump = m_OffsetTable[ m_Direction ];
    }

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
