/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleImageRegionConstIterator.h
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
#ifndef __itkSimpleImageRegionConstIterator_h
#define __itkSimpleImageRegionConstIterator_h

#include "itkImageConstIteratorWithIndex.h"

namespace itk
{

/**
 * \class SimpleImageRegionConstIterator
 * \brief Multi-dimensional image iterator which only walks a region.
 * 
 * SimpleImageRegionConstIterator is a templated class to represent a
 * multi-dimensional iterator. SimpleImageRegionConstIterator is templated
 * over the image type.  SimpleImageRegionConstIterator is constrained to
 * walk only within the specified region.
 *
 * SimpleImageRegionConstIterator is a multi-dimensional iterator,
 * requiring more information be specified before the iterator can be
 * used than conventional iterators. Whereas the std::vector::iterator
 * from the STL only needs to be passed a pointer to establish the
 * iterator, the multi-dimensional image iterator needs a pointer, the
 * size of the buffer, the size of the region, the start index of the
 * buffer, and the start index of the region. To gain access to this
 * information, SimpleImageRegionConstIterator holds a reference to the
 * image over which it is traversing.
 *
 * SimpleImageRegionConstIterator assumes a particular layout of the image data. The
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
 *
 * \example itkImageIteratorTest
 * \example itkIteratorTests
 * \example itkIteratorsForwardBackwardsTest
 *
 * \code
 * 
 *  IteratorType it( image, image->GetRequestedRegion() );
 *
 *  it.Begin();
 *
 *  while( it.IsNotAtEnd() ) 
 *  {  
 *    it.Set( 100.0 + it.Get() );
 *    ++it;
 *  }
 *
 * \endcode
 *
 *  It also can be used for walking in the reverse direction like
 *
 * \code
 * 
 *  IteratorType it( image, image->GetRequestedRegion() );
 *
 *  it.End();
 *
 *  while( !it.IsAtBegin() ) 
 *  {  
 *    it.Set( 100.0 );
 *    --it;
 *  }
 *
 * \endcode
 *
 *
 * \ingroup ImageIterators
 *
 *
 */
template<typename TImage>
class SimpleImageRegionConstIterator : public ImageConstIteratorWithIndex<TImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SimpleImageRegionConstIterator Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageConstIteratorWithIndex<TImage>  Superclass;

  /** 
   * Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index.
   */
  typedef typename TImage::IndexType  IndexType;

  /**
   * Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Image back to itk::Image to that is it not
   * confused with ImageIterator::Image.
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
   * Region typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Region back to itk::ImageRegion so that is
   * it not confused with ImageIterator::Index.
   */
  typedef typename TImage::RegionType RegionType;

  /**
   * Default constructor. Needed since we provide a cast constructor.
   */
  SimpleImageRegionConstIterator() : ImageConstIteratorWithIndex<TImage>() {}
  
  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  SimpleImageRegionConstIterator( const TImage *ptr,
                                  const RegionType& region )
    : ImageConstIteratorWithIndex<TImage>(ptr, region) {}

  /**
   * Constructor that can be used to cast from an ImageIterator to an
   * SimpleImageRegionConstIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an SimpleImageRegionConstIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a SimpleImageRegionConstIterator.
   */
  SimpleImageRegionConstIterator( const ImageConstIteratorWithIndex<TImage> &it)
    { this->ImageConstIteratorWithIndex<TImage>::operator=(it); }

  
  /**
   * Increment (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the end of the row of the region
   * to the beginning of the next row of the region) up until the iterator
   * tries to moves past the last pixel of the region.  Here, the iterator
   * will be set to be one pixel past the end of the region.
   * \sa operator--
   */
  Self & operator++();

  /**
   * Decrement (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the beginning of the row of the 
   * region to the end of the previous row of the region) up until the iterator
   * tries to moves past the first pixel of the region.  Here, the iterator
   * will be set to be one pixel past the beginning of the region.
   * \sa operator++
   */
  Self & operator--();

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimpleImageRegionConstIterator.txx"
#endif

#endif 

