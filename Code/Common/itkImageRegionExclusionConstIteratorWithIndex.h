/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionExclusionConstIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageRegionExclusionConstIteratorWithIndex_h
#define __itkImageRegionExclusionConstIteratorWithIndex_h

#include "itkImageConstIteratorWithIndex.h"

namespace itk
{

/** \class ImageRegionExclusionConstIteratorWithIndex
 * \brief Multi-dimensional image iterator which walks a region 
 * excluding a second region completly contained in the first.
 * 
 * ImageRegionExclusionConstIteratorWithIndex is a templated class to represent a
 * multi-dimensional iterator. ImageRegionExclusionConstIteratorWithIndex is templated
 * over the image type.  ImageRegionExclusionConstIteratorWithIndex is constrained to
 * walk only within the specified region. The exclusion regions has to be set
 * after construction. By default the exclusion region is empty and the 
 * iterator will behave as the itk::ImageRegionIteratorWithIndex with a penalty
 * in performace due to internal extra checking.
 *
 * ImageRegionExclusionConstIteratorWithIndex is a multi-dimensional iterator,
 * requiring more information be specified before the iterator can be
 * used than conventional iterators. Whereas the std::vector::iterator
 * from the STL only needs to be passed a pointer to establish the
 * iterator, the multi-dimensional image iterator needs a pointer, the
 * size of the buffer, the size of the region, the start index of the
 * buffer, and the start index of the region. To gain access to this
 * information, ImageRegionExclusionConstIteratorWithIndex holds a reference to the
 * image over which it is traversing.
 *
 * ImageRegionExclusionConstIteratorWithIndex assumes a particular layout of the image data. The
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
 * \example itkImageRegionExclusionConstIteratorWithIndex
 *
 * \code
 * 
 *  IteratorType it( image, image->GetRequestedRegion() );
 *
 *  it.SetExclusionRegion( exclusionRegion );
 *
 *  it.GoToBegin();
 *
 *  while( ! it.IsAtEnd() ) 
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
 *  it.SetExclusionRegion( exclusionRegion );
 *  it.GoToEnd();
 *
 *  while( !it.IsAtBegin() ) 
 *  {  
 *    it.Set( 100.0 );
 *    --it;
 *  }
 *
 * \endcode
 *
 * \ingroup ImageIterators
 */
template<typename TImage>
class ImageRegionExclusionConstIteratorWithIndex : public ImageConstIteratorWithIndex<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionExclusionConstIteratorWithIndex Self;
  typedef ImageConstIteratorWithIndex<TImage>  Superclass;
  
  /** Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index. */
  typedef typename TImage::IndexType  IndexType;
  typedef typename TImage::SizeType SizeType;

  /** Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Image back to itk::Image to that is it not
   * confused with ImageIterator::Image. */
  typedef TImage ImageType;

  /** PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename TImage::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;
  
  /** Region typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Region back to itk::ImageRegion so that is
   * it not confused with ImageIterator::Index. */
  typedef typename TImage::RegionType RegionType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRegionExclusionConstIteratorWithIndex() : ImageConstIteratorWithIndex<TImage>() {}
  
  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionExclusionConstIteratorWithIndex(TImage *ptr,
                            const RegionType& region)
    : ImageConstIteratorWithIndex<TImage>(ptr, region) {}

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionExclusionConstIteratorWithIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageRegionExclusionConstIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionExclusionConstIteratorWithIndex. */
  ImageRegionExclusionConstIteratorWithIndex( const ImageConstIteratorWithIndex<TImage> &it)
    { this->ImageConstIteratorWithIndex<TImage>::operator=(it); }

  /** Increment (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the end of the row of the region
   * to the beginning of the next row of the region) up until the iterator
   * tries to moves past the last pixel of the region.  Here, the iterator
   * will be set to be one pixel past the end of the region.
   * \sa operator++(int) */
  Self & operator++();

  /** Decrement (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the beginning of the row of the region
   * to the end of the next row of the region) up until the iterator
   * tries to moves past the first pixel of the region.  Here, the iterator
   * will be set to be one pixel past the beginning of the region.
   * \sa operator--(int) */
  Self & operator--();


  /** Method to define the Exclusion region. The iterator will skip pixels 
   * inside this region. 
   * \warning The exclusion region must be completly containe inside the
   * normal region used to construct the iterator. A border of at least on
   * pixels should exist between the normal region and the exclusion region.
   */
  void SetExclusionRegion( const RegionType & region );

private:

  RegionType      m_ExclusionRegion;

  IndexType       m_ExclusionBegin;
  IndexType       m_ExclusionEnd;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionExclusionConstIteratorWithIndex.txx"
#endif

#endif 
