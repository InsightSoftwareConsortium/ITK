/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkImageRegionIterator_h
#define __itkImageRegionIterator_h

#include "itkImageIterator.h"

namespace itk
{

/**
 * \class ImageRegionIterator
 * \brief Multi-dimensional image iterator which only walks a region.
 * 
 * ImageRegionIterator is a templated class to represent a multi-dimensional
 * iterator. ImageRegionIterator is templated over the dimension of the image
 * and the data type of the image. ImageRegionIterator is constrained to walk
 * only within the specified region.
 *
 * ImageRegionIterator is a multi-dimensional iterator, requiring more
 * information be specified before the iterator can be used than conventional
 * iterators. Whereas the std::vector::iterator from the STL only needs to be
 * passed a pointer to establish the iterator, the multi-dimensional image
 * iterator needs a pointer, the size of the buffer, the size of the region,
 * the start index of the buffer, and the start index of the region. To gain
 * access to this information, ImageRegionIterator holds a reference to the
 * image over which it is traversing.
 *
 * ImageRegionIterator assumes a particular layout of the image data. The
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
template<typename TPixel, unsigned int VImageDimension=2>
class ImageRegionIterator : public ImageIterator<TPixel, VImageDimension>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageRegionIterator Self;

  /** 
   * Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index.
   */
  typedef itk::Index<VImageDimension> Index;

  /**
   * Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index.
   */
  typedef itk::Image<TPixel, VImageDimension> Image;

  /**
   * Default constructor. Needed since we provide a cast constructor.
   */
  ImageRegionIterator() : ImageIterator<TPixel, VImageDimension>() {}
  
  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  ImageRegionIterator(const SmartPointer<Image> &ptr,
                      const Index &start,
                      const unsigned long size[VImageDimension])
    : ImageIterator<TPixel, VImageDimension>(ptr, start, size) {}

  /**
   * Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageRegionIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionIterator.
   */
  ImageRegionIterator( const ImageIterator<TPixel, VImageDimension> &it)
    { this->ImageIterator<TPixel, VImageDimension>::operator=(it); }

  
  /**
   * Increment (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the end of the row of the region
   * to the beginning of the next row of the region) up until the iterator
   * tries to moves past the last pixel of the region.  Here, the iterator
   * will be set to be one pixel past the end of the region.
   * \sa operator++(int)
   */
  Self &
  operator++()
  {
    // I think we can make this routine a lot faster by operating strictly
    // on m_Offset and using the offset table to determine when to wrap.
    
#if 1
    // Make sure that index is up to date.  This is expensive.  Can we do
    // the same thing by operating strictly on m_Offset?
    ImageIterator<TPixel, VImageDimension>::Index
      ind = m_Image->ComputeIndex( m_Offset );

    // increment along a row, then wrap at the end of the region row.
    int done;
    int dim;
    ind[0]++;

    // check to see if we are past the last pixel in the region
    done = (ind[0] == m_StartIndex[0] + m_Size[0]);
    for (unsigned int i=1; done && i < VImageDimension; i++)
      {
      done = (ind[i] == m_StartIndex[i] + m_Size[i] - 1);
      }
    
    // if the iterator is outside the region (but not past region end) then
    // we need to wrap around the region
    dim = 0;
    while (!done)
      {
      done = 1;
      if (dim < VImageDimension-1)
	{
	if (ind[dim] > m_StartIndex[dim] + m_Size[dim] - 1)
	  {
	  ind[dim] = m_StartIndex[dim];
	  ind[++dim]++;
	  done = 0;
	  }
	}
      }
    m_Offset = m_Image->ComputeOffset( ind );
    return *this;
#else
#endif
  }
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionIterator.txx"
#endif

#endif 
