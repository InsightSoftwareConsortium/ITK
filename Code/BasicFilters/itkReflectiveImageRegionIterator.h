/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectiveImageRegionIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkReflectiveImageRegionIterator_h
#define __itkReflectiveImageRegionIterator_h

#include "itkImageIteratorWithIndex.h"

namespace itk
{

/**
 * \class ReflectiveImageRegionIterator
 * \brief Multi-dimensional image iterator which only walks a region.
 * 
 * ReflectiveImageRegionIterator is a templated class to represent a
 * multi-dimensional iterator. ReflectiveImageRegionIterator is templated
 * over the image type.  ReflectiveImageRegionIterator is constrained to
 * walk only within the specified region.
 *
 * ReflectiveImageRegioIterator will perform two passes over the image
 * along each dimension. It is useful for algorithms that require to 
 * go back and forth (once) over the data. 
 *
 * \sa DanielssonDistanceMapImageFilter
 *
 * */
template<typename TImage>
class ReflectiveImageRegionIterator : public ImageIteratorWithIndex<TImage>
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef ReflectiveImageRegionIterator Self;

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
  ReflectiveImageRegionIterator() ;
  
  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  ReflectiveImageRegionIterator(TImage *ptr,
                            const RegionType& region)
    : ImageIteratorWithIndex<TImage>(ptr, region) {}

  /**
   * Constructor that can be used to cast from an ImageIterator to an
   * ReflectiveImageRegionIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an ReflectiveImageRegionIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ReflectiveImageRegionIterator.
   */
  ReflectiveImageRegionIterator( const ImageIteratorWithIndex<TImage> &it)
    { this->ImageIteratorWithIndex<TImage>::operator=(it); }

  bool IsReflected(unsigned int) const;
  /**
   * Increment (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the end of the row of the region
   * to the beginning of the next row of the region) up until the iterator
   * tries to moves past the last pixel of the region.  Here, the iterator
   * will be set to be one pixel past the end of the region.
   * \sa operator++(int)
   */
  Self & operator++();

private:
  bool m_IsFirstPass[TImage::ImageDimension];
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkReflectiveImageRegionIterator.txx"
#endif

#endif
