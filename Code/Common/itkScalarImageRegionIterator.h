/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageRegionIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkScalarImageRegionIterator_h
#define __itkScalarImageRegionIterator_h

#include "itkImageRegionIterator.h"
#include "itkPixelTraits.h"


namespace itk
{

/** \class ScalarImageRegionIterator
 * \brief Iterator that invokes GetScalar() on an image.
 *
 * ScalarImageRegionIterator is a templated class to represent a
 * multi-dimensional iterator. It is a specialized form of
 * ImageIterator that invokes the GetScalar() method. GetScalar() is
 * used when you want to write a filter that processes only the scalar
 * portion of a pixel.  
 */

template<class TPixel, unsigned int VImageDimension=2, class TPixelContainer=ValarrayImageContainer<unsigned long, TPixel> >
class ScalarImageRegionIterator : 
public ImageRegionIterator<TPixel,VImageDimension> 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ScalarImageRegionIterator  Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageRegionIterator<TPixel,VImageDimension,TPixelContainer>  Superclass;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ScalarImageRegionIterator, ImageRegionIterator);

  /** 
   * Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index.
   */
  typedef Index<VImageDimension> IndexType;

  /**
   * Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Image back to itk::Image to that is it not
   * confused with ImageIterator::Image.
   */
  typedef Image<TPixel, VImageDimension, TPixelContainer> ImageType;

  /** 
   * PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef TPixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /**
   * Region typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Region back to itk::ImageRegion so that is
   * it not confused with ImageIterator::Index.
   */
  typedef ImageRegion<VImageDimension> RegionType;

  /**
   * Default constructor. Needed since we provide a cast constructor.
   */
  ScalarImageRegionIterator()
    : ImageRegionIterator<TPixel, VImageDimension, TPixelContainer>() {}
  
  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  ScalarImageRegionIterator(ImageType *ptr,
                            const RegionType &region)
    : ImageRegionIterator<TPixel, VImageDimension, TPixelContainer>(ptr, region) {}

  /**
   * Constructor that can be used to cast from an ImageIterator to an
   * ScalarImageRegionIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an ScalarImageRegionIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ScalarImageRegionIterator.
   */
  ScalarImageRegionIterator( const ImageIterator<TPixel, VImageDimension, TPixelContainer> &it)
    { this->ImageIterator<TPixel, VImageDimension, TPixelContainer>::operator=(it); }

  /**
   * Dereference the iterator, returns a reference to the pixel. Used to set
   * or get the value referenced by the index.
   */
   typename ScalarTraits<TPixel>::ScalarValueType& operator*()
    { 
    return ScalarTraits<TPixel>::GetScalar(*( m_Buffer + m_Offset )); 
    }
  
  /**
   * Define operator= for native types.
   */
//   void operator=(const typename TPixel::ScalarValueType v)
//     { 
//     }
  
  /**
   * Define operator= for native types.
   */
//   void operator=(const double v)
//     { 
//     }  
  
};


} // end namespace itk
  
#endif
