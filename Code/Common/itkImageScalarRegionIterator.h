/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageScalarRegionIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkImageScalarRegionIterator_h
#define __itkImageScalarRegionIterator_h

#include "itkImageRegionIterator.h"
#include "itkPixelTraits.h"

ITK_NAMESPACE_BEGIN

/** \class ImageScalarRegionIterator
 * \brief Iterator that invokes GetScalar() on an image.
 *
 * ImageScalarRegionIterator is a templated class to represent a
 * multi-dimensional iterator. It is a specialized form of
 * ImageIterator that invokes the GetScalar() method. GetScalar() is
 * used when you want to write a filter that processes only the scalar
 * portion of a pixel.  
 */

template<class TPixel, unsigned int VImageDimension=2>
class ImageScalarRegionIterator : public ImageRegionIterator<TPixel,VImageDimension> {
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageScalarRegionIterator  Self;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageScalarRegionIterator, ImageRegionIterator);

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
  ImageScalarRegionIterator()
    : ImageRegionIterator<TPixel, VImageDimension>() {}
  
  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  ImageScalarRegionIterator(const SmartPointer<Image> &ptr,
                      const Index &start,
                      const unsigned long size[VImageDimension])
    : ImageRegionIterator<TPixel, VImageDimension>(ptr, start, size) {}

  /**
   * Constructor that can be used to cast from an ImageIterator to an
   * ImageScalarRegionIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageScalarRegionIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageScalarRegionIterator.
   */
  ImageScalarRegionIterator( const ImageIterator<TPixel, VImageDimension> &it)
    { this->ImageIterator<TPixel, VImageDimension>::operator=(it); }

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
  void operator=(const typename TPixel::ScalarValueType v)
    { 
    }
  
  /**
   * Define operator= for native types.
   */
  void operator=(const double v)
    { 
    }  
  
};

ITK_NAMESPACE_END
  
#endif
