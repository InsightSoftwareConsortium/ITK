/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageScalarIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkImageScalarIterator_h
#define __itkImageScalarIterator_h

#include "itkImageIterator.h"
#include "itkPixelTraits.h"

namespace itk
{

/** \class ImageScalarIterator
 * \brief Iterator that invokes GetScalar() on an image.
 *
 * ImageScalarIterator is a templated class to represent a
 * multi-dimensional iterator. It is a specialized form of
 * ImageIterator that invokes the GetScalar() method. GetScalar() is
 * used when you want to write a filter that processes only the scalar
 * portion of a pixel.  
 */

template<class TPixel, unsigned int TImageDimension=2>
class ImageScalarIterator : public ImageIterator<TPixel,TImageDimension> 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageScalarIterator  Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageIterator<TPixel,TImageDimension>  Superclass;

  /**
   * Dereference the iterator, returns a reference to the pixel. Used to set
   * or get the value referenced by the index.
   */
   typename TPixel::ScalarType& operator*()
    { 
    return ScalarTraits<TPixel>::GetScalar(*( m_Image + m_Offset )); 
    }
  bool
  operator!=(const Self& it) const
    {
    return m_Offset != it.m_Offset;
    }
  
  /**
   * Define operator= for native types.
   */
  void operator=(const typename TPixel::ScalarType v)
    { 
    }
  
  /**
   * Define operator= for native types.
   */
  void operator=(const double v)
    { 
    }  
  
};

} // end namespace itk
  
#endif
