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
/**
 * itkImageScalarIterator is a templated class to represent a
 * multi-dimensional iterator. It is a specialized form of
 * itkImageIterator that invokes the GetScalar() method. GetScalar() is
 * used when you want to write a filter that processes only the scalar
 * portion of a pixel.  
 */

#ifndef __itkImageScalarIterator_h
#define __itkImageScalarIterator_h

#include "itkImageIterator.h"
#include "itkPixelTraits.h"

template<class TPixel, unsigned int TImageDimension=2>
class itkImageScalarIterator : public itkImageIterator<TPixel,TImageDimension> {
public:
  /**
   * Dereference the iterator, returns a reference to the pixel. Used to set
   * or get the value referenced by the index.
   */
   typename TPixel::ScalarType& operator*()
    { 
    return itkScalarTraits<TPixel>::GetScalar(*( m_Image + m_Offset )); 
    }
  bool
  operator!=(const itkImageScalarIterator<TPixel,TImageDimension> &it) const
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

#endif 



