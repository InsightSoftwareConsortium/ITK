/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhood.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkNeighborhood_h
#define __itkNeighborhood_h

#include <string.h>
#include <valarray>
#include "itkMacro.h"
#include "itkNeighborhoodBase.h"

namespace itk {

/**
 * \class Neighborhood
 * \brief A light-weight object for holding and manipulating a neighborhood
 *  of values.
 *
 *  Neighborhood is a light-weight object for holding and manipulating
 *  a neighborhood of values.  A Neighborhood is usually obtained by
 *  dereferencing a NeighborhoodIterator, but it can be constructed as
 *  a NeighborhoodOperator, or as a product of an operation on one or more
 *  existing Neighborhood objects.
 *
 *  Neighborhood provides a basic set of mathematical operations on its
 *  values, including convolution and inner product with other Neighborhoods.
 *  These operations are building blocks for constructing higher-level
 *  filtering operations on itk::Image data.
 *
 * \sa NeighborhoodPointer
 * \sa NeighborhoodOperator
 * \sa NeighborhoodBase
 * \sa NeighborhoodAlgorithm
 */  


template<class TPixel, unsigned int VDimension = 2>
class ITK_EXPORT Neighborhood : public NeighborhoodBase<TPixel, VDimension>
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef Neighborhood Self;

  /**
   * Standard Superclass typedef
   */
  typedef NeighborhoodBase<TPixel, VDimension> Superclass;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Neighborhood, NeighborhoodBase);

  /**
   * Support for underlying scalar value type of the pixel type.
   */
  typedef typename ScalarTraits<TPixel>::ScalarValueType ScalarValueType;
  
  /**
   * Default constructor method.
   */ 
  Neighborhood() {}
  
  /**
   * Assignment operator.
   */
  Self &operator=( const Self &orig )
  {
    Superclass::operator=(orig);
    return *this;
  }

  /**
   * Sets all of the scalar values in this neighborhood to a pixel value..
   */
  Self &operator=( const TPixel v )
  {
    for (Iterator it = this->Begin(); it < this->End(); ++it)
      {
        *it = v;
      }
    return *this;
  }

  /**
   * Prints some debugging info.
   */
  void PrintSelf();

  /**
   * Prints some debugging info.  Formats the output data logically.
   */
  void PrintScalarData();

  /**
   * Returns the value of the center pixel in a Neighborhood.
   */
  TPixel Center() const
  {
    return this->operator[]((this->size())>>1);
  }

};

  
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhood.txx"
#endif

#endif
