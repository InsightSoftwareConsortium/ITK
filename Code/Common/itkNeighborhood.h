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

template<class TPixel, unsigned int VDimension> class Neighborhood;

/**
 *  Templated function for convolving two neighborhoods of arbitrary
 *  dimension.  The third argument dictates the mode of the convolution,
 *  where Mode = 0 is true convolution: the resulting neighborhood object
 *  has radius A::Radius + B::Radius; and Mode = 1 is partial convolution:
 *  the resulting neighborhood object has radius A::Radius.
 *
 *  One of a set of specialized convolution functions on Neighborhoods.
 *  These templated functions are instantiated within the
 *  Neighborhood::Convolve function so that convolution is automatically
 *  specialized for the dimensionality of the neighborhood. 
 */
template<class TPixel, unsigned int VDimension>
Neighborhood<TPixel, VDimension>
ConvolveND(Neighborhood<TPixel, VDimension>&,
         Neighborhood<TPixel, VDimension>&, int);  

/**
 *  Templated function for convolving two neighborhoods, each of three
 *  dimensions.  The third argument dictates the mode of the convolution,
 *  where Mode = 0 is true convolution: the resulting neighborhood object
 *  has radius A::Radius + B::Radius; and Mode = 1 is partial convolution:
 *  the resulting neighborhood object has radius A::Radius.
 *
 *  One of a set of specialized convolution functions on Neighborhoods.
 *  These templated functions are instantiated within the
 *  Neighborhood::Convolve function so that convolution is automatically
 *  specialized for the dimensionality of the neighborhood. 
 */
template<class TPixel, unsigned int VDimension>
Neighborhood<TPixel, VDimension>
Convolve3D(Neighborhood<TPixel, VDimension> &,
           Neighborhood<TPixel, VDimension> &, int);


/**
 *  Templated function for convolving two neighborhoods,each of two
 *  dimensions.  The third argument dictates the mode of the convolution,
 *  where Mode = 0 is true convolution: the resulting neighborhood object
 *  has radius A::Radius + B::Radius; and Mode = 1 is partial convolution:
 *  the resulting neighborhood object has radius A::Radius.
 *
 *  One of a set of specialized convolution functions on Neighborhoods.
 *  These templated functions are instantiated within the
 *  Neighborhood::Convolve function so that convolution is automatically
 *  specialized for the dimensionality of the neighborhood. 
 */
template<class TPixel, unsigned int VDimension>
Neighborhood<TPixel, VDimension>
Convolve2D(Neighborhood<TPixel, VDimension> &,
           Neighborhood<TPixel, VDimension> &, int);

/**
 *  Templated function for convolving two neighborhoods, each of one
 *  dimension.  The third argument dictates the mode of the convolution,
 *  where Mode = 0 is true convolution: the resulting neighborhood object
 *  has radius A::Radius + B::Radius; and Mode = 1 is partial convolution:
 *  the resulting neighborhood object has radius A::Radius.
 *
 *  One of a set of specialized convolution functions on Neighborhoods.
 *  These templated functions are instantiated within the
 *  Neighborhood::Convolve function so that convolution is automatically
 *  specialized for the dimensionality of the neighborhood.
 */
template<class TPixel, unsigned int VDimension>
Neighborhood<TPixel, VDimension>
Convolve1D(Neighborhood<TPixel, VDimension> &,
           Neighborhood<TPixel, VDimension> &, int);  
  
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
   * Calculates the inner product with a valarray (and therefore also any
   * NeighborhoodBase subclass) argument.  Returns a scalar value.
   * The result of the inner product between two arrays of data of unequal
   * size is undefined.  For efficiency, InnerProduct does no bounds checking.
   * \sa SlicedInnerProduct
   */
  ScalarValueType InnerProduct(std::valarray<TPixel> &);

  /**
   * Slices the neighborhood and returns the slice's inner product with
   * the valarray argument.
   * \sa InnerProduct
   */
  ScalarValueType SlicedInnerProduct(const std::slice &,
                                           std::valarray<TPixel> &);

  /**
   * Returns the value of the center pixel in a Neighborhood.
   */
  TPixel Center() const
  {
    return this->operator[]((this->size())>>1);
  }

  /**
   * Returns the full convolution of two Neighborhoods.  Convolve will
   * instantiate one of a set of specialized convolution functions
   * on Neighborhoods according to the dimensionality of the
   * two Neighborhoods.
   *
   * Convolve returns a Neighborhood with radii equal to the sum
   * of its two operands' radii, respectively.
   */
  Self Convolve(Self &B)
  {
    if (VDimension == 2)
      {
      return Convolve2D<TPixel, VDimension>(*this, B, 0);
      }
    else if (VDimension == 3)
      {
      return Convolve3D<TPixel, VDimension>(*this, B, 0);
      }
    else if (VDimension == 1)
      {
      return Convolve1D<TPixel, VDimension>(*this, B, 0);
      }
    else
      {
      return ConvolveND<TPixel, VDimension>(*this, B, 0);
      }
  }

  /**
   * Returns a partial convolution of two Neighborhoods.  Convolve will
   * instantiate one of a set of specialized convolution functions
   * on Neighborhoods according to the dimensionality of the
   * two Neighborhoods.
   *
   * Convolve returns a Neighborhood with radii equal to the radii
   * of the Neighborhood on which it is called.
   *
   */
  Self ConvolveToSize(Self &B)
  {
    if (VDimension == 2)
      {
      return Convolve2D<TPixel, VDimension>(*this, B, 1);
      }
    else if (VDimension == 3)
      {
      return Convolve3D<TPixel, VDimension>(*this, B, 1);
      }
    else if (VDimension == 1)
      {
      return Convolve1D<TPixel, VDimension>(*this, B, 1);
      }
    else
      {
      return ConvolveND<TPixel, VDimension>(*this, B, 1);
      }
  }
   
};

  
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhood.txx"
#endif

#endif
