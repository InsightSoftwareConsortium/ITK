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

#include <cstring>
#include <valarray>
#include "itkMacro.h"
#include "itkNeighborhoodBase.h"

namespace itk {

template<class TPixel, unsigned int VDimension>  class Neighborhood;

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
Convolve(Neighborhood<TPixel, VDimension>&,
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
Neighborhood<TPixel, 3>
Convolve(Neighborhood<TPixel, 3> &, Neighborhood<TPixel, 3> &, int);


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
Neighborhood<TPixel, 2>
Convolve(Neighborhood<TPixel, 2> &, Neighborhood<TPixel, 2> &, int);

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
Neighborhood<TPixel, 1>
Convolve(Neighborhood<TPixel, 1> &, Neighborhood<TPixel, 1> &, int);  
  
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
class Neighborhood : public NeighborhoodBase<TPixel, VDimension>
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef Neighborhood Self;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Neighborhood, NeighborhoodBase);

  /**
   * Default constructor method.
   */ 
  Neighborhood() {}

  /**
   * Assignment operator.
   */
  Neighborhood &operator=( Neighborhood &orig )
  {
    NeighborhoodBase<TPixel, VDimension>::operator=(orig);
    this->resize(orig.size());
    memcpy(this->begin(), orig.begin(), this->size() * sizeof(TPixel));
    return *this;
  }

  /**
   * Sets all of the values in this neighborhood to a constant.
   */
  Neighborhood &operator=( const TPixel &v )
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
  void Print();

  /**
   * Prints some debugging info.  Formats the output data logically.
   */
  void PrintData();

  /**
   * Calculates the inner product with a valarray (and therefore also any
   * NeighborhoodBase subclass) argument.  Returns a scalar value.
   * The result of the inner product between two arrays of data of unequal
   * size is undefined.  For efficiency, InnerProduct does no bounds checking.
   * \sa SlicedInnerProduct
   */
  TPixel InnerProduct(std::valarray<TPixel> &);

  /**
   * Slices the neighborhood and returns the slice's inner product with
   * the valarray argument.
   * \sa InnerProduct
   */
  TPixel SlicedInnerProduct(const std::slice &, std::valarray<TPixel> &);

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
  Neighborhood Convolve(Neighborhood &B)
  {
    return itk::Convolve<TPixel, VDimension>(*this, B, 0); 
  }

  /**
   * Returns a partial convolution of two Neighborhoods.  Convolve will
   * instantiate one of a set of specialized convolution functions
   * on Neighborhoods according to the dimensionality of the
   * two Neighborhoods.
   *
   * Convolve returns a Neighborhood with radii equal to the radii
   * of the Neighborhood on which it is called.
   */
  Neighborhood ConvolveToSize(Neighborhood &B)
  {
    return itk::Convolve<TPixel, VDimension>(*this, B, 1); 
  }
   
  /**
   * Returns a Neighborhood that is a mirror image of this Neighborhood across
   * all its axes.
   */
  Neighborhood Mirror(void);
  
  /**
   * Returns the sum of all the pixel values in the Neighborhood.
   */
  TPixel Sum()
  {
    TPixel accum;
    for (ConstIterator it = this->Begin(); it < this->End(); ++it)
      {
        accum+=*it;
      }
    return accum;
  }

  /**
   * Returns a Neighborhood whose pixel values are the sum of the
   * respective pixel values of the two operands.
   */
  template<class YPixel>
  Neighborhood<TPixel, VDimension> operator+(const YPixel &) const;

  /**
   * Returns a Neighborhood whose pixel values are all raised to
   * a power.
   */
  Neighborhood<TPixel, VDimension> pow(const double &) const;
  
};

  
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhood.txx"
#endif

#endif
