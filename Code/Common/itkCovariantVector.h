/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovariantVector.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkCovariantVector_h
#define __itkCovariantVector_h


#include "itkArray.h"
#include "vnl/vnl_vector_ref.h"


namespace itk
{

/** \class CovariantVector
 * \brief A templated class holding a n-Dimensional covariant vector.
 * 
 * CovariantVector is a templated class that holds a single vector (i.e., an array
 * of values).  CovariantVector can be used as the data type held at each pixel in
 * an Image or at each vertex of an Mesh. The template parameter T can
 * be any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  The TCovariantVectorDimension defines the number of
 * components in the vector array. 
 *
 * CovariantVector is not a dynamically extendible array like std::vector. It is
 * intended to be used like a mathematical vector.
 *
 * If you wish a simpler pixel types, you can use Scalar, which represents
 * a single data value at a pixel. There is also the more complex type
 * ScalarCovariantVector, which supports (for a given pixel) a single scalar value
 * plus an array of vector values. (The scalar and vectors can be of
 * different data type.)
 * 
 * CovariantVector is the type that should be used for representing normals
 * to surfaces and gradients of functions. Affine Transforms transform
 * covariant vectors different than vectors.
 *
 * \sa Image
 * \sa Mesh
 * \sa Point
 * \sa CovariantCovariantVector
 * \sa Matrix
 *
 */

template<class T, unsigned int TCovariantVectorDimension=3>
class CovariantVector : public Array<T,TCovariantVectorDimension> {
 public:
  /**
   * Standard "Self" typedef.
   */
  typedef CovariantVector  Self;
  
  /**
   * ValueType can be used to declare a variable that is the same type
   * as a data element held in an CovariantVector.  
   */
  typedef T ValueType;

  /**
   * Dimension of the Space
   */
  enum { CovariantVectorDimension = TCovariantVectorDimension };

  typedef Self CovariantVectorType;
  
  /**
   * The Array type from which this CovariantVector is derived.
   */
  typedef Array<T, TCovariantVectorDimension>                BaseArray;
  typedef typename BaseArray::ArrayCommaListCopier  ArrayCommaListCopier;
  
  /**
   * Get the dimension (size) of the vector.
   */
  static unsigned int GetCovariantVectorDimension() 
    { return TCovariantVectorDimension; }  

  /**
   * Get a vnl_vector_ref referencing the same memory block
   */
  vnl_vector_ref<T> Get_vnl_vector( void );


  /**
   * Default constructor has nothing to do.
   */
  CovariantVector() {}

  /*@{
   * Pass-through constructor for the Array base class.
   */
  CovariantVector(const Self& r): BaseArray(r) {}
  CovariantVector(const typename BaseArray::Reference& r): BaseArray(r) {}
  CovariantVector(const typename BaseArray::ConstReference& r): BaseArray(r) {}
  CovariantVector(const ValueType r[CovariantVectorDimension]): BaseArray(r) {}  
  //@}
  
  /*@{
   * Pass-through assignment operator for the Array base class.
   */
  CovariantVector& operator= (const Self& r);
  CovariantVector& operator= (const typename BaseArray::Reference& r);
  CovariantVector& operator= (const typename BaseArray::ConstReference& r);
  CovariantVector& operator= (const ValueType r[CovariantVectorDimension]);
  ArrayCommaListCopier operator= (const ValueType& r);
  //@}
  
  /**
   * Scalar operator*=.  Scales elements by a scalar.
   */
  const Self& operator*=(const ValueType &value);


  /**
   * Scalar operator/=.  Scales (divides) elements by a scalar.
   */
  const Self& operator/=(const ValueType &value);


  /**
   * CovariantVector operator+=.  Adds a vectors to the current vector.
   */
  const Self& operator+=(const Self &vec);


  /**
   * CovariantVector operator-=.  Subtracts a vector from a current vector.
   */
  const Self& operator-=(const Self &vec);


  /**
   * CovariantVector negation.  Negate all the elements of a vector. Return a new vector
   */
  Self operator-() const;
  

  /**
   * CovariantVector addition. Add two vectors. Return a new vector.
   */
  Self operator+(const Self &vec) const;
  

   /**
   * CovariantVector subtraction. Subtract two vectors. Return a new vector.
   */
  Self operator-(const Self &vec) const;
  

  /**
   * Scalar operator*. Scale the elements of a vector by a scalar.
   * Return a new vector.
   */
  Self operator*(const ValueType& val) const;
  

  /**
   * Scalar operator/. Scale (divide) the elements of a vector by a scalar.
   * Return a new vector.
   */
  Self operator/(const ValueType& val) const;


  /**
   * Returns the Euclidean Norm of the vector 
   */
  ValueType GetNorm( void ) const;
  

  /**
   * Returns vector's Squared Euclidean Norm 
   */
  ValueType GetSquaredNorm( void ) const;
 
};

  
} // end namespace itk
  

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCovariantVector.txx"
#endif


#endif 
