/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVector.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkVector_h
#define __itkVector_h


#include "itkArray.h"
#include "vnl/vnl_vector_ref.h"


namespace itk
{

/** \class Vector
 * \brief A templated class holding a n-Dimensional vector.
 * 
 * Vector is a templated class that holds a single vector (i.e., an array
 * of values).  Vector can be used as the data type held at each pixel in
 * an Image or at each vertex of an Mesh. The template parameter T can
 * be any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  The TVectorDimension defines the number of
 * components in the vector array. 
 *
 * Vector is not a dynamically extendible array like std::vector. It is
 * intended to be used like a mathematical vector.
 *
 * If you wish a simpler pixel types, you can use Scalar, which represents
 * a single data value at a pixel. There is also the more complex type
 * ScalarVector, which supports (for a given pixel) a single scalar value
 * plus an array of vector values. (The scalar and vectors can be of
 * different data type.)
 * 
 * \sa Image
 * \sa Mesh
 * \sa Point
 * \sa CovariantVector
 * \sa Matrix
 *
 */

template<class T, unsigned int TVectorDimension=3>
class Vector : public Array<T,TVectorDimension> {
 public:
  /**
   * Standard "Self" typedef.
   */
  typedef Vector  Self;
  
  /**
   * ValueType can be used to declare a variable that is the same type
   * as a data element held in an Vector.  
   */
  typedef T ValueType;

  /**
   * Dimension of the Space
   */
  enum { VectorDimension = TVectorDimension };

  typedef Self VectorType;
  
  /**
   * The Array type from which this Vector is derived.
   */
  typedef Array<T, TVectorDimension>                BaseArray;
  typedef typename BaseArray::ArrayCommaListCopier  ArrayCommaListCopier;
  
  /**
   * Get the dimension (size) of the vector.
   */
  static unsigned int GetVectorDimension() 
    { return TVectorDimension; }  

  /**
   * Get a vnl_vector_ref referencing the same memory block
   */
  vnl_vector_ref<T> Get_vnl_vector( void );


  /**
   * Default constructor has nothing to do.
   */
  Vector() {}

  /*@{
   * Pass-through constructor for the Array base class.
   */
  Vector(const Self& r): BaseArray(r) {}
  Vector(const typename BaseArray::Reference& r): BaseArray(r) {}
  Vector(const typename BaseArray::ConstReference& r): BaseArray(r) {}
  Vector(const ValueType r[VectorDimension]): BaseArray(r) {}  
  //@}
  
  /*@{
   * Pass-through assignment operator for the Array base class.
   */
  Vector& operator= (const Self& r);
  Vector& operator= (const typename BaseArray::Reference& r);
  Vector& operator= (const typename BaseArray::ConstReference& r);
  Vector& operator= (const ValueType r[VectorDimension]);
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
   * Vector operator+=.  Adds a vectors to the current vector.
   */
  const Self& operator+=(const Self &vec);


  /**
   * Vector operator-=.  Subtracts a vector from a current vector.
   */
  const Self& operator-=(const Self &vec);


  /**
   * Vector negation.  Negate all the elements of a vector. Return a new vector
   */
  Self operator-() const;
  

  /**
   * Vector addition. Add two vectors. Return a new vector.
   */
  Self operator+(const Self &vec) const;
  

   /**
   * Vector subtraction. Subtract two vectors. Return a new vector.
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
#include "itkVector.txx"
#endif


#endif 
