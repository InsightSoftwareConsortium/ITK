/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalar.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
  ==========================================================================*/
#ifndef __itkScalar_h
#define __itkScalar_h

#include "itkMacro.h"

namespace itk
{

/** \class Scalar
 * \brief A templated class holding a single scalar value and responding to
 *        the GetScalar() method.
 * 
 * Scalar is a templated class that holds a single scalar value.
 * Scalar can be used as the data type held at each pixel in an Image
 * or at each vertex (or cell) of an Mesh. The template parameter can be
 * any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  itk filters that rely on scalar data assume the
 * data type held at each pixel or each vertex responds to
 * GetScalar()/SetScalar() methods. If not, a compile time error will occur.
 *
 * For efficiency sake, Scalar does not define a default constructor, a
 * copy constructor, or many operators. We rely on the compiler to provide
 * efficient methods.
 *
 * If you wish more complex pixel types, you can use Vector, which
 * represents an array values of the same data type. There is also an
 * ScalarVector, which supports (for a given pixel) a single scalar value
 * plus an array of vector values. (The scalar and vectors can be of
 * different data type.)
 * 
 * \sa Image
 * \sa Mesh
 * \sa Vector
 * \sa ScalarVector 
 */

template<class T>
class Scalar {
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Scalar Self;
  
  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in an Scalar.
   */
  typedef T ValueType;

  /**
   * ScalarValueType can be used to declare a variable that is the same type
   * as the data held in an Scalar.
   */
  typedef T ScalarValueType;

  /**
   * Get the dimension (size) of this scalar. The method is necessary
   * to satisfy the pixel manipulation API.
   */
  static unsigned int GetPixelDimension() 
    { return 1; }
  
  /**
   * Get the scalar value.
   * \sa SetScalar()
   */
  T& GetScalar()
  { return m_Scalar; }

  /**
   * Operator to get (cast) the scalar value. This version can be used as
   * an lvalue. Note that by returning a reference to T, the compiler now
   * has a way to implicitly create operator==, operator>, operator+,
   * operator+=, etc.
   */
  operator T& ()
  { return m_Scalar; };

  /**
   * Operator to get the scalar value as an rvalue.
   */
  operator T () const
  { return m_Scalar; };
  
  /**
   * Set the scalar value.
   * \sa GetScalar()
   */
  void SetScalar(const T &val) { m_Scalar = val; }

  /**
   * Operator to set the scalar value.
   */
  Self& operator= (const T &val) { m_Scalar = val; return *this;}
  
  
private:
  T m_Scalar;
};

} // end namespace itk
  
#endif 
