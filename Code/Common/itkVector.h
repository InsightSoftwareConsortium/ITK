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

#include "itkMacro.h"

#include <memory.h>

ITK_NAMESPACE_BEGIN

/** \class Vector
 * \brief A templated class holding a n vector values and responding to
 *        the GetVector() method.
 * 
 * Vector is a templated class that holds a single vector (i.e., an array
 * of values).  Vector can be used as the data type held at each pixel in
 * an Image or at each vertex of an Mesh. The template parameter T can
 * be any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  The TVectorDimension defines the number of
 * components in the vector array. itk filters that rely on vector data
 * assume the data type held at each pixel or each vertex responds to
 * GetVector()/SetVector() methods. If not, a compile time error will occur.
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
 * \sa Vector
 * \sa ScalarVector 
 */

template<class T, unsigned int TVectorDimension=3>
class Vector {
 public:
  /**
   * Standard "Self" typedef.
   */
  typedef Vector  Self;
  
  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in an Vector.  
   */
  typedef T ValueType;

  /**
   * Get the dimension (size) of the vector.
   */
  static unsigned int GetVectorDimension() 
    { return TVectorDimension; }
  
  /**
   * Get the vector. This provides a read only reference to the vector.
   * \sa SetVector
   */
  const T *GetVector() const 
    { return m_Vector; }

  /**
   * Set the vector.
   * \sa GetVector
   */
  void SetVector(const T *val)
    { memcpy(m_Vector, val, sizeof(T)*TVectorDimension); };

  /**
   * Add a vector to this vector and return a new vector.
   */
  const Self
  operator+(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < TVectorDimension; i++)
      { result[i] = m_Vector[i] + vec.m_Vector[i]; }
    return result;
    }

  /**
   * Add a vector to this vector, modifying the value of this vector.
   */
  const Self &
  operator+=(const Self &vec)
    {
    for (unsigned int i=0; i < TVectorDimension; i++)
      { m_Vector[i] += vec.m_Vector[i]; }
    return *this;
    }

  /**
   * Subtract a vector from this vector and return a new vector.
   */
  const Self
  operator-(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < TVectorDimension; i++)
      { result[i] = m_Vector[i] - vec.m_Vector[i]; }
    return result;
    }

  /**
   * Subtract a vector from this vector, modifying the value of this vector.
   */
  const Self &
  operator-=(const Self &vec)
    {
    for (unsigned int i=0; i < TVectorDimension; i++)
      { m_Vector[i] -= vec.m_Vector[i]; }
    return *this;
    }

 private:
  T m_Vector[TVectorDimension];
};

  
ITK_NAMESPACE_END

#endif 
