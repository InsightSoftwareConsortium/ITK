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

/**
 * itkVector is a templated class that holds a single vector.  itkVector
 * can be used as the data type held at each pixel in an itkImage or at each
 * vertex of an itkMesh. The template parameter can be any data type that
 * behaves like a primitive (or atomic) data type (int, short, float, complex).
 * itk filters that rely on vector data assume the data type held at each
 * pixel or each vertex responds to GetVector()/SetVector() methods. If not,
 * a compile time error will occur.
 *
 * itkVector is not a dynamically extendible array like std::vector. It is
 * intended to be used like a mathematical vector.
 *
 * For efficiency sake, itkVector does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * \sa itkImage
 * \sa itkMesh
 * \sa itkVector
 * \sa itkScalarVector
 * \sa itkTensor
 * \sa itkScalarTensor
 * \sa itkVectorTensor
 * \sa itkScalarVectorTensor
 */

// To compile / test this class
// Windows: cl itkDataTypeTest.cxx; .\itkDataTypeTest.exe
// linux:   c++ itkDataTypeTest.cxx; ./a.out
// other:   CCcompiler itkDataTypeTest.cxx;  ./a.out

#ifndef __itkVector_h
#define __itkVector_h

#include <memory.h>

namespace itk {
  
template<class T, int TVectorDimension=3>
class itkVector {
 public:
  /**
   * vector_value_type can be used to declare a variable that is the same type
   * as the data held in an itkVector.  The naming convention here follows
   * STL conventions. vector_value_type is what STL would call value_type.
   * vector_value_type is used instead of STL's value_type to avoid confusion
   * when a data type holds a scalar and vector of different types.
   * \sa itkScalarVector
   */
  typedef typename T vector_value_type;

  /**
   * Get the dimension (size) of the vector.
   */
  int GetVectorDimension() const { return TVectorDimension; }
  
  /**
   * Get the vector. This provides a read only reference to the vector.
   * \sa SetVector
   */
  const T *GetVector() const { return m_Vector; };

  /**
   * Set the vector.
   * \sa GetVector
   */
  void SetVector(const T *val)
  { memcpy(m_Vector, val, sizeof(T)*TVectorDimension); };

  /**
   * Add a vector to this vector and return a new vector.
   */
  const itkVector<T, TVectorDimension>
  operator+(const itkVector<T, TVectorDimension> &vec)
    {
    itkVector<T, TVectorDimension> result;
    for (int i=0; i < TVectorDimension; i++)
      { result = m_Vector[i] + vec.m_Vector[i]; }
    return result;
    }

  /**
   * Add a vector to this vector, modifying the value of this vector.
   */
  const itkVector<T, TVectorDimension> &
  operator+=(const itkVector<T, TVectorDimension> &vec)
    {
    for (int i=0; i < TVectorDimension; i++)
      { m_Vector[i] += vec.m_Vector[i]; }
    return *this;
    }

  /**
   * Subtract a vector from this vector and return a new vector.
   */
  const itkVector<T, TVectorDimension>
  operator-(const itkVector<T, TVectorDimension> &vec)
    {
    itkVector<T, TVectorDimension> result;
    for (int i=0; i < TVectorDimension; i++)
      { result = m_Vector[i] - vec.m_Vector[i]; }
    return result;
    }

  /**
   * Subtract a vector from this vector, modifying the value of this vector.
   */
  const itkVector<T, TVectorDimension> &
  operator-=(const itkVector<T, TVectorDimension> &vec)
    {
    for (int i=0; i < TVectorDimension; i++)
      { m_Vector[i] -= vec.m_Vector[i]; }
    return *this;
    }


 private:
  T m_Vector[TVectorDimension];
};

}
#endif 
