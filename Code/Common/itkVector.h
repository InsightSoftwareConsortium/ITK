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
#include "vnl/vnl_vector.txx"
#include "vnl/vnl_vector_fixed.txx"
#include "vnl/vnl_c_vector.txx"

#include <memory.h>

namespace itk
{

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
   * as a data element held in an Vector.  
   */
  typedef T ValueType;

  /**
   * VectorValueType can be used to declare a variable that is the same type
   * as a data element held in an Vector.  
   */
  typedef T VectorValueType;
  
  /**
   * VectorType can be used to declare a variable that is the same type
   * as the internal vector.  
   */
  typedef vnl_vector_fixed<T, TVectorDimension> VectorType;

  /**
   * Get the dimension (size) of the vector.
   */
  static unsigned int GetVectorDimension() 
    { return TVectorDimension; }
  
  /**
   * Get the vector. This provides a read only reference to the vector.
   * \sa SetVector
   */
  const VectorType &GetVector() const 
    { return m_Vector; }

  /**
   * Get the vector. This provides a read/write reference to the vector.
   * \sa SetVector
   */
  VectorType &GetVector()  
    { return m_Vector; }
  
  /**
   * Set the vector. 
   * \sa GetVector
   */
  void SetVector(const VectorType &vec)
    { m_Vector = vec; }


  /**
   * Operator=.  Assign a vector to a vector.
   */
  Self& operator=(const Self& vec)
  { m_Vector = vec.m_Vector; return *this; }

  Self& operator=(const VectorType &vec)
  { m_Vector = vec; return *this; }

  /**
   * Scalar operator=.  Sets all elements of the vector to the same value.
   */
  Self& operator=(const VectorValueType &value)
  { m_Vector = value; return *this; }

  /**
   * Scalar operator+=.  Adds a scalar to all elements.
   */
  Self& operator+=(const VectorValueType &value)
  { m_Vector += value; return *this; };

  /**
   * Scalar operator-=.  Subtracts a scalar to all elements.
   */
  Self& operator-=(const VectorValueType &value)
  { m_Vector -= value; return *this; };
  
  /**
   * Scalar operator*=.  Scales elements by a scalar.
   */
  Self& operator*=(const VectorValueType &value)
  { m_Vector *= value; return *this; };

  /**
   * Scalar operator/=.  Scales (divides) elements by a scalar.
   */
  Self& operator/=(const VectorValueType &value)
  { m_Vector /= value; return *this; };



  /**
   * Vector operator+=.  Adds a vectors to the current vector.
   */
  Self& operator+=(const Self &vec)
  { m_Vector += vec.m_Vector; return *this; };

  Self& operator+=(const VectorType &vec)
  { m_Vector += vec; return *this; };

  /**
   * Vector operator-=.  Subtracts a vector from a current vector.
   */
  Self& operator-=(const Self &vec)
  { m_Vector -= vec.m_Vector; return *this; };

  Self& operator-=(const VectorType &vec)
  { m_Vector -= vec; return *this; };
  


  /**
   * Vector negation.  Negate all the elements of a vector. Return a new vector
   */
  Self operator-() const
  { Self vec(*this); vec *= -1; return vec; }
  
  /**
   * Vector addition. Add two vectors. Return a new vector.
   */
  Self operator+(const Self &vec) const
  { Self result(*this); result.m_Vector += vec.m_Vector; return result; }
  
  Self operator+(const VectorType &vec) const
  { Self result(*this); result.m_Vector += vec; return result; }

  /**
   * Vector subtraction. Subtract two vectors. Return a new vector.
   */
  Self operator-(const Self &vec) const
  { Self result(*this); result.m_Vector -= vec.m_Vector; return result; }
  
  Self operator-(const VectorType &vec) const
  { Self result(*this); result.m_Vector -= vec; return result; }
  
  /**
   * Scalar operator+. Add a scalar to all elements of a vector. Return
   * a new vector.
   */
  Self operator+(const VectorValueType& val) const
  { Self result(*this); result.m_Vector += val; return result; }
    
  /**
   * Scalar operator-. Subtract a scalar from all elements of a vector.
   * Return a new vector.
   */
  Self operator-(const VectorValueType& val) const
  { Self result(*this); result.m_Vector -= val; return result; }

  /**
   * Scalar operator*. Scale the elements of a vector by a scalar.
   * Return a new vector.
   */
  Self operator*(const VectorValueType& val) const
  { Self result(*this); result.m_Vector *= val; return result; }
  
  /**
   * Scalar operator/. Scale (divide) the elements of a vector by a scalar.
   * Return a new vector.
   */
  Self operator/(const VectorValueType& val) const
  { Self result(*this); result.m_Vector /= val; return result; }
  

  /**
   * Access an element of a vector. This version can be used as an lvalue.
   */
  VectorValueType& operator[] (unsigned int i)
  { return m_Vector[i]; }

  /**
   * Access an element of a vector. This version can only a rvalue.
   */
  VectorValueType operator[] (unsigned int i) const
  { return m_Vector[i]; }

  
  private:
    VectorType       m_Vector;
};

  
} // end namespace itk

#endif 
