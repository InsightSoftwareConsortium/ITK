/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarVector.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
/**
 * ScalarVector is a templated class that holds a scalar value plus an
 * array of values (a vector).  ScalarVector can be used as the data type
 * held at each pixel in an Image or at each vertex of an Mesh. There
 * are three template parameters: the type of scalar, the type of vector, and
 * the number of components of the vector. The types can be any data type
 * that behaves like a primitive (or atomic) data type (int, short, float,
 * complex).  itk filters that rely on scalar data assume the data type held
 * at each pixel or each vertex responds to GetScalar()/SetScalar()
 * methods. itk filters that rely on vector data assume the data type held at
 * each pixel or each vertex responds to GetVector()/SetVector() methods. If
 * not, a compile time error will occur.
 *
 * ScalarVector is not a dynamically extendible array like std::vector. It
 * is intended to be used like a mathematical vector.
 *
 * If you wish a simpler pixel types, you can use Scalar, which represents
 * a single data value at a pixel. YOu can also use Vector, which supports
 * (for a given pixel) an array of vector values.
 * 
 * \sa Image
 * \sa Mesh
 * \sa Scalar
 * \sa Vector 
 */

#ifndef __itkScalarVector_h
#define __itkScalarVector_h

#include <memory.h>

namespace itk
{

template<class TScalar, class TVector, unsigned int TVectorDimension=3>
class ScalarVector {
public:
  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in the scalar portion of the ScalarVector.  
   */
  typedef typename TScalar ValueType;

  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in the scalar portion of the ScalarVector.  
   */
  typedef typename TScalar ScalarValueType;

  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in the scalar portion of the ScalarVector.  
   */
  typedef typename TVector VectorValueType;

  /**
   * Get the scalar value.
   * \sa SetScalar()
   */
  TScalar GetScalar() const 
    { return m_Scalar; }

  /**
   * Set the scalar value.
   * \sa GetScalar()
   */
  void SetScalar(const TScalar &val) 
    { m_Scalar = val; }

  /**
   * Get the dimension (size) of the vector.
   */
  static unsigned int GetVectorDimension() 
    { return TVectorDimension; }
  
  /**
   * Get the vector. This provides a read only reference to the vector.
   * \sa SetVector().
   */
  const TVector *GetVector() const 
    { return m_Vector; }

  /**
   * Set the vector.
   * \sa GetVector().
   */
  void SetVector(const TVector *val)
    { memcpy(m_Vector, val, sizeof(T)*TVectorDimension); };

private:
  TScalar m_Scalar;
  TVector m_Vector[TVectorDimension];
};

  
} // namespace itk
  
#endif 
