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
 * itkScalarVector is a templated class that holds a scalar value plus an
 * array of values (a vector).  itkScalarVector can be used as the data type
 * held at each pixel in an itkImage or at each vertex of an itkMesh. There
 * are three template parameters: the type of scalar, the type of vector, and
 * the number of components of the vector. The types can be any data type
 * that behaves like a primitive (or atomic) data type (int, short, float,
 * complex).  itk filters that rely on scalar data assume the data type held
 * at each pixel or each vertex responds to GetScalar()/SetScalar()
 * methods. itk filters that rely on vector data assume the data type held at
 * each pixel or each vertex responds to GetVector()/SetVector() methods. If
 * not, a compile time error will occur.
 *
 * itkScalarVector is not a dynamically extendible array like std::vector. It
 * is intended to be used like a mathematical vector.
 *
 * If you wish a simpler pixel types, you can use itkScalar, which represents
 * a single data value at a pixel. YOu can also use itkVector, which supports
 * (for a given pixel) an array of vector values.
 * 
 * \sa itkImage
 * \sa itkMesh
 * \sa itkScalar
 * \sa itkVector 
 */

#ifndef __itkScalarVector_h
#define __itkScalarVector_h

#include <memory.h>

template<class TScalar, class TVector, unsigned int TVectorDimension=3>
class itkScalarVector {
public:
  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in the scalar portion of the itkScalarVector.  
   */
  typedef typename TScalar ValueType;

  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in the scalar portion of the itkScalarVector.  
   */
  typedef typename TScalar ScalarValueType;

  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in the scalar portion of the itkScalarVector.  
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

#endif 
