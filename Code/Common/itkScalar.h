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

/**
 * itkScalar is a templated class that holds a single scalar value.  itkScalar
 * can be used as the data type held at each pixel in an itkImage or at each
 * vertex of an itkMesh. The template parameter can be any data type that
 * behaves like a primitive (or atomic) data type (int, short, float, complex).
 * itk filters that rely on scalar data assume the data type held at each
 * pixel or each vertex responds to GetScalar()/SetScalar() methods. If not,
 * a compile time error will occur.
 *
 * For efficiency sake, itkScalar does not define a default constructor, a
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

#ifndef __itkScalar_h
#define __itkScalar_h

template<class T>
class itkScalar {
 public:
  /**
   * scalar_value_type can be used to declare a variable that is the same type
   * as the data held in an itkScalar.  The naming convention here follows
   * STL conventions. scalar_value_type is what STL would call value_type.
   * scalar_value_type is used instead of STL's value_type to avoid confusion
   * when a data type holds a scalar and a vector of different types.
   * \sa itkScalarVector
   */
  typedef T scalar_value_type;

  /**
   * Get the scalar value.
   * \sa SetScalar
   */
  T GetScalar() const { return m_Scalar; };

  /**
   * Set the scalar value.
   * \sa GetScalar
   */
  void SetScalar(const T &val) { m_Scalar = val; };

 private:
  T m_Scalar;
};

#endif 
