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
 * itkScalar is a templated class that holds a single scalar value.
 * itkScalar can be used as the data type held at each pixel in an itkImage
 * or at each vertex (or cell) of an itkMesh. The template parameter can be
 * any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  itk filters that rely on scalar data assume the
 * data type held at each pixel or each vertex responds to
 * GetScalar()/SetScalar() methods. If not, a compile time error will occur.
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
   * ValueType can be used to declare a variable that is the same type
   * as the data held in an itkScalar.
   */
  typedef typename T ValueType;

  /**
   * Get the dimension (size) of this scalar. The method is necessary
   * to satisfy the pixel manipulation API.
   */
  static unsigned int GetPixelDimension() 
    { return 1; }
  
  /**
   * Get the scalar value.
   * \sa SetScalar
   */
  T GetScalar() const 
    { return m_Scalar; }

  /**
   * Set the scalar value.
   * \sa GetScalar
   */
  void SetScalar(const T &val) { m_Scalar = val; }

 private:
  T m_Scalar;
};

#endif 
