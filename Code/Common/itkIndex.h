/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
  ==========================================================================*/

/**
 * itkIndex is a templated class that holds a single index.  itkIndex
 * can be used as the data type held at each pixel in an itkImage or at each
 * vertex of an itkMesh. The template parameter can be any data type that
 * behaves like a primitive (or atomic) data type (int, short, float, complex).
 * itk filters that rely on index data assume the data type held at each
 * pixel or each vertex responds to GetIndex()/SetIndex() methods. If not,
 * a compile time error will occur.
 *
 * itkIndex is not a dynamically extendible array like std::index. It is
 * intended to be used like a mathematical index.
 *
 * For efficiency sake, itkIndex does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 */

// To compile / test this class
// Windows: cl itkDataTypeTest.cxx; .\itkDataTypeTest.exe
// linux:   c++ itkDataTypeTest.cxx; ./a.out
// other:   CCcompiler itkDataTypeTest.cxx;  ./a.out

#ifndef __itkIndex_h
#define __itkIndex_h

#include <memory.h>

template<unsigned int TIndexDimension=3>
class itkIndex {
 public:
  /**
   * Get the dimension (size) of the index.
   */
  static unsigned int GetIndexDimension() { return TIndexDimension; }
  
  /**
   * Get the index. This provides a read only reference to the index.
   * \sa SetIndex
   */
  const unsigned long *GetIndex() const { return m_Index; };

  /**
   * Set the index.
   * \sa GetIndex
   */
  void SetIndex(const unsigned long  *val)
  { memcpy(m_Index, val, sizeof(unsigned long)*TIndexDimension); };


 private:
  unsigned long m_Index[TIndexDimension];
};


#endif 
