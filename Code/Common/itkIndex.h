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
#ifndef __itkIndex_h
#define __itkIndex_h

#include "itkMacro.h"
#include "itkSize.h"

#include <memory>

#include "itkExceptionObject.h"

namespace itk
{

/** 
 * \class Index
 * \brief Represent a n-dimensional index in a n-dimensional image.
 *
 * Index is a templated class to represent a multi-dimensional index,
 * i.e. (i,j,k,...). Index is templated over the dimension of the index.  
 * ITK assumes the first element of an index is the fastest moving index.
 *
 * For efficiency sake, Index does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * Index is an "aggregate" class.  Its data is public (m_Index)
 * allowing for fast and convenient instantiations/assignments.
 *
 * The following syntax for assigning an index is allowed/suggested:
 *
 *    Index<3> index = {{5, 2, 7}};
 *
 * The double braces {{ and }} are needed to prevent a compiler warning
 * about a partly bracketed initializer.
 *
 * \remark
 * Should there be an itkBoundedIndex to handle bounds checking? Or should
 * there be an API to perform bounded increments in the iterator.  
 */


template<unsigned int VIndexDimension=2>
class Index {
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Index  Self;
  
  /**
   * Get the dimension (size) of the index.
   */
  static unsigned int GetIndexDimension() { return VIndexDimension; }

  /**
   * Define the zero index for convenience.
   */
  static const Self ZeroIndex;
  
  /**
   * Add two indices. This method models a random access Index.
   */
  const Self
  operator+(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < VIndexDimension; i++)
      { result[i] = m_Index[i] + vec.m_Index[i]; }
    return result;
    }

  /**
   * Increment index by an index. This method models a random access Index.
   */
  const Self &
  operator+=(const Self &vec)
    {
    for (unsigned int i=0; i < VIndexDimension; i++)
      { m_Index[i] += vec.m_Index[i]; }
    return *this;
    }

  /**
   * Add a size to an index. This method models a random access Index.
   */
  const Self
  operator+(const Size<VIndexDimension> &size)
    {
    Self result;
    for (unsigned int i=0; i < VIndexDimension; i++)
      { result[i] = m_Index[i] + size[i]; }
    return result;
    }

  /**
   * Increment index by a size. This method models a random access Index.
   */
  const Self &
  operator+=(const Size<VIndexDimension> &size)
    {
    for (unsigned int i=0; i < VIndexDimension; i++)
      { m_Index[i] += size[i]; }
    return *this;
    }

  /**
   * Subtract two indices. This method models a random access Index.
   */
  const Self
  operator-(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < VIndexDimension; i++)
      { result[i] = m_Index[i] - vec.m_Index[i]; }
    return result;
    }

  /**
   * Decrement index by an index. This method models a random access Index.
   */
  const Self &
  operator-=(const Self &vec)
    {
    for (unsigned int i=0; i < VIndexDimension; i++)
      { m_Index[i] -= vec.m_Index[i]; }
    return *this;
    }

  /**
   * Multiply two indices (elementwise product). This method models a random
   * access Index. 
   */
  const Self
  operator*(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < VIndexDimension; i++)
      { result[i] = m_Index[i] * vec.m_Index[i]; }
    return result;
    }

  /**
   * Multiply two indices (elementwise product). This method models a random
   * access Index. 
   */
  const Self &
  operator*=(const Self &vec)
    {
    for (unsigned int i=0; i < VIndexDimension; i++)
      { m_Index[i] *= vec.m_Index[i]; }
    return *this;
    }

  /**
   * Compare two indices.
   */
  bool
  operator==(const Self &vec) const
    {
    bool same=1;
    for (unsigned int i=0; i < VIndexDimension && same; i++)
      { same = (m_Index[i] == vec.m_Index[i]); }
    return same;
    }

  /**
   * Compare two indices.
   */
  bool
  operator!=(const Self &vec) const
    {
    bool same=1;
    for (unsigned int i=0; i < VIndexDimension && same; i++)
      { same = (m_Index[i] == vec.m_Index[i]); }
    return !same;
    }

  /**
   * Access an element of the index. Elements are numbered
   * 0, ..., VIndexDimension-1. No bounds checking is performed.
   */
  long & operator[](unsigned int dim)
    { return m_Index[dim]; }

  /**
   * Access an element of the index. Elements are numbered
   * 0, ..., VIndexDimension-1. This version can only be an rvalue.
   * No bounds checking is performed.
   */
  long operator[](unsigned int dim) const
    { return m_Index[dim]; }

  /**
   * Get the index. This provides a read only reference to the index.
   * \sa SetIndex()
   */
  const long *GetIndex() const { return m_Index; };

  /**
   * Set the index.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   * \sa GetIndex()
   */
  void SetIndex(const long val[VIndexDimension])
    { memcpy(m_Index, val, sizeof(long)*VIndexDimension); }

  /**
   * Return a basis vector of the form [0, ..., 0, 1, 0, ... 0] where the "1"
   * is positioned in the location specified by the parameter "dim". Valid
   * values of "dim" are 0, ..., VIndexDimension-1.
   */
  static Self GetBasisIndex(unsigned int dim); 

  /**
   * Index is an "aggregate" class.  Its data is public (m_Index)
   * allowing for fast and convienent instantiations/assignments.
   *
   * The following syntax for assigning an index is allowed/suggested:
   *    Index<3> index = {5, 2, 7};
   */
  long m_Index[VIndexDimension];
  
public:

private:
};


template<unsigned int VIndexDimension>
Index<VIndexDimension> 
Index<VIndexDimension>
::GetBasisIndex(unsigned int dim)
{
  Self ind;
  
  memset(ind.m_Index, 0, sizeof(long)*VIndexDimension);
  ind.m_Index[dim] = 1;
  return ind;
}

template<unsigned int VIndexDimension>
std::ostream & operator<<(std::ostream &os, const Index<VIndexDimension> &ind)
{
  os << "[";
  for (unsigned int i=0; i < VIndexDimension - 1; ++i)
    {
    os << ind[i] << ", ";
    }
  if (VIndexDimension >= 1)
    {
    os << ind[VIndexDimension-1];
    }
  os << "]" << std::endl;
  return os;
}

// Set the const definition of the ZeroIndex. This uses the aggregate
// initialization shortcut to assign all the data in the aggregate to zero.
template<unsigned int VIndexDimension>
const Index<VIndexDimension>
Index<VIndexDimension>
::ZeroIndex = {0};

} // end namespace itk

#endif 
