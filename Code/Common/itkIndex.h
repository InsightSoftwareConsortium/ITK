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
 * Index is a templated class to represent a multi-dimensional index.  
 * Index is templated over the dimension of the index.
 *
 * For efficiency sake, Index does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * Should there be an BoundedIndex to handle bounds checking? Or should
 * there be an API to perform bounded increments in the iterator.
 */


#ifndef __itkIndex_h
#define __itkIndex_h

#include <memory>

#include "itkExceptionObject.h"

namespace itk
{

template<unsigned int TIndexDimension=2>
class Index {
public:
  typedef Index  Self;
  
  /**
   * Get the dimension (size) of the index.
   */
  static unsigned int GetIndexDimension() { return TIndexDimension; }

  /**
   * Add two indices. This method models a random access Index.
   */
  const Self
  operator+(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < TIndexDimension; i++)
      { result[i] = m_Index[i] + vec.m_Index[i]; }
    return result;
    }

  /**
   * Increment index by an index. This method models a random access Index.
   */
  const Self &
  operator+=(const Self &vec)
    {
    for (unsigned int i=0; i < TIndexDimension; i++)
      { m_Index[i] += vec.m_Index[i]; }
    return *this;
    }

  /**
   * Subtract two indices. This method models a random access Index.
   */
  const Self
  operator-(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < TIndexDimension; i++)
      { result[i] = m_Index[i] - vec.m_Index[i]; }
    return result;
    }

  /**
   * Decrement index by an index. This method models a random access Index.
   */
  const Self &
  operator-=(const Self &vec)
    {
    for (unsigned int i=0; i < TIndexDimension; i++)
      { m_Index[i] -= vec.m_Index[i]; }
    return *this;
    }

  /**
   * Access an element of the index. Elements are numbered
   * 0, ..., TIndexDimension-1. No bounds checking is performed.
   */
  unsigned long & operator[](unsigned int dim)
    { return m_Index[dim]; }
  

  /**
   * Get the index. This provides a read only reference to the index.
   * \sa SetIndex
   */
  const unsigned long *GetIndex() const { return m_Index; };

  /**
   * Set the index.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   * \sa GetIndex
   */
  void SetIndex(const long val[TIndexDimension])
    { memcpy(m_Index, val, sizeof(long)*TIndexDimension); }

  /**
   * Return a basis vector of the form [0, ..., 0, 1, 0, ... 0] where the "1"
   * is positioned in the location specified by the parameter "dim". Valid
   * values of "dim" are 0, ..., TIndexDimension-1.
   *
   * This routine will throw an exception (InvalidDimension) if
   * "dim" > dimension of the index.
   */
  static Self GetBasisIndex(unsigned int dim); 

public:
  virtual void PrintSelf(std::ostream& os, Indent indent);

private:
  unsigned long m_Index[TIndexDimension];
};

template<unsigned int TIndexDimension>
void 
Index<TIndexDimension>
::PrintSelf(std::ostream& os, Indent indent)
{
  unsigned int i;
  
  os << "[";
  for (i=0; i < TIndexDimension - 1; i++)
    {
    os << m_Index[i] << ", ";
    }
  os << m_Index[i] << "] ";
}

template<unsigned int TIndexDimension>
Index<TIndexDimension> 
Index<TIndexDimension>
::GetBasisIndex(unsigned int dim)
{
  if (dim >= TIndexDimension)
    {
    throw InvalidDimension;
    }
  
  Self ind;
  
  memset(ind.m_Index, 0, sizeof(long)*TIndexDimension);
  ind.m_Index[dim] = 1;
  return ind;
}

} // namespace itk

#endif 
