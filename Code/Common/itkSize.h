/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSize.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkSize_h
#define __itkSize_h

#include "itkMacro.h"

#include <memory>

#include "itkExceptionObject.h"

namespace itk
{

/** 
 * \class Size
 * \brief Represent the size (bounds) of a n-dimensional image.
 *
 * Size is a templated class to represent multi-dimensional array bounds
 * Size is templated over the dimension. itk assumes the first element of
 * Size is the fastest moving index.
 *
 * For efficiency sake, Size does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * Size is an "aggregate" class.  Its data is public (m_Size)
 * allowing for fast and convienent instantiations/assignments.
 *
 * The following syntax for assigning a size is allowed/suggested:
 *    Size<3> size = {256, 256, 20};
 *
 * \sa Index
 */


template<unsigned int VDimension=2>
class Size {
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Size  Self;
  
  /**
   * Get the dimension of the size object.
   */
  static unsigned int GetSizeDimension() { return VDimension; }

  /**
   * Add two sizes. 
   */
  const Self
  operator+(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < VDimension; i++)
      { result[i] = m_Size[i] + vec.m_Size[i]; }
    return result;
    }

  /**
   * Increment size by a size. 
   */
  const Self &
  operator+=(const Self &vec)
    {
    for (unsigned int i=0; i < VDimension; i++)
      { m_Size[i] += vec.m_Size[i]; }
    return *this;
    }

  /**
   * Subtract two sizes. 
   */
  const Self
  operator-(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < VDimension; i++)
      { result[i] = m_Size[i] - vec.m_Size[i]; }
    return result;
    }

  /**
   * Decrement size by a size. 
   */
  const Self &
  operator-=(const Self &vec)
    {
    for (unsigned int i=0; i < VDimension; i++)
      { m_Size[i] -= vec.m_Size[i]; }
    return *this;
    }

  /**
   * Multiply two sizes (elementwise product). 
   */
  const Self
  operator*(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < VDimension; i++)
      { result[i] = m_Size[i] * vec.m_Size[i]; }
    return result;
    }

  /**
   * Multiply two sizes (elementwise product). 
   */
  const Self &
  operator*=(const Self &vec)
    {
    for (unsigned int i=0; i < VDimension; i++)
      { m_Size[i] *= vec.m_Size[i]; }
    return *this;
    }

  /**
   * Compare two sizes.
   */
  bool
  operator==(const Self &vec) const
    {
    bool same=1;
    for (unsigned int i=0; i < VDimension && same; i++)
      { same = (m_Size[i] == vec.m_Size[i]); }
    return same;
    }

  /**
   * Compare two sizes.
   */
  bool
  operator!=(const Self &vec) const
    {
    bool same=1;
    for (unsigned int i=0; i < VDimension && same; i++)
      { same = (m_Size[i] == vec.m_Size[i]); }
    return !same;
    }

  /**
   * Access an element of the size. Elements are numbered
   * 0, ..., VDimension-1. No bounds checking is performed.
   */
  unsigned long & operator[](unsigned int dim)
    { return m_Size[dim]; }

  /**
   * Access an element of the size. Elements are numbered
   * 0, ..., VDimension-1. This version can only be an rvalue.
   * No bounds checking is performed.
   */
  unsigned long operator[](unsigned int dim) const
    { return m_Size[dim]; }

  /**
   * Get the size. This provides a read only reference to the size.
   * \sa SetSize
   */
  const unsigned long *GetSize() const { return m_Size; };

  /**
   * Set the size.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   * \sa GetSize
   */
  void SetSize(const unsigned long val[VDimension])
    { memcpy(m_Size, val, sizeof(unsigned long)*VDimension); }

  /**
   * Size is an "aggregate" class.  Its data is public (m_Size)
   * allowing for fast and convenient instantiations/assignments.
   *
   * The following syntax for assigning a size is allowed/suggested:
   *
   * Size<3> size = {{256, 256, 20}};
   *
   * The doubled braces {{ and }} are required to prevent `gcc -Wall'
   * (and perhaps other compilers) from complaining about a partly
   * bracketed initializer.
   **/
  unsigned long m_Size[VDimension];
  
public:

private:
};


template<unsigned int VDimension>
std::ostream & operator<<(std::ostream &os, const Size<VDimension> &size)
{
  os << "[";
  for (unsigned int i=0; i < VDimension - 1; ++i)
    {
    os << size[i] << ", ";
    }
  if (VDimension >= 1)
    {
    os << size[VDimension-1];
    }
  os << "]" << std::endl;
  return os;
}

} // end namespace itk

#endif 
