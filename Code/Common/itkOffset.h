/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOffset.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOffset_h
#define __itkOffset_h

#include "itkMacro.h"
#include "itkSize.h"

#include <memory>

#include "itkExceptionObject.h"

namespace itk
{

/** 
 * \class Offset
 * \brief Represent the offset between two n-dimensional indexes
 *  in a n-dimensional image.
 *
 * Offset is a templated class to represent a multi-dimensional offset,
 * i.e. (i,j,k,...). Offset is templated over the dimension of the space.  
 *
 * For the sake of efficiency, Size does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * \sa Index
 * \ingroup ImageAccess
 */


template<unsigned int VOffsetDimension=2>
class Offset {
public:
  /** Standard class typedefs. */
  typedef Offset  Self;
  
  /** Get the dimension (size) of the index. */
  static unsigned int GetOffsetDimension() { return VOffsetDimension; }

  /** Compatible offset typedefs. */
  typedef   Offset<VOffsetDimension>  OffsetType;
  typedef   long OffsetValueType;
    
  /** Add an offset to an offset. */
  const Self
  operator+(const Self &offset) const
    {
    Self result;
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { result[i] = m_Offset[i] + offset[i]; }
    return result;
    }

  /** Add a size to an offset.  */
  const Self
  operator+(const Size<VOffsetDimension> &size) const
    {
    Self result;
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { result[i] = m_Offset[i] + size[i]; }
    return result;
    }

  /** Increment index by a size.  */
  const Self &
  operator+=(const Size<VOffsetDimension> &size)
    {
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { m_Offset[i] += size[i]; }
    return *this;
    }

  /** Decrement index by a size.  */
  const Self &
  operator-=(const Size<VOffsetDimension> &size)
    {
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { m_Offset[i] -= size[i]; }
    return *this;
    }

  /** Subtract two offsets. */
  const Self
  operator-(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { result[i] = m_Offset[i] - vec.m_Offset[i]; }
    return result;
    }

  /** Increment offset by an offset.  */
  const Self &
  operator+=(const Self &vec)
    {
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { m_Offset[i] += vec.m_Offset[i]; }
    return *this;
    }

  /** Decrement offset by an offset.  */
  const Self &
  operator-=(const Self &vec)
    {
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { m_Offset[i] -= vec.m_Offset[i]; }
    return *this;
    }

  /** Compare two offsets. */
  bool
  operator==(const Self &vec) const
    {
    bool same=1;
    for (unsigned int i=0; i < VOffsetDimension && same; i++)
      { same = (m_Offset[i] == vec.m_Offset[i]); }
    return same;
    }

  /** Compare two offsets. */
  bool
  operator!=(const Self &vec) const
    {
    bool same=1;
    for (unsigned int i=0; i < VOffsetDimension && same; i++)
      { same = (m_Offset[i] == vec.m_Offset[i]); }
    return !same;
    }

  /** Access an element of the offset. Elements are numbered
   * 0, ..., VOffsetDimension-1. No bounds checking is performed. */
  OffsetValueType & operator[](unsigned int dim)
    { return m_Offset[dim]; }

  /** Access an element of the index. Elements are numbered
   * 0, ..., VOffsetDimension-1. This version can only be an rvalue.
   * No bounds checking is performed. */
  OffsetValueType operator[](unsigned int dim) const
    { return m_Offset[dim]; }

  /** Get the index. This provides a read only reference to the index.
   * \sa SetOffset() */
  const OffsetValueType *GetOffset() const { return m_Offset; };

  /** Set the index.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   * \sa GetOffset() */
  void SetOffset(const OffsetValueType val[VOffsetDimension])
    { memcpy(m_Offset, val, sizeof(OffsetValueType)*VOffsetDimension); }

  /** Return a basis vector of the form [0, ..., 0, 1, 0, ... 0] where the "1"
   * is positioned in the location specified by the parameter "dim". Valid
   * values of "dim" are 0, ..., VOffsetDimension-1. */
  static Self GetBasisOffset(unsigned int dim); 

  /** Set one value for the offset in all dimensions.  Useful for initializing
   * an offset to zero. */
  void Fill(OffsetValueType value)
    { for(unsigned int i=0;i < VOffsetDimension; ++i) m_Offset[i] = value; }
  
  /** Offset is an "aggregate" class.  Its data is public (m_Offset)
   * allowing for fast and convienent instantiations/assignments.
   *
   * The following syntax for assigning an index is allowed/suggested:
   *    Offset<3> index = {5, 2, 7}; */
  OffsetValueType m_Offset[VOffsetDimension];

};


template<unsigned int VOffsetDimension>
Offset<VOffsetDimension> 
Offset<VOffsetDimension>
::GetBasisOffset(unsigned int dim)
{
  Self ind;
  
  memset(ind.m_Offset, 0, sizeof(OffsetValueType)*VOffsetDimension);
  ind.m_Offset[dim] = 1;
  return ind;
}

template<unsigned int VOffsetDimension>
std::ostream & operator<<(std::ostream &os, const Offset<VOffsetDimension> &ind)
{
  os << "[";
  for (unsigned int i=0; i < VOffsetDimension - 1; ++i)
    {
    os << ind[i] << ", ";
    }
  if (VOffsetDimension >= 1)
    {
    os << ind[VOffsetDimension-1];
    }
  os << "]";
  return os;
}

} // end namespace itk

#endif 
