/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkIndex_h
#define __itkIndex_h

#include "itkMacro.h"
#include "itkOffset.h"
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
 *
 * \ingroup ImageAccess
 * \ingroup ImageObjects
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
   * Compatible Size typedef
   */
  typedef   Size<VIndexDimension>  SizeType;

  /**
   * Compatible Offset typedef
   */
  typedef   Offset<VIndexDimension>  OffsetType;

  /**
   * Add a size to an index. This method models a random access Index.
   */
  const Self
  operator+(const SizeType &size) const
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
  operator+=(const SizeType &size)
    {
    for (unsigned int i=0; i < VIndexDimension; i++)
      { m_Index[i] += size[i]; }
    return *this;
    }

  /**
   * Add an offset to an index.
   */
  const Self
  operator+(const OffsetType &offset) const
    {
    Self result;
    for (unsigned int i=0; i < VIndexDimension; i++)
      { result[i] = m_Index[i] + offset[i]; }
    return result;
    }

  /**
   * Increment index by an offset. This method models a random access Index.
   */
  const Self &
  operator+=(const OffsetType &offset)
    {
    for (unsigned int i=0; i < VIndexDimension; i++)
      { m_Index[i] += offset[i]; }
    return *this;
    }

  /**
   * Decrement index by an offset. This method models a random access Index.
   */
  const Self &
  operator-=(const OffsetType &offset)
    {
    for (unsigned int i=0; i < VIndexDimension; i++)
      { m_Index[i] -= offset[i]; }
    return *this;
    }

  /**
   * Subtract an offset from an index.
   */
  const Self
  operator-(const OffsetType &off) const
    {
    Self result;
    for (unsigned int i=0; i < VIndexDimension; i++)
      { result[i] = m_Index[i] - off.m_Offset[i]; }
    return result;
    }

  /**
   * Subtract two indices. This method models a random access Index.
   */
  const OffsetType
  operator-(const Self &vec)
    {
    OffsetType result;
    for (unsigned int i=0; i < VIndexDimension; i++)
      { result[i] = m_Index[i] - vec.m_Index[i]; }
    return result;
    }

  /**
   * Multiply an index by a size (elementwise product). This method 
   * models a random access Index. 
   */
  const Self
  operator*(const SizeType &vec)
    {
    Self result;
    for (unsigned int i=0; i < VIndexDimension; i++)
      { result[i] = m_Index[i] * vec.m_Size[i]; }
    return result;
    }

  /**
   * Compare two indices.
   */
  bool
  operator==(const Self &vec) const
    {
    bool same=true;
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
    bool same=true;
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
  void SetIndex(const unsigned long val[VIndexDimension])
    { memcpy(m_Index, val, sizeof(unsigned long)*VIndexDimension); }

  /**
   * Return a basis vector of the form [0, ..., 0, 1, 0, ... 0] where the "1"
   * is positioned in the location specified by the parameter "dim". Valid
   * values of "dim" are 0, ..., VIndexDimension-1.
   */
  static Self GetBasisIndex(unsigned int dim); 

 /**
   * Set one value for the index in all dimensions.  Useful for initializing
   * an offset to zero.
   */
  void Fill(long value)
    { for(unsigned int i=0;i < VIndexDimension; ++i) m_Index[i] = value; }

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
  
  memset(ind.m_Index, 0, sizeof(unsigned long)*VIndexDimension);
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
::ZeroIndex = {{0}};

} // end namespace itk

#endif 
