/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOffset.h
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
 * \sa Index
 * \ingroup ImageAccess
 */


template<unsigned int VOffsetDimension=2>
class Offset {
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Offset  Self;
  
  /**
   * Get the dimension (size) of the index.
   */
  static unsigned int GetOffsetDimension() { return VOffsetDimension; }

  /**
   * Compatible Offset typedef
   */
  typedef   Offset<VOffsetDimension>  OfficeType;
  
  /**
   * Add an offset to an offset.
   */
  const Self
  operator+(const Self &offset) const
    {
    Self result;
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { result[i] = m_Offset[i] + offset[i]; }
    return result;
    }

  /**
   * Add a size to an offset. 
   */
  const Self
  operator+(const Size<VOffsetDimension> &size) const
    {
    Self result;
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { result[i] = m_Offset[i] + size[i]; }
    return result;
    }

  /**
   * Increment index by a size. 
   */
  const Self &
  operator+=(const Size<VOffsetDimension> &size)
    {
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { m_Offset[i] += size[i]; }
    return *this;
    }

  /**
   * Subtract two offsets.
   */
  const Self
  operator-(const Self &vec)
    {
    Self result;
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { result[i] = m_Offset[i] - vec.m_Offset[i]; }
    return result;
    }

  /**
   * Decrement offset by an offset. 
   */
  const Self &
  operator-=(const Self &vec)
    {
    for (unsigned int i=0; i < VOffsetDimension; i++)
      { m_Offset[i] -= vec.m_Offset[i]; }
    return *this;
    }

  /**
   * Compare two offsets.
   */
  bool
  operator==(const Self &vec) const
    {
    bool same=1;
    for (unsigned int i=0; i < VOffsetDimension && same; i++)
      { same = (m_Offset[i] == vec.m_Offset[i]); }
    return same;
    }

  /**
   * Compare two offsets.
   */
  bool
  operator!=(const Self &vec) const
    {
    bool same=1;
    for (unsigned int i=0; i < VOffsetDimension && same; i++)
      { same = (m_Offset[i] == vec.m_Offset[i]); }
    return !same;
    }

  /**
   * Access an element of the offset. Elements are numbered
   * 0, ..., VOffsetDimension-1. No bounds checking is performed.
   */
  long & operator[](unsigned int dim)
    { return m_Offset[dim]; }

  /**
   * Access an element of the index. Elements are numbered
   * 0, ..., VOffsetDimension-1. This version can only be an rvalue.
   * No bounds checking is performed.
   */
  long operator[](unsigned int dim) const
    { return m_Offset[dim]; }

  /**
   * Get the index. This provides a read only reference to the index.
   * \sa SetOffset()
   */
  const long *GetOffset() const { return m_Offset; };

  /**
   * Set the index.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   * \sa GetOffset()
   */
  void SetOffset(const long val[VOffsetDimension])
    { memcpy(m_Offset, val, sizeof(long)*VOffsetDimension); }

  /**
   * Return a basis vector of the form [0, ..., 0, 1, 0, ... 0] where the "1"
   * is positioned in the location specified by the parameter "dim". Valid
   * values of "dim" are 0, ..., VOffsetDimension-1.
   */
  static Self GetBasisOffset(unsigned int dim); 

  /**
   * Set one value for the offset in all dimensions.  Useful for initializing
   * an offset to zero.
   */
  void Fill(long value)
    { for(unsigned int i=0;i < VOffsetDimension; ++i) m_Offset[i] = value; }
  
  /**
   * Offset is an "aggregate" class.  Its data is public (m_Offset)
   * allowing for fast and convienent instantiations/assignments.
   *
   * The following syntax for assigning an index is allowed/suggested:
   *    Offset<3> index = {5, 2, 7};
   */
  long m_Offset[VOffsetDimension];
  
public:

private:
};


template<unsigned int VOffsetDimension>
Offset<VOffsetDimension> 
Offset<VOffsetDimension>
::GetBasisOffset(unsigned int dim)
{
  Self ind;
  
  memset(ind.m_Offset, 0, sizeof(long)*VOffsetDimension);
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
