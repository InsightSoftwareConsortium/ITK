/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAllocator.h
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
#ifndef __itkNeighborhoodAllocator_h
#define __itkNeighborhoodAllocator_h
#include <iostream>

namespace itk {
/**
 * \class NeighborhoodAllocator
 * 
 * This is a memory allocator for use as the default allocator type in
 * Neighborhood.  The API is designed to mimic that of vnl_vector so that
 * vnl_vector can also be used as an allocator for Neighborhood.
 *
 * The decision to create this allocator with the vnl_vector api (versus
 * using an STL allocator and wrapping the vnl_vector API) was made because
 * the STL allocator API is not guaranteed stable at this time.
 *
 * \ingroup Operators
 */
template <class TPixel>
class NeighborhoodAllocator
{
public:
  /**
   * Standard typedefs.
   */
  typedef NeighborhoodAllocator Self;

  /**
   * Iterator support.
   */
  typedef TPixel * iterator;
  typedef const TPixel * const_iterator;

  /**
   * Default constructor
   */
  NeighborhoodAllocator() : m_ElementCount(0), m_Data(0)  {}

  /**
   * Default destructor
   */
  ~NeighborhoodAllocator()
  { this->Deallocate(); }

  /**
   * Allocates memory using new()
   */
  void Allocate(unsigned int n)
  {
    m_Data = new TPixel[n];
    m_ElementCount = n;    
  }

  /**
   * Deallocates memory using delete[]()
   */
  void Deallocate()
  {
    if (m_Data) delete[] m_Data;
    m_ElementCount = 0;
  }

  /**
   * Copy constructor
   */
  NeighborhoodAllocator(const Self& other) : m_ElementCount(0), m_Data(0)
  {
    this->resize(other.m_ElementCount);
    for (unsigned int i = 0; i < other.m_ElementCount; ++i)
      this->operator[](i) = other[i];
    m_ElementCount = other.m_ElementCount;
  }

  /**
   * Assignment operator
   */
  const Self& operator=(const Self& other)
  {
    this->resize(other.m_ElementCount);
    for (unsigned int i = 0; i < other.m_ElementCount; ++i)
      this->operator[](i) = other[i];
    m_ElementCount = other.m_ElementCount;
    return *this;
  }

  /**
   * STL-style iterator support for the memory buffer
   */
  iterator begin()
  { return m_Data; }
  
  const_iterator begin() const
  { return m_Data; }
  
  iterator end()
  { return (m_Data + m_ElementCount); }
  
  const_iterator end() const
  { return (m_Data + m_ElementCount); }

  unsigned int size() const
  { return m_ElementCount; }
  
  /**
   * Data access methods
   */
  const TPixel & operator[](unsigned int i) const
  { return m_Data[i]; }
  
  TPixel &operator[](unsigned int i)
  { return m_Data[i]; }

  /**
   * Allocates or Reallocates a buffer of size n
   */
  void resize(unsigned int n)
  {
    if (m_Data) Deallocate();
    this->Allocate(n);
  }

protected:
  unsigned int m_ElementCount;
  TPixel *m_Data;
};

template<class TPixel>
inline std::ostream& operator<<(std::ostream &o, const NeighborhoodAllocator<TPixel>
                                & a)
{
    o << "NeighborhoodAllocator { this = " << &a << ", begin = "
      << a.begin()
      << ", size=" << a.size()
      << " }";
      //      << ", contents:{ ";
      //    for (int i = 0; i < a.size(); ++i) o << a[i] << " ";
    o << " } }";
  return o;
}
  


} // end namespace itk
#endif
