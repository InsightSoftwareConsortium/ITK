/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSliceIterator.h
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
#ifndef __itkSliceIterator_h
#define __itkSliceIterator_h

#include "itkMacro.h"
#include "itkExceptionObject.h"
#include <valarray>
namespace itk {

/**
 * \class SliceIterator
 * \brief A flexible iterator for itk containers(i.e. itk::Neighborhood)
 * that support pixel access through operator[].
 *
 * SliceIterator allows iteration along a std::slice through the container.
 * A slice is a construct that defines a starting position, stride length
 * (distance between adjacent elements), and a length.
 *
 * Any container with operator[] is supported.  Because it uses this interface
 * the iterator is only as efficient as the implementation of a container's
 * operator[] method.
 *
 * References:
 * Modeled after a slice iterator proposed by Bjarne Stroustrup
 * in C++ Programming Language, Third Edition. Bjarne Stroustrup.  Addison
 * Wesley, Reading, MA. 1997.
 *
 *
 * \ingroup Iterators
 *
 *
 *
 */
template<class TPixel, class TContainer>
class ITK_EXPORT SliceIterator
{
public:
  /**
   * Constructor.
   */
  SliceIterator(TContainer *n, std::slice s)
    : m_ContainerPointer(n), m_Pos(0), m_Slice(s) {}

  /**
   * Returns a SliceIterator that points to the beginning of the slice.
   */
  SliceIterator Begin()
  {
    SliceIterator ans = *this;
    ans.m_Pos = 0;
    return ans;
  }
  
  /**
   * Returns a SliceIterator that points to one past the end of the slice.
   */
  SliceIterator End()
  {
    SliceIterator ans = *this;
    ans.m_Pos = m_Slice.size();
    return ans;
  }

  /**
   * Increments the iterator.
   */
  SliceIterator operator++()
  {
    m_Pos++;
    return *this;
  }

  /**
   * Increments the iterator.
   */
  SliceIterator operator++(int)
  {
    SliceIterator ans  = *this;
    m_Pos++;
    return ans;
  }

  /**
   * Returns the element at position n of the slice. Sets the
   * iterator to point to position n.
   */
  TPixel& operator[](unsigned long n)
  {
    return this->Loc(m_Pos=n);
  }

  /**
   * Dereferences the iterator, returning the value that it points
   * to.
   */
  TPixel& operator*()
  {
    return Loc(m_Pos);
  }

  /**
   * Returns the logical && of the boolean == of two slice iterator positions,
   * stride, and start locations.
   */
  bool operator==(const SliceIterator &orig)
  {
    return orig.m_Pos==this->m_Pos
      &&   orig.m_Slice.stride()==this->m_Slice.stride()
      &&   orig.m_Slice.start() ==this->m_Slice.start();
  }
  
  /**
   * Returns the logical inverse of the boolean == of two slice iterators.
   */
  bool operator!=(const SliceIterator &orig)
  {
    return ! operator==(orig);
  }

  /**
   * Returns the boolean < of two slice iterator positions.  Result
   * is only true if the slice iterators have the same stride and
   * start location.
   */
  bool operator<(const SliceIterator &orig)
  {
    return this->m_Pos < orig.m_Pos
      &&   this->m_Slice.stride()==orig.m_Slice.stride()
      &&   this->m_Slice.start()==orig.m_Slice.start();
  }
  
private:
  /**
   * Returns the value located at position n of the slice.
   */
  TPixel& Loc(unsigned long n) const
  {
    return (*m_ContainerPointer)[m_Slice.start()
                                       + n * m_Slice.stride()];
  }  

  /**
   * Pointer to the container referenced by the slice iterator.
   */
  TContainer *m_ContainerPointer;

  /**
   * Current position within the slice.
   */
  unsigned long m_Pos;

  /**
   * Slice structure information.
   */
  std::slice m_Slice;
};
  
} // end namespace itk

#endif
