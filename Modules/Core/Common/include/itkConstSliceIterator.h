/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkConstSliceIterator_h
#define itkConstSliceIterator_h

#include "itkMacro.h"
#include "itkIntTypes.h"
#include <valarray>

namespace itk
{
/**
 * \class ConstSliceIterator
 * \brief A flexible iterator for ITK containers(i.e. itk::Neighborhood)
 * that support pixel access through operator[].
 *
 * ConstSliceIterator allows const iteration along a std::slice through the
 * container.
 * A slice is a construct that defines a starting position, stride length
 * (distance between adjacent elements), and a length.
 *
 * Any container with operator[] const is supported.  Because it uses this
 *  interface the iterator is only as efficient as the implementation of a
 * container's operator[] method.
 *
 * References:
 * Modelled after a slice iterator proposed by Bjarne Stroustrup
 * in C++ Programming Language, Third Edition. Bjarne Stroustrup.  Addison
 * Wesley, Reading, MA. 1997.
 *
 * \ingroup Iterators
 *
 * \ingroup ITKCommon
 */
template< typename TPixel, typename TContainer >
class ConstSliceIterator
{
public:
  /** Constructor. */
  ConstSliceIterator(const TContainer *n, std::slice s):
    m_ContainerPointer(n), m_Pos(0), m_Slice(s) {}

  /** Returns a ConstSliceIterator that points to the beginning of the slice. */
  ConstSliceIterator Begin()
  {
    ConstSliceIterator ans = *this;

    ans.m_Pos = 0;
    return ans;
  }

  /** Returns a ConstSliceIterator that points to one past the end
   *  of the slice. */
  ConstSliceIterator End()
  {
    ConstSliceIterator ans = *this;

    ans.m_Pos = static_cast< SizeValueType >( m_Slice.size() );
    return ans;
  }

  /** Increments the iterator. */
  ConstSliceIterator operator++()
  {
    m_Pos++;
    return *this;
  }

  /** Increments the iterator. */
  ConstSliceIterator operator++(int)
  {
    ConstSliceIterator ans  = *this;

    m_Pos++;
    return ans;
  }

  /** Returns the element at position n of the slice. Sets the
   * iterator to point to position n. */
  const TPixel & operator[](SizeValueType n)
  {
    return this->Loc(m_Pos = n);
  }

  /** Dereferences the iterator, returning the value that it points
   * to. */
  const TPixel & operator*()
  {
    return Loc(m_Pos);
  }

  /** Returns the logical && of the boolean == of two slice iterator positions,
   * stride, and start locations. */
  bool operator==(const ConstSliceIterator & orig)
  {
    return orig.m_Pos == this->m_Pos
           &&   orig.m_Slice.stride() == this->m_Slice.stride()
           &&   orig.m_Slice.start() == this->m_Slice.start();
  }

  /** Returns the logical inverse of the boolean == of two slice iterators. */
  bool operator!=(const ConstSliceIterator & orig)
  {
    return !operator==(orig);
  }

  /** Returns the boolean < of two slice iterator positions.  Result
   * is only true if the slice iterators have the same stride and
   * start location. */
  bool operator<(const ConstSliceIterator & orig)
  {
    return this->m_Pos < orig.m_Pos
           &&   this->m_Slice.stride() == orig.m_Slice.stride()
           &&   this->m_Slice.start() == orig.m_Slice.start();
  }

private:
  /** Returns the value located at position n of the slice. */
  const TPixel & Loc(SizeValueType n) const
  {
    return ( *m_ContainerPointer )[static_cast< SizeValueType >( m_Slice.start() + n * m_Slice.stride())];
  }

  /** Pointer to the container referenced by the slice iterator. */
  const TContainer *m_ContainerPointer;

  /** Current position within the slice. */
  SizeValueType m_Pos;

  /** Slice structure information. */
  std::slice m_Slice;
};
} // end namespace itk

#endif
