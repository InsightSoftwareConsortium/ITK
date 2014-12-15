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
#ifndef itkSliceIterator_h
#define itkSliceIterator_h

#include "itkMacro.h"
#include "itkIntTypes.h"
#include <valarray>

namespace itk
{
/** \class SliceIterator
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
 * Modelled after a slice iterator proposed by Bjarne Stroustrup
 * in C++ Programming Language, Third Edition. Bjarne Stroustrup.  Addison
 * Wesley, Reading, MA. 1997.
 *
 * \ingroup Iterators
 * \ingroup ITKCommon
 */
template< typename TPixel, typename TContainer >
class SliceIterator
{
public:
  /** Constructor. */
  SliceIterator(TContainer *n, std::slice s):
    m_ContainerPointer(n), m_Pos(0), m_Slice(s) {}

  /** Returns a SliceIterator that points to the beginning of the slice. */
  SliceIterator Begin()
  {
    SliceIterator ans = *this;

    ans.m_Pos = 0;
    return ans;
  }

  /** Returns a SliceIterator that points to one past the end of the slice. */
  SliceIterator End()
  {
    SliceIterator ans = *this;

    ans.m_Pos = static_cast< OffsetValueType >( m_Slice.size() );
    return ans;
  }

  /** Increments the iterator. */
  SliceIterator operator++()
  {
    m_Pos++;
    return *this;
  }

  /** Increments the iterator. */
  SliceIterator operator++(int)
  {
    SliceIterator ans  = *this;

    m_Pos++;
    return ans;
  }

  /** Returns the element at position n of the slice. Sets the
   * iterator to point to position n. */
  TPixel & operator[](OffsetValueType n)
  { return this->Loc(m_Pos = n); }

  /** Dereferences the iterator, returning the value that it points
   * to. */
  TPixel & operator*()
  { return Loc(m_Pos); }

  /** Returns the logical && of the boolean == of two slice iterator positions,
   * stride, and start locations. */
  bool operator==(const SliceIterator & orig)
  {
    return orig.m_Pos == this->m_Pos
           &&   orig.m_Slice.stride() == this->m_Slice.stride()
           &&   orig.m_Slice.start() == this->m_Slice.start();
  }

  /** Returns the logical inverse of the boolean == of two slice iterators. */
  bool operator!=(const SliceIterator & orig)
  {
    return !operator==(orig);
  }

  /** Returns the boolean < of two slice iterator positions.  Result
   * is only true if the slice iterators have the same stride and
   * start location. */
  bool operator<(const SliceIterator & orig)
  {
    return this->m_Pos < orig.m_Pos
           &&   this->m_Slice.stride() == orig.m_Slice.stride()
           &&   this->m_Slice.start() == orig.m_Slice.start();
  }

private:
  /** Returns the value located at position n of the slice. */
  TPixel & Loc(OffsetValueType n) const
  {
    const OffsetValueType start  = static_cast< OffsetValueType >( m_Slice.start() );
    const OffsetValueType stride = static_cast< OffsetValueType >( m_Slice.stride() );

    return ( *m_ContainerPointer )[start + n * stride];
  }

  /** Pointer to the container referenced by the slice iterator. */
  TContainer *m_ContainerPointer;

  /** Current position within the slice. */
  OffsetValueType m_Pos;

  /** Slice structure information. */
  std::slice m_Slice;
};
} // end namespace itk

#endif
