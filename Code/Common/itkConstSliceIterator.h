/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstSliceIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConstSliceIterator_h
#define __itkConstSliceIterator_h

#include "itkMacro.h"
#include "itkExceptionObject.h"
#include <valarray>
namespace itk {

/**
 * \class ConstSliceIterator
 * \brief A flexible iterator for itk containers(i.e. itk::Neighborhood)
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
 * Modeled after a slice iterator proposed by Bjarne Stroustrup
 * in C++ Programming Language, Third Edition. Bjarne Stroustrup.  Addison
 * Wesley, Reading, MA. 1997.
 *
 * \ingroup Iterators
 *
 */
template<class TPixel, class TContainer>
class ITK_EXPORT ConstSliceIterator
{
public:
  /** Constructor. */
  ConstSliceIterator(const TContainer *n, std::slice s)
    : m_ContainerPointer(n), m_Pos(0), m_Slice(s) {}

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
    ans.m_Pos = m_Slice.size();
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
  const TPixel& operator[](unsigned long n)
    {
    return this->Loc(m_Pos=n);
    }

  /** Dereferences the iterator, returning the value that it points
   * to. */
  const TPixel& operator*()
    {
    return Loc(m_Pos);
    }

  /** Returns the logical && of the boolean == of two slice iterator positions,
   * stride, and start locations. */
  bool operator==(const ConstSliceIterator &orig)
    {
    return orig.m_Pos == this->m_Pos
      &&   orig.m_Slice.stride() == this->m_Slice.stride()
      &&   orig.m_Slice.start() == this->m_Slice.start();
    }
  
  /** Returns the logical inverse of the boolean == of two slice iterators. */
  bool operator!=(const ConstSliceIterator &orig)
    {
    return ! operator==(orig);
    }

  /** Returns the boolean < of two slice iterator positions.  Result
   * is only true if the slice iterators have the same stride and
   * start location. */
  bool operator<(const ConstSliceIterator &orig)
    {
    return this->m_Pos < orig.m_Pos
      &&   this->m_Slice.stride() == orig.m_Slice.stride()
      &&   this->m_Slice.start() == orig.m_Slice.start();
    }
  
private:
  /** Returns the value located at position n of the slice. */
  const TPixel& Loc(unsigned long n) const
    {
    return (*m_ContainerPointer)[m_Slice.start() + n * m_Slice.stride()];
    }  

  /** Pointer to the container referenced by the slice iterator. */
  const TContainer *m_ContainerPointer;

  /** Current position within the slice. */
  unsigned long m_Pos;

  /** Slice structure information. */
  std::slice m_Slice;
};
  
} // end namespace itk

#endif
