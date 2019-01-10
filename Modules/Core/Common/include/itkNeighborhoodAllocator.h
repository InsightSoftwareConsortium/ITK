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
#ifndef itkNeighborhoodAllocator_h
#define itkNeighborhoodAllocator_h
#include <algorithm>
#include <iostream>
#include "itkMacro.h"

namespace itk
{
/** \class NeighborhoodAllocator
 *  \brief A memory allocator for use as the default allocator type in
 *         Neighborhood.
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
 * \ingroup ITKCommon
 */
template< typename TPixel >
class NeighborhoodAllocator
{
public:
  /** Standard class type aliases. */
  using Self = NeighborhoodAllocator;

  /** Iterator support. Note that the naming of the type alias is on purpose.
  * itk::Neighborhood makes reference to the allocator, which because it may
  * be vnl or other type, uses the lower case/underscore forms iterator and
  * const_iterator. */
  using iterator = TPixel *;
  using const_iterator = const TPixel *;

  /** Default constructor */
  NeighborhoodAllocator(): m_Data(nullptr)  {}

  /** Default destructor */
  ~NeighborhoodAllocator()
  { this->Deallocate(); }

  /** Allocates memory using new() */
  void Allocate(unsigned int n)
  {
    m_Data = new TPixel[n];
    m_ElementCount = n;
  }

  /** Deallocates memory using delete[](). */
  void Deallocate()
  {
    delete[] m_Data;
    m_ElementCount = 0;
  }

  /** Copy constructor. */
  NeighborhoodAllocator(const Self & other):
    m_ElementCount(other.m_ElementCount),
    m_Data(new TPixel[other.m_ElementCount])
  {
    std::copy(other.m_Data, other.m_Data + m_ElementCount, m_Data);
  }


  /** Move-constructor. */
  NeighborhoodAllocator(Self&& other) ITK_NOEXCEPT
    :
  m_ElementCount{ other.m_ElementCount },
  m_Data{ other.m_Data }
  {
    other.m_ElementCount = 0;
    other.m_Data = nullptr;
  }


  /** Assignment operator. */
  Self & operator=(const Self & other)
  {
    if(this != &other)
      {
      this->set_size(other.m_ElementCount);
      std::copy(other.m_Data, other.m_Data + m_ElementCount, m_Data);
    }
    return *this;
  }


  /** Move-assignment. */
  Self& operator=(Self&& other) ITK_NOEXCEPT
  {
    if (this != &other)
    {
      this->Deallocate();
      m_ElementCount = other.m_ElementCount;
      m_Data = other.m_Data;
      other.m_ElementCount = 0;
      other.m_Data = nullptr;
    }
    return *this;
  }


  /** STL-style iterator support for the memory buffer. */
  iterator begin()
  { return m_Data; }
  const_iterator begin() const
  { return m_Data; }
  iterator end()
  { return ( m_Data + m_ElementCount ); }
  const_iterator end() const
  { return ( m_Data + m_ElementCount ); }
  unsigned int size() const
  { return m_ElementCount; }

  /** Data access methods */
  const TPixel & operator[](unsigned int i) const
  { return m_Data[i]; }
  TPixel & operator[](unsigned int i)
  { return m_Data[i]; }

  /** Allocates or Reallocates a buffer of size n */
  void set_size(unsigned int n)
  {
    if (n != m_ElementCount)
      {
      if ( m_Data )
        {
        this->Deallocate();
        }
      this->Allocate(n);
      }
  }

protected:
  unsigned int m_ElementCount{0};
  TPixel *     m_Data;
};

template< typename TPixel >
inline std::ostream & operator<<(
  std::ostream & o, const NeighborhoodAllocator< TPixel >
  & a)
{
  o << "NeighborhoodAllocator { this = " << &a << ", begin = "
  << static_cast< const void * >( a.begin() )
  << ", size=" << a.size()
  << " }";
  return o;
}


// Equality operator.
template< typename TPixel >
inline bool operator==(
  const NeighborhoodAllocator< TPixel >& lhs,
  const NeighborhoodAllocator< TPixel >& rhs)
{
  const unsigned int size = lhs.size();
  return (size == rhs.size()) &&
    ((size == 0) || std::equal(lhs.begin(), lhs.end(), rhs.begin()));
}

// Inequality operator.
template< typename TPixel >
inline bool operator!=(
  const NeighborhoodAllocator< TPixel >& lhs,
  const NeighborhoodAllocator< TPixel >& rhs)
{
  return ! (lhs == rhs);
}
} // end namespace itk
#endif
