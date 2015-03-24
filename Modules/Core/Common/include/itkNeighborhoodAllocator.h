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
  /** Standard class typedefs. */
  typedef NeighborhoodAllocator Self;

  /** Iterator support. Note that the naming of the typedefs is on purpose.
  * itk::Neighborhood makes reference to the allocator, which because it may
  * be vnl or other type, uses the lower case/underscore forms iterator and
  * const_iterator. */
  typedef TPixel *       iterator;
  typedef const TPixel * const_iterator;

  /** Default constructor */
  NeighborhoodAllocator():m_ElementCount(0), m_Data(ITK_NULLPTR)  {}

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
  NeighborhoodAllocator(const Self & other):m_ElementCount(0), m_Data(0)
  {
    this->set_size(other.m_ElementCount);
    for ( unsigned int i = 0; i < other.m_ElementCount; ++i )
      {
      this->operator[](i) = other[i];
      }
    m_ElementCount = other.m_ElementCount;
  }

  /** Assignment operator. */
  const Self & operator=(const Self & other)
  {
    if(this != &other)
      {
      this->set_size(other.m_ElementCount);
      for ( unsigned int i = 0; i < other.m_ElementCount; ++i )
        {
        this->operator[](i) = other[i];
        }
      m_ElementCount = other.m_ElementCount;
      }
    return *this;
  }

  /** Comparison operator. */
  bool operator==(const Self & other) const
  {
    return ( m_Data == other.m_Data );
  }

  /** Not Equal operator. */
  bool operator!=(const Self & other) const
  {
    return ( m_Data != other.m_Data );
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
    if ( m_Data ) { Deallocate(); }
    this->Allocate(n);
  }

protected:
  unsigned int m_ElementCount;
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
} // end namespace itk
#endif
