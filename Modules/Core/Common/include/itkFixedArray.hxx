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
#ifndef itkFixedArray_hxx
#define itkFixedArray_hxx

#include "itkNumericTraitsFixedArrayPixel.h"

namespace itk
{
/**
 * Default constructor uses compiler's default initialization of memory.
 * For efficiency, no initialization to zero is done.
 */
template< typename TValue, unsigned int VLength >
FixedArray< TValue, VLength >
::FixedArray()
{}

/**
 * Constructor to initialize entire array to one value.
 */
template< typename TValue, unsigned int VLength >
FixedArray< TValue, VLength >
::FixedArray(const ValueType & r)
{
  for ( Iterator i = Begin(); i != End(); ++i )
    {
    *i = r;
    }
}

/**
 * Constructor assumes input points to array of correct size.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template< typename TValue, unsigned int VLength >
FixedArray< TValue, VLength >
::FixedArray(const ValueType r[VLength])
{
  ConstIterator input = r;
  Iterator      i = this->Begin();

  while ( i != this->End() )
    {
    *i++ = *input++;
    }
}

/**
 * Assignment operator assumes input points to array of correct size.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template< typename TValue, unsigned int VLength >
FixedArray< TValue, VLength > &
FixedArray< TValue, VLength >
::operator=(const ValueType r[VLength])
{
  if ( r != m_InternalArray )
    {
    ConstIterator input = r;
    Iterator      i = this->Begin();
    while ( i != this->End() )
      {
      *i++ = *input++;
      }
    }
  return *this;
}

/**
 * Operator != compares different types of arrays.
 */
template< typename TValue, unsigned int VLength >
bool
FixedArray< TValue, VLength >
::operator==(const FixedArray & r) const
{
  ConstIterator i = this->Begin();
  ConstIterator j = r.Begin();

  while ( i != this->End() )
    {
CLANG_PRAGMA_PUSH
CLANG_SUPPRESS_Wfloat_equal
    if ( *i != *j )
CLANG_PRAGMA_POP
      {
      return false;
      }
    ++j;
    ++i;
    }

  return true;
}

/**
 * Get an Iterator for the beginning of the FixedArray.
 */
template< typename TValue, unsigned int VLength >
typename FixedArray< TValue, VLength >::Iterator
FixedArray< TValue, VLength >
::Begin()
{
  return Iterator(m_InternalArray);
}

/**
 * Get a ConstIterator for the beginning of the FixedArray.
 */
template< typename TValue, unsigned int VLength >
typename FixedArray< TValue, VLength >::ConstIterator
FixedArray< TValue, VLength >
::Begin() const
{
  return ConstIterator(m_InternalArray);
}

/**
 * Get an Iterator for the end of the FixedArray.
 */
template< typename TValue, unsigned int VLength >
typename FixedArray< TValue, VLength >::Iterator
FixedArray< TValue, VLength >
::End()
{
  return Iterator(m_InternalArray + VLength);
}

/**
 * Get a ConstIterator for the end of the FixedArray.
 */
template< typename TValue, unsigned int VLength >
typename FixedArray< TValue, VLength >::ConstIterator
FixedArray< TValue, VLength >
::End() const
{
  return ConstIterator(m_InternalArray + VLength);
}

/**
 * Get a begin ReverseIterator.
 */
template< typename TValue, unsigned int VLength >
typename FixedArray< TValue, VLength >::ReverseIterator
FixedArray< TValue, VLength >
::rBegin()
{
  return ReverseIterator(m_InternalArray + VLength);
}

/**
 * Get a begin ConstReverseIterator.
 */
template< typename TValue, unsigned int VLength >
typename FixedArray< TValue, VLength >::ConstReverseIterator
FixedArray< TValue, VLength >
::rBegin() const
{
  return ConstReverseIterator(m_InternalArray + VLength);
}

/**
 * Get an end ReverseIterator.
 */
template< typename TValue, unsigned int VLength >
typename FixedArray< TValue, VLength >::ReverseIterator
FixedArray< TValue, VLength >
::rEnd()
{
  return ReverseIterator(m_InternalArray);
}

/**
 * Get an end ConstReverseIterator.
 */
template< typename TValue, unsigned int VLength >
typename FixedArray< TValue, VLength >::ConstReverseIterator
FixedArray< TValue, VLength >
::rEnd() const
{
  return ConstReverseIterator(m_InternalArray);
}

/**
 * Get the size of the FixedArray.
 */
template< typename TValue, unsigned int VLength >
typename FixedArray< TValue, VLength >::SizeType
FixedArray< TValue, VLength >
::Size() const
{
  return VLength;
}

/**
 * Fill all elements of the array with the given value.
 */
template< typename TValue, unsigned int VLength >
void
FixedArray< TValue, VLength >
::Fill(const ValueType & value)
{
  Iterator i = this->Begin();

  while ( i != this->End() )
    {
    *i++ = value;
    }
}

/**
 * Return an FixedArray with all elements assigned to the given value.
 */
template< typename TValue, unsigned int VLength >
FixedArray< TValue, VLength >
FixedArray< TValue, VLength >
::Filled(const ValueType & value)
{
  FixedArray< ValueType, VLength > array;
  array.Fill(value);
  return array;
}

template< typename TValue, unsigned int VLength >
std::ostream & operator<<(std::ostream & os, const FixedArray< TValue, VLength > & arr)
{
  os << "[";
  if ( VLength == 1 )
    {
    os << arr[0];
    }
  else
    {
    for ( int i = 0; i < static_cast< int >( VLength ) - 1; ++i )
      {
      os << arr[i] << ", ";
      }
    os << arr[VLength - 1];
    }
  os << "]";
  return os;
}
} // namespace itk

#endif
