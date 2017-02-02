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
#ifndef itkFixedArray_h
#define itkFixedArray_h

#include "itkMacro.h"

namespace itk
{

/** \class FixedArray
 *  \brief Simulate a standard C array with copy semnatics.
 *
 * Simulates a standard C array, except that copy semantics are used instead
 * of reference semantics.  Also, arrays of different sizes cannot be
 * assigned to one another, and size information is known for function
 * returns.
 *
 * \tparam TValue Element type stored at each location in the array.
 * \tparam VLength    = Length of the array.
 *
 * The length of the array is fixed at compile time. If you wish to
 * specify the length of the array at run-time, use the class itk::Array.
 * If you wish to change to change the length of the array at run-time,
 * you're best off using std::vector<>.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Utilities/FixedArray,C-style array}
 * \endwiki
 */
template< typename TValue, unsigned int VLength = 3 >
class ITK_TEMPLATE_EXPORT FixedArray
{
public:
  /** Length constant */
  itkStaticConstMacro(Length, unsigned int, VLength);

  /** Dimension constant */
  itkStaticConstMacro(Dimension, unsigned int, VLength);

  /** The element type stored at each location in the FixedArray. */
  typedef TValue ValueType;

  /** A type representing the C-array version of this FixedArray. */
  typedef ValueType CArray[VLength];

  /** An iterator through the array. */
  typedef ValueType *Iterator;

  /** A const iterator through the array. */
  typedef const ValueType *ConstIterator;

  class ConstReverseIterator;

  /** \class ReverseIterator
   * \brief A reverse iterator through an array.
   * \ingroup ITKCommon
   */
  class ReverseIterator
  {
  public:
    explicit ReverseIterator(Iterator i):m_Iterator(i) {}
    Iterator operator++()        { return --m_Iterator; }
    Iterator operator++(int)     { return m_Iterator--; }
    Iterator operator--()        { return ++m_Iterator; }
    Iterator operator--(int)     { return m_Iterator++; }
    Iterator operator->() const { return ( m_Iterator - 1 ); }
    ValueType & operator*() const { return *( m_Iterator - 1 ); }
    bool operator!=(const ReverseIterator & rit) const { return m_Iterator != rit.m_Iterator; }
    bool operator==(const ReverseIterator & rit) const { return m_Iterator == rit.m_Iterator; }

  private:
    Iterator m_Iterator;
    friend class ConstReverseIterator;
  };

  /** \class ConstReverseIterator
   * \brief A const reverse iterator through an array.
   * \ingroup ITKCommon
   */
  class ConstReverseIterator
  {
  public:
    explicit ConstReverseIterator(ConstIterator i):m_Iterator(i) {}
    ConstReverseIterator(const ReverseIterator & rit) { m_Iterator = rit.m_Iterator; }
    ConstIterator operator++()         { return --m_Iterator; }
    ConstIterator operator++(int)      { return m_Iterator--; }
    ConstIterator operator--()         { return ++m_Iterator; }
    ConstIterator operator--(int)      { return m_Iterator++; }
    ConstIterator operator->() const { return ( m_Iterator - 1 ); }
    const ValueType & operator*() const { return *( m_Iterator - 1 ); }
    bool operator!=(const ConstReverseIterator & rit) const { return m_Iterator != rit.m_Iterator; }
    bool operator==(const ConstReverseIterator & rit) const { return m_Iterator == rit.m_Iterator; }

  private:
    ConstIterator m_Iterator;
  };

  /** A pointer to the ValueType. */
  typedef ValueType *pointer;

  /** A const pointer to the ValueType. */
  typedef const ValueType *const_pointer;

  /** A reference to the ValueType. */
  typedef ValueType & reference;

  /** A const reference to the ValueType. */
  typedef const ValueType & const_reference;

  typedef unsigned int SizeType;

public:
  /** Constructors */
  FixedArray();
  FixedArray(const ValueType r[VLength]);
  FixedArray(const ValueType & r);

  /** Constructor to initialize a fixed array from another of any data type */
  template< typename TFixedArrayValueType >
  FixedArray(const FixedArray< TFixedArrayValueType, VLength > & r)
  {
    typename FixedArray< TFixedArrayValueType, VLength >::ConstIterator input = r.Begin();
    Iterator i = this->Begin();
    while ( i != this->End() )
      {
      *i++ = static_cast< TValue >( *input++ );
      }
  }

  template< typename TScalarValue >
  FixedArray(const TScalarValue *r)
    {
      std::copy(r, r + this->Size(), this->GetDataPointer());
    }

  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory.
   *
   * The destructor is PURPOSELY NOT DEFINED, in order to prevent inefficient
   * byte alignment of arrays of this object.
   *
   * ~FixedArray();
   *
   * For a full discussion, see
   * https://www.itk.org/mailman/private/insight-developers/2008-June/010480.html
   *
   */

  /** Operator= defined for a variety of types. */
  template< typename TFixedArrayValueType >
  FixedArray & operator=(const FixedArray< TFixedArrayValueType, VLength > & r)
  {
    if ( (const void *)r.Begin() != (const void *)m_InternalArray )
      {
      typename FixedArray< TFixedArrayValueType, VLength >::ConstIterator input = r.Begin();
      Iterator i = this->Begin();
      while ( i != this->End() )
        {
        *i++ = static_cast< TValue >( *input++ );
        }
      }
    return *this;
  }

  FixedArray & operator=(const ValueType r[VLength]);

  /** Operators == and != are used to compare whether two arrays are equal.
   * Note that arrays are equal when the number of components (size) is the
   * same, and each component value is equal. */
  bool operator==(const FixedArray & r) const;

  bool operator!=(const FixedArray & r) const
  { return !operator==(r); }

  /** Allow the FixedArray to be indexed normally.  No bounds checking is done.
   * The separate versions are a work-around for an integer conversion bug in
   * Visual C++. */
  reference operator[](short index)                { return m_InternalArray[index]; }
  const_reference operator[](short index) const { return m_InternalArray[index]; }
  reference operator[](unsigned short index)       { return m_InternalArray[index]; }
  const_reference operator[](unsigned short index) const { return m_InternalArray[index]; }
  reference operator[](int index)                  { return m_InternalArray[index]; }
  const_reference operator[](int index) const { return m_InternalArray[index]; }
// false positive warnings with GCC
#if defined( __GNUC__ )
#if ( __GNUC__ == 4 ) && ( __GNUC_MINOR__ == 9 ) || ( __GNUC__ >= 7 )
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#endif
#endif
  reference operator[](unsigned int index)         { return m_InternalArray[index]; }
  const_reference operator[](unsigned int index) const { return m_InternalArray[index]; }
#if defined( __GNUC__ )
#if ( __GNUC__ == 4 ) && ( __GNUC_MINOR__ == 9 ) || ( __GNUC__ >= 7 )
#pragma GCC diagnostic pop
#endif
#endif
  reference operator[](long index)                 { return m_InternalArray[index]; }
  const_reference operator[](long index) const { return m_InternalArray[index]; }
  reference operator[](unsigned long index)        { return m_InternalArray[index]; }
  const_reference operator[](unsigned long index) const { return m_InternalArray[index]; }
  reference operator[](long long index)                 { return m_InternalArray[index]; }
  const_reference operator[](long long index) const { return m_InternalArray[index]; }
  reference operator[](unsigned long long index)        { return m_InternalArray[index]; }
  const_reference operator[](unsigned long long index) const { return m_InternalArray[index]; }

  /** Set/Get element methods are more convenient in wrapping languages */
  void SetElement(unsigned short index, const_reference value)
  { m_InternalArray[index] = value; }
  const_reference GetElement(unsigned short index) const { return m_InternalArray[index]; }

  /** Return a pointer to the data. */
  ValueType * GetDataPointer()
  {
    return m_InternalArray; \
  }

  const ValueType * GetDataPointer() const
  {
    return m_InternalArray; \
  }

  /** Get various iterators to the array. */
  Iterator      Begin();

  ConstIterator Begin() const;

  Iterator      End();

  ConstIterator End() const;

  ReverseIterator      rBegin();

  ConstReverseIterator rBegin() const;

  ReverseIterator      rEnd();

  ConstReverseIterator rEnd() const;

  SizeType      Size() const;

  void Fill(const ValueType &);

private:
  /** Internal C array representation. */
  CArray m_InternalArray;

public:

  static FixedArray Filled(const ValueType &);
};

template< typename TValue, unsigned int VLength >
std::ostream & operator<<(std::ostream & os, const FixedArray< TValue, VLength > & arr);
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFixedArray.hxx"
#endif

#include "itkNumericTraitsFixedArrayPixel.h"

#endif
