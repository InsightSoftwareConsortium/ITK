/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include <algorithm>
#include <array>

namespace itk
{

/** \class FixedArray
 *  \brief Simulate a standard C array with copy semantics.
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
 * \sphinx
 * \sphinxexample{Core/Common/CreateAFixedArray,Create A Fixed Array}
 * \endsphinx
 */
template <typename TValue, unsigned int VLength = 3>
class ITK_TEMPLATE_EXPORT FixedArray
{
public:
  /** Length constant */
  static constexpr unsigned int Length = VLength;

  /** Dimension constant */
  static constexpr unsigned int Dimension = VLength;

  /** The element type stored at each location in the FixedArray. */
  using ValueType = TValue;

  /** A type representing the C-array version of this FixedArray. */
  using CArray = ValueType[VLength];

  /** An iterator through the array. */
  using Iterator = ValueType *;

  /** A const iterator through the array. */
  using ConstIterator = const ValueType *;

  class ConstReverseIterator;

  /** \class ReverseIterator
   * \brief A reverse iterator through an array.
   * \ingroup ITKCommon
   */
  class ReverseIterator
  {
  public:
    explicit ReverseIterator(Iterator i)
      : m_Iterator(i)
    {}
    ReverseIterator
    operator++()
    {
      return ReverseIterator(--m_Iterator);
    }
    ReverseIterator
    operator++(int)
    {
      return ReverseIterator(m_Iterator--);
    }
    ReverseIterator
    operator--()
    {
      return ReverseIterator(++m_Iterator);
    }
    ReverseIterator
    operator--(int)
    {
      return ReverseIterator(m_Iterator++);
    }
    Iterator    operator->() const { return (m_Iterator - 1); }
    ValueType & operator*() const { return *(m_Iterator - 1); }
    bool
    operator!=(const ReverseIterator & rit) const
    {
      return m_Iterator != rit.m_Iterator;
    }
    bool
    operator==(const ReverseIterator & rit) const
    {
      return m_Iterator == rit.m_Iterator;
    }

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
    explicit ConstReverseIterator(ConstIterator i)
      : m_Iterator(i)
    {}
    ConstReverseIterator(const ReverseIterator & rit) { m_Iterator = rit.m_Iterator; }
    ConstReverseIterator
    operator++()
    {
      return ConstReverseIterator(--m_Iterator);
    }
    ConstReverseIterator
    operator++(int)
    {
      return ConstReverseIterator(m_Iterator--);
    }
    ConstReverseIterator
    operator--()
    {
      return ConstReverseIterator(++m_Iterator);
    }
    ConstReverseIterator
    operator--(int)
    {
      return ConstReverseIterator(m_Iterator++);
    }
    ConstIterator     operator->() const { return (m_Iterator - 1); }
    const ValueType & operator*() const { return *(m_Iterator - 1); }
    bool
    operator!=(const ConstReverseIterator & rit) const
    {
      return m_Iterator != rit.m_Iterator;
    }
    bool
    operator==(const ConstReverseIterator & rit) const
    {
      return m_Iterator == rit.m_Iterator;
    }

  private:
    ConstIterator m_Iterator;
  };

  /** A pointer to the ValueType. */
  using pointer = ValueType *;

  /** A const pointer to the ValueType. */
  using const_pointer = const ValueType *;

  /** A reference to the ValueType. */
  using reference = ValueType &;

  /** A const reference to the ValueType. */
  using const_reference = const ValueType &;

  /** The return type of the non-const overloads of begin() and end(). */
  using iterator = ValueType *;

  /** The return type of cbegin() and cend(), and the const overloads of begin() and end(). */
  using const_iterator = const ValueType *;

  using reverse_iterator = std::reverse_iterator<iterator>;

  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  using SizeType = unsigned int;

public:
  /** Constructors */
  FixedArray() = default;
  FixedArray(const FixedArray &) = default;
  FixedArray &
  operator=(const FixedArray &) = default;
  FixedArray(FixedArray &&) = default;
  FixedArray &
  operator=(FixedArray &&) = default;
  ~FixedArray() = default;

  /** Conversion constructors */
  FixedArray(const ValueType r[VLength]);
  FixedArray(const ValueType &);

  /** Explicit constructor for std::array. */
  explicit FixedArray(const std::array<ValueType, VLength> & stdArray)
  {
    std::copy_n(stdArray.cbegin(), VLength, m_InternalArray);
  }

  /** Constructor to initialize a fixed array from another of any data type */
  template <typename TFixedArrayValueType>
  FixedArray(const FixedArray<TFixedArrayValueType, VLength> & r)
  {
    auto input = r.cbegin();

    for (auto & element : m_InternalArray)
    {
      element = static_cast<TValue>(*input++);
    }
  }

  template <typename TScalarValue>
  FixedArray(const TScalarValue * r)
  {
    std::copy_n(r, VLength, m_InternalArray);
  }

  /** Operator= defined for a variety of types. */
  template <typename TFixedArrayValueType>
  FixedArray &
  operator=(const FixedArray<TFixedArrayValueType, VLength> & r)
  {
    auto input = r.cbegin();

    for (auto & element : m_InternalArray)
    {
      element = static_cast<TValue>(*input++);
    }
    return *this;
  }

  FixedArray &
  operator=(const ValueType r[VLength]);

  /** Operators == and != are used to compare whether two arrays are equal.
   * Note that arrays are equal when the number of components (size) is the
   * same, and each component value is equal. */
  bool
  operator==(const FixedArray & r) const;

  bool
  operator!=(const FixedArray & r) const
  {
    return !operator==(r);
  }

  /** Allow the FixedArray to be indexed normally.  No bounds checking is done.
   * The separate versions are a work-around for an integer conversion bug in
   * Visual C++. */
  reference       operator[](short index) { return m_InternalArray[index]; }
  const_reference operator[](short index) const { return m_InternalArray[index]; }
  reference       operator[](unsigned short index) { return m_InternalArray[index]; }
  const_reference operator[](unsigned short index) const { return m_InternalArray[index]; }
  reference       operator[](int index) { return m_InternalArray[index]; }
  const_reference operator[](int index) const { return m_InternalArray[index]; }
// false positive warnings with GCC
#if defined(__GNUC__)
#  if (__GNUC__ == 4) && (__GNUC_MINOR__ == 9) || (__GNUC__ >= 7)
#    pragma GCC diagnostic push
#    pragma GCC diagnostic ignored "-Warray-bounds"
#  endif
#endif
  reference       operator[](unsigned int index) { return m_InternalArray[index]; }
  const_reference operator[](unsigned int index) const { return m_InternalArray[index]; }
#if defined(__GNUC__)
#  if (__GNUC__ == 4) && (__GNUC_MINOR__ == 9) || (__GNUC__ >= 7)
#    pragma GCC diagnostic pop
#  endif
#endif
  reference       operator[](long index) { return m_InternalArray[index]; }
  const_reference operator[](long index) const { return m_InternalArray[index]; }
  reference       operator[](unsigned long index) { return m_InternalArray[index]; }
  const_reference operator[](unsigned long index) const { return m_InternalArray[index]; }
  reference       operator[](long long index) { return m_InternalArray[index]; }
  const_reference operator[](long long index) const { return m_InternalArray[index]; }
  reference       operator[](unsigned long long index) { return m_InternalArray[index]; }
  const_reference operator[](unsigned long long index) const { return m_InternalArray[index]; }

  /** Set/Get element methods are more convenient in wrapping languages */
  void
  SetElement(unsigned int index, const_reference value)
  {
    m_InternalArray[index] = value;
  }
  const_reference
  GetElement(unsigned int index) const
  {
    return m_InternalArray[index];
  }

  /** Return a pointer to the data. */
  ValueType *
  GetDataPointer()
  {
    return m_InternalArray;
  }

  const ValueType *
  GetDataPointer() const
  {
    return m_InternalArray;
  }

  ValueType *
  data()
  {
    return m_InternalArray;
  }

  const ValueType *
  data() const
  {
    return m_InternalArray;
  }

  /** Get various iterators to the array. */
  Iterator
  Begin();

  ConstIterator
  Begin() const;

  Iterator
  End();

  ConstIterator
  End() const;

  itkLegacyMacro(ReverseIterator rBegin());

  itkLegacyMacro(ConstReverseIterator rBegin() const);

  itkLegacyMacro(ReverseIterator rEnd());

  itkLegacyMacro(ConstReverseIterator rEnd() const);

  const_iterator
  cbegin() const noexcept
  {
    return m_InternalArray;
  }

  iterator
  begin() noexcept
  {
    return m_InternalArray;
  }

  const_iterator
  begin() const noexcept
  {
    return this->cbegin();
  }

  const_iterator
  cend() const noexcept
  {
    return m_InternalArray + VLength;
  }

  iterator
  end() noexcept
  {
    return m_InternalArray + VLength;
  }

  const_iterator
  end() const noexcept
  {
    return this->cend();
  }

  reverse_iterator
  rbegin()
  {
    return reverse_iterator{ this->end() };
  }

  const_reverse_iterator
  crbegin() const
  {
    return const_reverse_iterator{ this->cend() };
  }

  const_reverse_iterator
  rbegin() const
  {
    return this->crbegin();
  }

  reverse_iterator
  rend()
  {
    return reverse_iterator{ this->begin() };
  }

  const_reverse_iterator
  crend() const
  {
    return const_reverse_iterator{ this->cbegin() };
  }

  const_reverse_iterator
  rend() const
  {
    return this->crend();
  }

  /** Size of the container */
  SizeType
  Size() const;

  constexpr SizeType
  size() const
  {
    return VLength;
  }

  /** Set all the elements of the container to the input value. */
  void
  Fill(const ValueType &);

  void
  swap(FixedArray & other)
  {
    std::swap(m_InternalArray, other.m_InternalArray);
  }

private:
  /** Internal C array representation. */
  CArray m_InternalArray;

public:
  static FixedArray
  Filled(const ValueType &);
};

template <typename TValue, unsigned int VLength>
std::ostream &
operator<<(std::ostream & os, const FixedArray<TValue, VLength> & arr);


template <typename TValue, unsigned int VLength>
inline void
swap(FixedArray<TValue, VLength> & a, FixedArray<TValue, VLength> & b)
{
  a.swap(b);
}

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFixedArray.hxx"
#endif

#include "itkNumericTraitsFixedArrayPixel.h"

#endif
