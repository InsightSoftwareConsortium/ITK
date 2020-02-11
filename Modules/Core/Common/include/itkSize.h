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
#ifndef itkSize_h
#define itkSize_h

#include "itkIntTypes.h"
#include "itkMacro.h"
#include <algorithm> // For copy_n.
#include <type_traits>
#include <memory>

namespace itk
{

/** \struct Size
 * \brief Represent a n-dimensional size (bounds) of a n-dimensional image.
 *
 * Size is a templated class to represent multi-dimensional array bounds,
 * i.e. (I,J,K,...).  Size is templated over teh dimension of the bounds.
 * ITK assumes the first element of a size (bounds) is the fastest moving index.
 *
 * For efficiency, Size does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * Size is an "aggregate" class.  Its data is public (m_InternalArray)
 * allowing for fast and convenient instantiations/assignments.
 *
 * The following syntax for assigning an aggregate type like this is allowed/suggested:
 *
 *
 * Size<3> var{{ 256, 256, 20 }}; // Also prevent narrowing conversions
 * Size<3> var = {{ 256, 256, 20 }};
 *
 * The doubled braces {{ and }} are required to prevent `gcc -Wall'
 * (and perhaps other compilers) from complaining about a partly
 * bracketed initializer.
 *
 * As an aggregate type that is intended to provide highest performance
 * characteristics, this class is not appropriate to inherit from,
 * so setting this struct as final.
 *
 * \sa Index
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/CreateASize,Create A Size}
 * \endsphinx
 */

template <unsigned int VDimension = 2>
struct ITK_TEMPLATE_EXPORT Size final
{
public:
  // Using the `rule of zero` to this aggregate type
  // C++20 changes the definition of aggregate such that classes with any user-declared ctors are no longer aggregates.

  /** Standard class type aliases. */
  using Self = Size;

  /** Compatible Size and value type alias */
  using SizeType = Size<VDimension>;
  using SizeValueType = ::itk::SizeValueType;

  /** Dimension constant */
  static constexpr unsigned int Dimension = VDimension;

  /** Get the dimension. */
  static constexpr unsigned int
  GetSizeDimension()
  {
    return VDimension;
  }

  /** Add two sizes.  */
  const Self
  operator+(const Self & vec) const
  {
    Self result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] + vec.m_InternalArray[i];
    }
    return result;
  }

  /** Increment size by a size.  */
  const Self &
  operator+=(const Self & vec)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] += vec.m_InternalArray[i];
    }
    return *this;
  }

  /** Subtract two sizes.  */
  const Self
  operator-(const Self & vec) const
  {
    Self result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] - vec.m_InternalArray[i];
    }
    return result;
  }

  /** Decrement size by a size.  */
  const Self &
  operator-=(const Self & vec)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] -= vec.m_InternalArray[i];
    }
    return *this;
  }

  /** Multiply two sizes (elementwise product).  */
  const Self operator*(const Self & vec) const
  {
    Self result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] * vec.m_InternalArray[i];
    }
    return result;
  }

  /** Multiply two sizes (elementwise product).  */
  const Self &
  operator*=(const Self & vec)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] *= vec.m_InternalArray[i];
    }
    return *this;
  }

  /** Get the size. This provides a read only pointer to the size.
   * \sa SetSize */
  const SizeValueType *
  GetSize() const
  {
    return m_InternalArray;
  }

  /** Set the size.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   *  \sa GetSize */
  void
  SetSize(const SizeValueType val[VDimension])
  {
    std::copy_n(val, VDimension, m_InternalArray);
  }

  /** Sets the value of one of the elements.
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed.
   * \sa SetSize()
   * \sa GetElement() */
  void
  SetElement(unsigned long element, SizeValueType val)
  {
    m_InternalArray[element] = val;
  }

  /** Gets the value of one of the elements.
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed
   * \sa GetSize()
   * \sa SetElement() */
  SizeValueType
  GetElement(unsigned long element) const
  {
    return m_InternalArray[element];
  }

  /** Set one value for the index in all dimensions.  Useful for initializing
   * an offset to zero. */
  void
  Fill(SizeValueType value)
  {
    std::fill_n(begin(), size(), value);
  } // MATCH std::array assign, ITK Fill

  /** Size is an "aggregate" class.  Its data is public (m_InternalArray)
   * allowing for fast and convenient instantiations/assignments.
   * ( See main class documentation for an example of initialization)
   */
  /*
   *  Ask the compiler to align a type to the maximum useful alignment for the target
   *  machine you are compiling for. Whenever you leave out the alignment factor in an
   *  aligned attribute specification, the compiler automatically sets the alignment
   *  for the type to the largest alignment that is ever used for any data type on
   *  the target machine you are compiling for. Doing this can often make copy
   *  operations more efficient, because the compiler can use whatever instructions
   *  copy the biggest chunks of memory when performing copies to or from the variables
   *  that have types that you have aligned this way.
   */
  static_assert(VDimension > 0, "Error: Only positive value sized VDimension allowed");
  alignas(SizeValueType) SizeValueType m_InternalArray[VDimension];

  // ======================= Mirror the access pattern behavior of the std::array class
  /**
   * Mirror the std::array type aliases and member function
   * so that the Size class can be treated as a container
   * class in a way that is similar to the std::array.
   */
  using value_type = ::itk::SizeValueType;
  using reference = value_type &;
  using const_reference = const value_type &;
  using iterator = value_type *;
  using const_iterator = const value_type *;
  using size_type = unsigned int;
  using difference_type = std::ptrdiff_t;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  /**
   * Mirror behavior of the std::array manipulations
   * See std::array for documentation on these methods
   */
  void
  assign(const value_type & newValue)
  {
    std::fill_n(begin(), size(), newValue);
  }

  void
  swap(Size & other)
  {
    std::swap(m_InternalArray, other.m_InternalArray);
  }

  iterator
  begin()
  {
    return iterator(&m_InternalArray[0]);
  }

  const_iterator
  begin() const
  {
    return const_iterator(&m_InternalArray[0]);
  }

  iterator
  end()
  {
    return iterator(&m_InternalArray[VDimension]);
  }

  const_iterator
  end() const
  {
    return const_iterator(&m_InternalArray[VDimension]);
  }

  reverse_iterator
  rbegin()
  {
    return reverse_iterator(end());
  }

  const_reverse_iterator
  rbegin() const
  {
    return const_reverse_iterator(end());
  }

  reverse_iterator
  rend()
  {
    return reverse_iterator(begin());
  }

  const_reverse_iterator
  rend() const
  {
    return const_reverse_iterator(begin());
  }

  constexpr size_type
  size() const
  {
    return VDimension;
  }

  constexpr size_type
  max_size() const
  {
    return VDimension;
  }

  constexpr bool
  empty() const
  {
    return false;
  }

  reference operator[](size_type pos) { return m_InternalArray[pos]; }

  const_reference operator[](size_type pos) const { return m_InternalArray[pos]; }

  reference
  at(size_type pos)
  {
    ExceptionThrowingBoundsCheck(pos);
    return m_InternalArray[pos];
  }

  const_reference
  at(size_type pos) const
  {
    ExceptionThrowingBoundsCheck(pos);
    return m_InternalArray[pos];
  }

  reference
  front()
  {
    return *begin();
  }

  const_reference
  front() const
  {
    return *begin();
  }

  reference
  back()
  {
    return VDimension ? *(end() - 1) : *end();
  }

  const_reference
  back() const
  {
    return VDimension ? *(end() - 1) : *end();
  }

  SizeValueType *
  data()
  {
    return &m_InternalArray[0];
  }

  const SizeValueType *
  data() const
  {
    return &m_InternalArray[0];
  }

private:
  void
  ExceptionThrowingBoundsCheck(size_type pos) const
  {
    if (pos >= VDimension)
    {
      throw std::out_of_range("array::ExceptionThrowingBoundsCheck");
    }
  }

public:
  /** Returns a Size object, filled with the specified value for each element.
   */
  static Self
  Filled(const SizeValueType value)
  {
    Self result;
    result.Fill(value);
    return result;
  }

}; //------------ End struct Size


template <unsigned int VDimension>
std::ostream &
operator<<(std::ostream & os, const Size<VDimension> & obj)
{
  os << "[";
  for (unsigned int i = 0; i + 1 < VDimension; ++i)
  {
    os << obj[i] << ", ";
  }
  if (VDimension >= 1)
  {
    os << obj[VDimension - 1];
  }
  os << "]";
  return os;
}

// ======================= Mirror the access pattern behavior of the std::array class
// Array comparisons.
template <unsigned int VDimension>
inline bool
operator==(const Size<VDimension> & one, const Size<VDimension> & two)
{
  return std::equal(one.begin(), one.end(), two.begin());
}

template <unsigned int VDimension>
inline bool
operator!=(const Size<VDimension> & one, const Size<VDimension> & two)
{
  return !(one == two);
}

template <unsigned int VDimension>
inline bool
operator<(const Size<VDimension> & one, const Size<VDimension> & two)
{
  return std::lexicographical_compare(one.begin(), one.end(), two.begin(), two.end());
}

template <unsigned int VDimension>
inline bool
operator>(const Size<VDimension> & one, const Size<VDimension> & two)
{
  return two < one;
}

template <unsigned int VDimension>
inline bool
operator<=(const Size<VDimension> & one, const Size<VDimension> & two)
{
  return !(one > two);
}

template <unsigned int VDimension>
inline bool
operator>=(const Size<VDimension> & one, const Size<VDimension> & two)
{
  return !(one < two);
}

// Specialized algorithms [6.2.2.2].
template <unsigned int VDimension>
inline void
swap(Size<VDimension> & one, Size<VDimension> & two)
{
  std::swap(one.m_InternalArray, two.m_InternalArray);
}

// static constexpr definition explicitly needed in C++11
template <unsigned int VDimension>
constexpr unsigned int Size<VDimension>::Dimension;

} // end namespace itk

#endif
