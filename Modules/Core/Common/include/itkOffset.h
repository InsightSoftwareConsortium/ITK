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
#ifndef itkOffset_h
#define itkOffset_h

#include "itkSize.h"
#include "itkMath.h"

namespace itk
{

/**
 * \struct Offset
 * \brief Represent a n-dimensional offset between two n-dimensional indexes of n-dimensional image.
 *
 * Offset is a templated class to represent a multi-dimensional offset,
 * i.e. (i,j,k,...). Offset is templated over the dimension of the space.
 * ITK assumes the first element of a size (bounds) is the fastest moving index.
 *
 * For efficiency, Offset does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * Offset is an "aggregate" class.  Its data is public (m_InternalArray)
 * allowing for fast and convenient instantiations/assignments.
 *
 * The following syntax for assigning an aggregate type like this is allowed/suggested:
 *
 *
 * Offset<3> var{{ 256, 256, 20 }}; // Also prevent narrowing conversions
 * Offset<3> var = {{ 256, 256, 20 }};
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
 * \ingroup ImageAccess
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/AddOffsetToIndex,Add Offset To Index}
 * \endsphinx
 */

template <unsigned int VDimension = 2>
struct ITK_TEMPLATE_EXPORT Offset final
{
public:
  // Using the `rule of zero` to this aggregate type
  // C++20 changes the definition of aggregate such that classes with any user-declared ctors are no longer aggregates.

  /** Standard class type aliases. */
  using Self = Offset;

  /** Compatible Offset and value type alias. */
  using OffsetType = Offset<VDimension>;
  using OffsetValueType = ::itk::OffsetValueType;

  /** Dimension constant */
  static constexpr unsigned int Dimension = VDimension;

  /** Get the dimension (size) of the index. */
  static constexpr unsigned int
  GetOffsetDimension()
  {
    return VDimension;
  }


  /** Add two offsets. */
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

  /** Add a size to an offset.  */
  const Self
  operator+(const Size<VDimension> & sz) const
  {
    Self result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] + sz[i];
    }
    return result;
  }

  /** Increment index by a size.  */
  const Self &
  operator+=(const Size<VDimension> & sz)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] += sz[i];
    }
    return *this;
  }

  /** Decrement index by a size.  */
  const Self &
  operator-=(const Size<VDimension> & sz)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] -= sz[i];
    }
    return *this;
  }

  /** Subtract two offsets. */
  const Self
  operator-(const Self & vec)
  {
    Self result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] - vec.m_InternalArray[i];
    }
    return result;
  }

  /** Increment offset by an offset.  */
  const Self &
  operator+=(const Self & vec)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] += vec.m_InternalArray[i];
    }
    return *this;
  }

  /** Decrement offset by an offset.  */
  const Self &
  operator-=(const Self & vec)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] -= vec.m_InternalArray[i];
    }
    return *this;
  }


  /** Get the offset. This provides a read only pointer to the offset.
   * \sa SetOffset() */
  const OffsetValueType *
  GetOffset() const
  {
    return m_InternalArray;
  }

  /** Set the index.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   * \sa GetOffset() */
  void
  SetOffset(const OffsetValueType val[VDimension])
  {
    std::copy_n(val, VDimension, m_InternalArray);
  }

  /** Sets the value of one of the elements.
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed.
   * \sa SetIndex()
   * \sa GetElement() */
  void
  SetElement(unsigned long element, OffsetValueType val)
  {
    m_InternalArray[element] = val;
  }

  /** Gets the value of one of the elements.
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed
   * \sa GetIndex()
   * \sa SetElement() */
  OffsetValueType
  GetElement(unsigned long element) const
  {
    return m_InternalArray[element];
  }

  /** Set one value for the offset in all dimensions.  Useful for initializing
   * an offset to zero. */
  void
  Fill(OffsetValueType value)
  {
    std::fill_n(begin(), size(), value);
  } // MATCH std::array assign, ITK Fill

  /** Offset is an "aggregate" class.  Its data is public (m_InternalArray)
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
  alignas(OffsetValueType) OffsetValueType m_InternalArray[VDimension];

  /** Copy values from a FixedArray by rounding each one of the components */
  template <typename TCoordRep>
  inline void
  CopyWithRound(const FixedArray<TCoordRep, VDimension> & point)
  {
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      m_InternalArray[i] = Math::Round<OffsetValueType>(point[i]);
    }
  }

  /** Copy values from a FixedArray by casting each one of the components */
  template <typename TCoordRep>
  inline void
  CopyWithCast(const FixedArray<TCoordRep, VDimension> & point)
  {
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      m_InternalArray[i] = static_cast<OffsetValueType>(point[i]);
    }
  }

  /** Return a basis vector of the form [0, ..., 0, 1, 0, ... 0] where the "1"
   * is positioned in the location specified by the parameter "dim". Valid
   * values of "dim" are 0, ..., VDimension-1. */
  static Self
  GetBasisOffset(unsigned int dim);


  // ======================= Mirror the access pattern behavior of the std::array class
  /**
   * Mirror the std::array type aliases and member function
   * so that the Offset class can be treated as a container
   * class in a way that is similar to the std::array.
   */
  using value_type = ::itk::OffsetValueType;
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
  swap(Offset & other)
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

  OffsetValueType *
  data()
  {
    return &m_InternalArray[0];
  }

  const OffsetValueType *
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

}; //------------ End struct Offset

template <unsigned int VDimension>
Offset<VDimension>
Offset<VDimension>::GetBasisOffset(unsigned int dim)
{
  Self ind;

  memset(ind.m_InternalArray, 0, sizeof(OffsetValueType) * VDimension);
  ind.m_InternalArray[dim] = 1;
  return ind;
}

template <unsigned int VDimension>
std::ostream &
operator<<(std::ostream & os, const Offset<VDimension> & ind)
{
  os << "[";
  unsigned int dimlim = VDimension - 1;
  for (unsigned int i = 0; i < dimlim; ++i)
  {
    os << ind[i] << ", ";
  }
  if (VDimension >= 1)
  {
    os << ind[VDimension - 1];
  }
  os << "]";
  return os;
}

// ======================= Mirror the access pattern behavior of the std::array class
// Array comparisons.
template <unsigned int VDimension>
inline bool
operator==(const Offset<VDimension> & one, const Offset<VDimension> & two)
{
  return std::equal(one.begin(), one.end(), two.begin());
}

template <unsigned int VDimension>
inline bool
operator!=(const Offset<VDimension> & one, const Offset<VDimension> & two)
{
  return !(one == two);
}

template <unsigned int VDimension>
inline bool
operator<(const Offset<VDimension> & one, const Offset<VDimension> & two)
{
  return std::lexicographical_compare(one.begin(), one.end(), two.begin(), two.end());
}

template <unsigned int VDimension>
inline bool
operator>(const Offset<VDimension> & one, const Offset<VDimension> & two)
{
  return two < one;
}

template <unsigned int VDimension>
inline bool
operator<=(const Offset<VDimension> & one, const Offset<VDimension> & two)
{
  return !(one > two);
}

template <unsigned int VDimension>
inline bool
operator>=(const Offset<VDimension> & one, const Offset<VDimension> & two)
{
  return !(one < two);
}

// Specialized algorithms [6.2.2.2].
template <unsigned int VDimension>
inline void
swap(Offset<VDimension> & one, Offset<VDimension> & two)
{
  std::swap(one.m_InternalArray, two.m_InternalArray);
}

// static constexpr definition explicitly needed in C++11
template <unsigned int VDimension>
constexpr unsigned int Offset<VDimension>::Dimension;

} // end namespace itk

#endif
