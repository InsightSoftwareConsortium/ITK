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
#ifndef itkIndex_h
#define itkIndex_h

#include "itkOffset.h"

namespace itk
{

/** \struct Index
 * \brief Represent a n-dimensional index in a n-dimensional image.
 *
 * Index is a templated class to represent a multi-dimensional index,
 * i.e. (i,j,k,...). Index is templated over the dimension of the index.
 * ITK assumes the first element of an index is the fastest moving index.
 *
 * For efficiency sake, Index does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * Index is an "aggregate" class.  Its data is public (m_InternalArray)
 * allowing for fast and convenient instantiations/assignments.
 *
 * The following syntax for assigning an aggregate type like this is allowed/suggested:
 *
 *
 * Index<3> var{{ 256, 256, 20 }}; // Also prevent narrowing conversions
 * Index<3> var = {{ 256, 256, 20 }};
 *
 * The doubled braces {{ and }} are required to prevent `gcc -Wall'
 * (and perhaps other compilers) from complaining about a partly
 * bracketed initializer.
 *
 * As an aggregate type that is intended to provide highest performance
 * characteristics, this class is not appropriate to inherit from,
 * so setting this struct as final.
 *
 * \ingroup ImageAccess
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/DistanceBetweenIndices,Distance between two indices}
 * \sphinxexample{Core/Common/CreateAIndex,Create A Index}
 * \endsphinx
 */

template <unsigned int VDimension = 2>
struct ITK_TEMPLATE_EXPORT Index final
{
public:
  // Using the `rule of zero` to this aggregate type
  // C++20 changes the definition of aggregate such that classes with any user-declared ctors are no longer aggregates.

  /** Standard class type aliases. */
  using Self = Index;

  /** Compatible Index and value type alias */
  using IndexType = Index<VDimension>;
  using IndexValueType = ::itk::IndexValueType;

  /** Compatible Size type alias. */
  using SizeType = Size<VDimension>;

  /** Compatible Offset and Offset value type alias. */
  using OffsetType = Offset<VDimension>;
  using OffsetValueType = ::itk::OffsetValueType;

  /** Dimension constant */
  static constexpr unsigned int Dimension = VDimension;

  /** Get the dimension. */
  static constexpr unsigned int
  GetIndexDimension()
  {
    return VDimension;
  }


  /** Add a size to an index.  */
  const Self
  operator+(const SizeType & sz) const
  {
    Self result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] + static_cast<IndexValueType>(sz[i]);
    }
    return result;
  }

  /** Increment index by a size.  */
  const Self &
  operator+=(const SizeType & sz)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] += static_cast<IndexValueType>(sz[i]);
    }
    return *this;
  }

  /** Subtract a size from an index.
   */
  const Self
  operator-(const SizeType & sz) const
  {
    Self result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] - static_cast<IndexValueType>(sz[i]);
    }
    return result;
  }

  /** Decrement index by a size.  */
  const Self &
  operator-=(const SizeType & sz)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] -= static_cast<IndexValueType>(sz[i]);
    }
    return *this;
  }

  /** Add an offset to an index. */
  const Self
  operator+(const OffsetType & offset) const
  {
    Self result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] + offset[i];
    }
    return result;
  }

  /** Increment index by an offset.  */
  const Self &
  operator+=(const OffsetType & offset)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] += offset[i];
    }
    return *this;
  }

  /** Decrement index by an offset.  */
  const Self &
  operator-=(const OffsetType & offset)
  {
    for (unsigned int i = 0; i < VDimension; i++)
    {
      m_InternalArray[i] -= offset[i];
    }
    return *this;
  }

  /** Subtract an offset from an index. */
  const Self
  operator-(const OffsetType & off) const
  {
    Self result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] - off.m_InternalArray[i];
    }
    return result;
  }

  /** Subtract two indices.  */
  const OffsetType
  operator-(const Self & vec) const
  {
    OffsetType result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] - vec.m_InternalArray[i];
    }
    return result;
  }

  /**
   * Multiply an index by a size (elementwise product).
   */
  const Self operator*(const SizeType & vec) const
  {
    Self result;

    for (unsigned int i = 0; i < VDimension; i++)
    {
      result[i] = m_InternalArray[i] * static_cast<IndexValueType>(vec.m_InternalArray[i]);
    }
    return result;
  }

  /** Get the index. This provides a read only pointer to the index.
   * \sa SetIndex() */
  const IndexValueType *
  GetIndex() const
  {
    return m_InternalArray;
  }

  /** Set the index.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   * \sa GetIndex() */
  void
  SetIndex(const IndexValueType val[VDimension])
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
  SetElement(unsigned long element, IndexValueType val)
  {
    m_InternalArray[element] = val;
  }

  /** Gets the value of one of the elements.
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed
   * \sa GetIndex()
   * \sa SetElement() */
  IndexValueType
  GetElement(unsigned long element) const
  {
    return m_InternalArray[element];
  }

  /** Set one value for the index in all dimensions.  Useful for initializing
   * an offset to zero. */
  void
  Fill(IndexValueType value)
  {
    std::fill_n(begin(), size(), value);
  } // MATCH std::array assign, ITK Fill

  /** Index is an "aggregate" class.  Its data is public (m_InternalArray)
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
  alignas(IndexValueType) IndexValueType m_InternalArray[VDimension];

  /** Copy values from a FixedArray by rounding each one of the components */
  template <typename TCoordRep>
  inline void
  CopyWithRound(const FixedArray<TCoordRep, VDimension> & point)
  {
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      m_InternalArray[i] = Math::Round<IndexValueType>(point[i]);
    }
  }

  /** Copy values from a FixedArray by casting each one of the components */
  template <typename TCoordRep>
  inline void
  CopyWithCast(const FixedArray<TCoordRep, VDimension> & point)
  {
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      m_InternalArray[i] = static_cast<IndexValueType>(point[i]);
    }
  }

  /** Return a basis vector of the form [0, ..., 0, 1, 0, ... 0] where the "1"
   * is positioned in the location specified by the parameter "dim". Valid
   * values of "dim" are 0, ..., VDimension-1. */
  static Self
  GetBasisIndex(unsigned int dim);


  // ======================= Mirror the access pattern behavior of the std::array class
  /**
   * Mirror the std::array type aliases and member function
   * so that the Index class can be treated as a container
   * class in a way that is similar to the std::array.
   */
  using value_type = ::itk::IndexValueType;
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
  swap(Index & other)
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

  IndexValueType *
  data()
  {
    return &m_InternalArray[0];
  }

  const IndexValueType *
  data() const
  {
    return &m_InternalArray[0];
  }


  /** Returns an Index object, filled with the specified value for each element.
   */
  static Self
  Filled(const IndexValueType value)
  {
    Self result;
    result.Fill(value);
    return result;
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

}; //------------ End struct Index

template <unsigned int VDimension>
Index<VDimension>
Index<VDimension>::GetBasisIndex(unsigned int dim)
{
  Self ind{ { 0 } };

  ind.m_InternalArray[dim] = 1;
  return ind;
}

template <unsigned int VDimension>
std::ostream &
operator<<(std::ostream & os, const Index<VDimension> & obj)
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
operator==(const Index<VDimension> & one, const Index<VDimension> & two)
{
  return std::equal(one.begin(), one.end(), two.begin());
}

template <unsigned int VDimension>
inline bool
operator!=(const Index<VDimension> & one, const Index<VDimension> & two)
{
  return !(one == two);
}

template <unsigned int VDimension>
inline bool
operator<(const Index<VDimension> & one, const Index<VDimension> & two)
{
  return std::lexicographical_compare(one.begin(), one.end(), two.begin(), two.end());
}

template <unsigned int VDimension>
inline bool
operator>(const Index<VDimension> & one, const Index<VDimension> & two)
{
  return two < one;
}

template <unsigned int VDimension>
inline bool
operator<=(const Index<VDimension> & one, const Index<VDimension> & two)
{
  return !(one > two);
}

template <unsigned int VDimension>
inline bool
operator>=(const Index<VDimension> & one, const Index<VDimension> & two)
{
  return !(one < two);
}

// Specialized algorithms [6.2.2.2].
template <unsigned int VDimension>
inline void
swap(Index<VDimension> & one, Index<VDimension> & two)
{
  std::swap(one.m_InternalArray, two.m_InternalArray);
}

// static constexpr definition explicitly needed in C++11
template <unsigned int VDimension>
constexpr unsigned int Index<VDimension>::Dimension;
} // end namespace itk

#endif
