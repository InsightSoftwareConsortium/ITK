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

#ifndef itkIndexRange_h
#define itkIndexRange_h

#include <cassert>
#include <cstddef>     // For ptrdiff_t.
#include <iterator>    // For bidirectional_iterator_tag and reverse_iterator.
#include <type_traits> // For conditional and enable_if.

#include "itkImageRegion.h"
#include "itkIndex.h"
#include "itkSize.h"

namespace itk
{

/**
  * \class IndexRange
  *
  * Modern C++11 range, supporting efficient iteration over the indices of
  * an image grid space.
  *
  * The following example prints all indices of an 2-D grid space of size 2x3.
    \code
    constexpr unsigned Dimension = 2;
    const Size<Dimension> size = { {2, 3} };
    const ZeroBasedIndexRange<Dimension> indexRange{ size };

    for (const Index<Dimension> index : indexRange)
    {
      std::cout << index;
    }
    // Output: "[0, 0][1, 0][0, 1][1, 1][0, 2][1, 2]"
    \endcode
  *
  * The indices from IndexRange can also be used as consecutive locations
  * of a ShapedImageNeighborhoodRange, for example:
    \code
    for (const auto index : indexRange)
    {
      shapedImageNeighborhoodRange.SetLocation(index);

      for (const PixelType neighborPixel : shapedImageNeighborhoodRange)
      {
        // Process neighbor pixel...
      }
    }
    \endcode
  *
  * IndexRange is designed to conform to Standard C++ Iterator requirements,
  * so that it can be used in range-based for loop, and its iterators can be
  * passed to Standard C++ algorithms.
  *
  * \author Niels Dekker, LKEB, Leiden University Medical Center
  *
  * \see ShapedImageNeighborhoodRange
  * \ingroup ImageIterators
  * \ingroup ITKCommon
  */
template <unsigned VDimension, bool VBeginAtZero>
class IndexRange final
{
public:
  constexpr static unsigned Dimension = VDimension;
  using SizeType = Size<VDimension>;
  using IndexType = Index<VDimension>;

  class const_iterator final
  {
  public:
    // Types conforming the iterator requirements of the C++ standard library:
    using difference_type = std::ptrdiff_t;
    using value_type = IndexType;
    using reference = const IndexType &;
    using pointer = const IndexType *;
    using iterator_category = std::bidirectional_iterator_tag;

    /** Default-constructor, as required for any C++11 Forward Iterator.
     * \note The other five "special member functions" (copy-constructor,
     * copy-assignment operator, move-constructor, move-assignment operator,
     * and destructor) are defaulted implicitly, following the C++ "Rule of Zero".
     */
    const_iterator() = default;


    /**  Returns a reference to the current index. */
    reference operator*() const ITK_NOEXCEPT { return m_Index; }


    /**  Returns a pointer to the current index. */
    pointer operator->() const ITK_NOEXCEPT { return &(**this); }


    /** Prefix increment ('++it'). */
    const_iterator &
    operator++() ITK_NOEXCEPT
    {
      for (unsigned i = 0; i < (VDimension - 1); ++i)
      {
        auto & indexValue = m_Index[i];

        ++indexValue;

        if (indexValue <= m_MaxIndex[i])
        {
          return *this;
        }
        indexValue = m_MinIndex[i];
      }
      ++m_Index.back();

      return *this;
    }


    /** Postfix increment ('it++').
     * \note Usually prefix increment ('++it') is preferable. */
    const_iterator
    operator++(int) ITK_NOEXCEPT
    {
      auto result = *this;
      ++(*this);
      return result;
    }


    /** Prefix decrement ('--it'). */
    const_iterator &
    operator--() ITK_NOEXCEPT
    {
      for (unsigned i = 0; i < (VDimension - 1); ++i)
      {
        auto & indexValue = m_Index[i];

        --indexValue;

        if (indexValue >= m_MinIndex[i])
        {
          return *this;
        }
        indexValue = m_MaxIndex[i];
      }
      --m_Index.back();
      return *this;
    }


    /** Postfix increment ('it--').
     * \note  Usually prefix increment ('--it') is preferable. */
    const_iterator
    operator--(int) ITK_NOEXCEPT
    {
      auto result = *this;
      --(*this);
      return result;
    }


    /** Returns (it1 == it2) for iterators it1 and it2. Note that these iterators
     * should be from the same range. This operator does not support comparing iterators
     * from different ranges. */
    friend bool
    operator==(const const_iterator & lhs, const const_iterator & rhs) ITK_NOEXCEPT
    {
      assert(lhs.m_MaxIndex == rhs.m_MaxIndex);

      return lhs.m_Index == rhs.m_Index;
    }


    /** Returns (it1 != it2) for iterators it1 and it2. */
    friend bool
    operator!=(const const_iterator & lhs, const const_iterator & rhs) ITK_NOEXCEPT
    {
      // Implemented just like the corresponding std::rel_ops operator.
      return !(lhs == rhs);
    }


    /** Returns (it1 < it2) for iterators it1 and it2. */
    friend bool
    operator<(const const_iterator & lhs, const const_iterator & rhs) ITK_NOEXCEPT
    {
      for (unsigned i = VDimension; i > 0; --i)
      {
        const auto difference = lhs.m_Index[i - 1] - rhs.m_Index[i - 1];

        if (difference < 0)
        {
          return true;
        }
        if (difference > 0)
        {
          break;
        }
      }
      return false;
    }


    /** Returns (it1 > it2) for iterators it1 and it2. */
    friend bool
    operator>(const const_iterator & lhs, const const_iterator & rhs) ITK_NOEXCEPT
    {
      // Implemented just like the corresponding std::rel_ops operator.
      return rhs < lhs;
    }


    /** Returns (it1 <= it2) for iterators it1 and it2. */
    friend bool
    operator<=(const const_iterator & lhs, const const_iterator & rhs) ITK_NOEXCEPT
    {
      // Implemented just like the corresponding std::rel_ops operator.
      return !(rhs < lhs);
    }


    /** Returns (it1 >= it2) for iterators it1 and it2. */
    friend bool
    operator>=(const const_iterator & lhs, const const_iterator & rhs) ITK_NOEXCEPT
    {
      // Implemented just like the corresponding std::rel_ops operator.
      return !(lhs < rhs);
    }


  private:
    friend class IndexRange;

    // Represents an N-dimensional index that is always zero
    // Aims towards zero runtime overhead.
    struct ZeroIndex
    {
      // The "index" operator.
      constexpr IndexValueType operator[](unsigned) const { return 0; }

      // Implicitly converts to a default-initialized itk::Index<N>.
      constexpr operator IndexType() const { return IndexType(); }
    };


    // When BeginAtZero is true, use zero as minimum index, otherwise use itk::Index<N>.
    using MinIndexType = typename std::conditional<VBeginAtZero, ZeroIndex, IndexType>::type;

    // Private constructor, only used by friend class IndexRange.
    const_iterator(const IndexType & index, const MinIndexType & minIndex, const IndexType & maxIndex) ITK_NOEXCEPT
      :
      // Note: Use parentheses instead of curly braces to initialize data members,
      // to avoid AppleClang 6.0.0.6000056 compilation error, "no viable conversion..."
      m_Index(index)
      , m_MinIndex(minIndex)
      , m_MaxIndex(maxIndex)
    {}


    // IndexRange::const_iterator data members:

    // Current (N-dimensional) index.
    IndexType m_Index;

    // Minimum (N-dimensional) index.
    MinIndexType m_MinIndex;

    // Maximum (N-dimensional) index.
    IndexType m_MaxIndex;
  };

  using iterator = const_iterator;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  /** Explicitly defaulted default-constructor. Constructs an empty range.
   * \note The other five "special member functions" (copy-constructor,
   * copy-assignment operator, move-constructor, move-assignment operator,
   * and destructor) are implicitly defaulted, following the C++ "Rule of Zero".
   */
  IndexRange() = default;


  /** Constructs a range of indices for the specified grid size.
   */
  explicit IndexRange(const SizeType & gridSize)
    : // Note: Use parentheses instead of curly braces to initialize data members,
      // to avoid AppleClang 6.0.0.6000056 compile errors, "no viable conversion..."
    m_MinIndex()
    , m_MaxIndex(CalculateMaxIndex(typename iterator::MinIndexType(), gridSize))
  {}


  /** Constructs a range of indices for the specified image region.
   * \note This function is unavailable when VBeginAtZero is true (in
   * case there is a substitution failure, and C++ "SFINAE" kicks in).
   */
  template <bool VIsSubstitutionFailure = VBeginAtZero,
            typename TVoid = typename std::enable_if<!VIsSubstitutionFailure>::type>
  explicit IndexRange(const ImageRegion<VDimension> & imageRegion)
    : // Note: Use parentheses instead of curly braces to initialize data members,
      // to avoid AppleClang 6.0.0.6000056 compile errors, "no viable conversion..."
    m_MinIndex(imageRegion.GetIndex())
    , m_MaxIndex(CalculateMaxIndex(imageRegion.GetIndex(), imageRegion.GetSize()))
  {
    // Three compile-time asserts, just to check if SFINAE worked properly:
    static_assert(!VIsSubstitutionFailure,
                  "This template should (of course) be instantiated without substitution failure.");
    static_assert(std::is_same<TVoid, void>::value,
                  "std::enable_if<!VIsSubstitutionFailure> should yield void, by definition.");
    static_assert(!VBeginAtZero, "This constructor should only be is available when VBeginAtZero is false.");
  }


  /** Returns an iterator to the first index. */
  iterator
  begin() const ITK_NOEXCEPT
  {
    return iterator(m_MinIndex, m_MinIndex, m_MaxIndex);
  }

  /** Returns an 'end iterator' for this range. */
  iterator
  end() const ITK_NOEXCEPT
  {
    IndexType index = m_MinIndex;
    index.back() = m_MaxIndex.back() + 1;
    return iterator(index, m_MinIndex, m_MaxIndex);
  }

  /** Returns a const iterator to the first index.
   * Provides only read-only access to the index data. */
  const_iterator
  cbegin() const ITK_NOEXCEPT
  {
    return this->begin();
  }

  /** Returns a const 'end iterator' for this range. */
  const_iterator
  cend() const ITK_NOEXCEPT
  {
    return this->end();
  }

  /** Returns a reverse 'begin iterator' for this range. */
  reverse_iterator
  rbegin() const ITK_NOEXCEPT
  {
    return reverse_iterator(this->end());
  }

  /** Returns a reverse 'end iterator' for this range. */
  reverse_iterator
  rend() const ITK_NOEXCEPT
  {
    return reverse_iterator(this->begin());
  }

  /** Returns a const reverse 'begin iterator' for this range. */
  const_reverse_iterator
  crbegin() const ITK_NOEXCEPT
  {
    return this->rbegin();
  }

  /** Returns a const reverse 'end iterator' for this range. */
  const_reverse_iterator
  crend() const ITK_NOEXCEPT
  {
    return this->rend();
  }


  /** Returns the size of the range, that is the number of indices. */
  std::size_t
  size() const ITK_NOEXCEPT
  {
    std::size_t result = 1;

    for (unsigned i = 0; i < VDimension; ++i)
    {
      result *= ((m_MaxIndex[i] + 1) - m_MinIndex[i]);
    }
    return result;
  }


  /** Tells whether the range is empty. */
  bool
  empty() const ITK_NOEXCEPT
  {
    // When an IndexRange is empty, each index value of m_MaxIndex is less than the corresponding
    // index value of m_MinIndex. And vise versa: when an IndexRange is non-empty, each index value
    // of m_MaxIndex is greater than or equal to the corresponding index value of m_MinIndex.
    // Note that the range contains one element when m_MaxIndex == m_MinIndex.
    return m_MaxIndex[0] < m_MinIndex[0];
  }


private:
  using MinIndexType = typename iterator::MinIndexType;

  static IndexType
  CalculateMaxIndex(const MinIndexType & minIndex, const SizeType & size)
  {
    const bool sizeHasZeroValue = [&size] {
      for (const auto sizeValue : size)
      {
        if (sizeValue == 0)
        {
          return true;
        }
      }
      return false;
    }();

    // Treat any size that has a zero value equally.
    const SizeType normalizedSize = sizeHasZeroValue ? SizeType{ { 0 } } : size;

    IndexType index;

    for (unsigned i = 0; i < VDimension; ++i)
    {
      index[i] = minIndex[i] + static_cast<IndexValueType>(normalizedSize[i]) - 1;
    }

    return index;
  }

  // IndexRange data members:

  // Minimum (N-dimensional) index.
  MinIndexType m_MinIndex = MinIndexType();

  // Maximum (N-dimensional) index.
  IndexType m_MaxIndex = IndexType::Filled(-1);
};

template <unsigned VDimension>
using ImageRegionIndexRange = IndexRange<VDimension, false>;

template <unsigned VDimension>
using ZeroBasedIndexRange = IndexRange<VDimension, true>;

} // namespace itk

#endif
