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


/*
 * Tests Covered for the Aggregate types of (I-Index, S-Size, O-Offset)
 * This provides a comprehensive testing of all features of the
 * classes that are intended to mimic the std::array class.
================================================================

ISO  static constexpr unsigned int Dimension = VDimension;
ISO  static constexpr unsigned int GetIndexDimension()
ISO  void SetElement(unsigned long element, IndexValueType val)
ISO  IndexValueType GetElement(unsigned long element) const
ISO  void Fill(IndexValueType value)
ISO  alignas(IndexValueType) IndexValueType m_InternalArray[VDimension];

  // ======================= Mirror the access pattern behavior of the std::array class
ISO  void assign(const value_type & newValue)
ISO  void swap(Index & other)
tested in swap  iterator begin()
tested in swap  iterator end()
tested in operator= const_iterator begin() const
tested in operator= const_iterator end() const

ISO  reverse_iterator rbegin()
ISO  reverse_iterator rend()
ISO  const_reverse_iterator rbegin() const
ISO  const_reverse_iterator rend() const

ISO constexpr size_type size() const
ISO constexpr size_type max_size() const
ISO constexpr bool empty() const
ISO reference operator[](size_type pos)
ISO const_reference operator[](size_type pos) const

ISO reference at(size_type pos)
ISO const_reference at(size_type pos) const

ISO reference front()
ISO const_reference front() const

ISO reference back()
ISO const_reference back() const

ISO IndexValueType * data()
ISO const IndexValueType * data() const
ISO template <unsigned int VDimension> std::ostream & operator<<(std::ostream & os, const Index<VDimension> & obj)

// ======================= Mirror the access pattern behavior of the std::array class
// Array comparisons.
ISO inline bool operator==(const Index<VDimension> & one, const Index<VDimension> & two)
ISO inline bool operator!=(const Index<VDimension> & one, const Index<VDimension> & two)
ISO inline bool operator<(const Index<VDimension> & one, const Index<VDimension> & two)
ISO inline bool operator>(const Index<VDimension> & one, const Index<VDimension> & two)
ISO inline bool operator<=(const Index<VDimension> & one, const Index<VDimension> & two)
ISO inline bool operator>=(const Index<VDimension> & one, const Index<VDimension> & two)
ISO inline void swap(Index<VDimension> & one, Index<VDimension> & two)

Index & Offset types
IO  const Self operator+(const SizeType & size) const
IO  const Self & operator+=(const SizeType & size)
IO  const Self operator-(const SizeType & size) const
IO  const Self & operator-=(const SizeType & size)
IO  const Self operator*(const SizeType & vec) const

IO  const Self operator+(const OffsetType & offset) const
IO  const Self & operator+=(const OffsetType & offset)
IO  const Self & operator-=(const OffsetType & offset)
IO  const Self operator-(const OffsetType & off) const

IO  const OffsetType operator-(const Self & vec) const
IO  inline void CopyWithRound(const FixedArray<TCoordRep, VDimension> & point)
IO  inline void CopyWithCast(const FixedArray<TCoordRep, VDimension> & point)

Index
I  void SetIndex(const IndexValueType val[VDimension])
I  static Self GetBasisIndex(unsigned int dim);

Offset
O  void SetIndex(const IndexValueType val[VDimension])
O  static Self GetBasisIndex(unsigned int dim);

Size
S  void SetIndex(const IndexValueType val[VDimension])
*/

#include <iostream>
#include <array>
#include <type_traits>

#include "itkGTest.h"

#include "itkIndex.h"
#include "itkSize.h"
#include "itkOffset.h"

/*
 * The following line should throw a compile-time error
 * because only positive Index sizes are supported.
 */
// itk::Index<0> zeroSizedIndex;
// itk::Index<-3> negativeSizedIndex;

namespace
{

template <class AggregateType>
class CommonTests
{
public:
  void
  doTest()
  {
    const AggregateType knownInitValues{ { 10, 20, 30, 40 } };

    const AggregateType knownAll4s{ { 4, 4, 4, 4 } };
    const AggregateType knownAll7s{ { 7, 7, 7, 7 } };
    const AggregateType knownAll6s{ { 6, 6, 6, 6 } };

    EXPECT_EQ(std::is_pod<AggregateType>::value, true);
    EXPECT_EQ(AggregateType::Dimension, 4);

    AggregateType index1 = { { 10, 20, 30, 40 } };
    EXPECT_EQ(index1.size(), 4);
    EXPECT_EQ(index1.max_size(), 4);
    EXPECT_EQ(index1.empty(), false);

    for (auto i : { 0, 1, 2, 3 })
    {
      EXPECT_EQ(index1[i], knownInitValues[i]);
      EXPECT_EQ(index1.GetElement(i), knownInitValues[i]);
      EXPECT_EQ(index1.at(i), knownInitValues[i]);
    }

    EXPECT_THROW(index1.at(10), std::out_of_range);

    index1[0] = 6;                 // non-const operator[]
    index1.at(1) = 6;              // non-const at()
    index1.m_InternalArray[2] = 6; // Direct access
    index1.SetElement(3, 6);       // SetElement
    ITK_EXPECT_VECTOR_NEAR(index1, knownAll6s, 0);

    AggregateType index2 = index1; // Copy constructor
    ITK_EXPECT_VECTOR_NEAR(index2, index1, 0);

    index2.assign(4); // Use std::array like syntax
    ITK_EXPECT_VECTOR_NEAR(index2, knownAll4s, 0);
    index2.front() = 6;   // Non const ref
    index2.data()[1] = 6; // Test non-const data
    index2.data()[2] = 6;
    index2.back() = 6; // Non const ref
    ITK_EXPECT_VECTOR_NEAR(index2, knownAll6s, 0);
    index2.Fill(7);
    EXPECT_EQ(index2.back(), 7);
    EXPECT_EQ(index2.back(), 7);
    EXPECT_EQ(index2.data()[3], 7); // Test const data access

    index2.swap(index1);
    ITK_EXPECT_VECTOR_NEAR(index1, knownAll7s, 0);
    swap(index1, index2);
    ITK_EXPECT_VECTOR_NEAR(index1, knownAll6s, 0);

    {
      auto rit = knownInitValues.rbegin(); // test const reverse_iterator
      auto it = index2.rbegin();           // test reverse_iterator
      while (rit != knownInitValues.rend() && it != index2.rend())
      {
        *it = *rit;
        ++rit;
        ++it;
      }
      ITK_EXPECT_VECTOR_NEAR(index2, knownInitValues, 0);
    }

    index2.Fill(6);
    ITK_EXPECT_VECTOR_NEAR(index2, knownAll6s, 0);

    index2 = index1;
    EXPECT_EQ(index2 == index1, true);
    EXPECT_EQ(index2 <= index1, true);
    EXPECT_EQ(index2 >= index1, true);

    const AggregateType knowValuesSmaller{ { 1, 1, 0, 1 } };
    const AggregateType knowValuesLarger{ { 1, 1, 1, 1 } };
    EXPECT_EQ(knowValuesSmaller < knowValuesLarger, true);
    EXPECT_EQ(knowValuesSmaller > knowValuesLarger, false);
    EXPECT_EQ(knowValuesSmaller == knowValuesLarger, false);
    EXPECT_EQ(knowValuesSmaller != knowValuesLarger, true);

    EXPECT_EQ(knowValuesSmaller <= knowValuesLarger, true);
    EXPECT_EQ(knowValuesSmaller >= knowValuesLarger, false);

    std::stringstream ss;
    ss << knowValuesLarger;
    EXPECT_EQ(ss.str(), "[1, 1, 1, 1]");
  }
};


template <class AggregateType>
class CommonIndexOffsetMathOps
{
public:
  void
  doTest()
  {
    using SizeType = typename AggregateType::SizeType;
    using OffsetType = itk::Offset<AggregateType::Dimension>;

    const AggregateType knownInitValues{ { 10, 20, 30, 40 } };
    const AggregateType knownInitValuesPlus1{ { 11, 21, 31, 41 } };

    AggregateType index1 = knownInitValues;
    AggregateType index2 = knownInitValues;
    AggregateType index3;

    //============ Test math with Size ====================================
    {
      const SizeType knownAll1s{ { 1, 1, 1, 1 } };
      const SizeType knownAll2s{ { 2, 2, 2, 2 } };
      const SizeType knownAll4s{ { 4, 4, 4, 4 } };

      index2 += knownAll1s; // operator+= SizeType
      ITK_EXPECT_VECTOR_NEAR(index2, knownInitValuesPlus1, 0);

      index3 = index1 + knownAll1s; // operator+ SizeType
      ITK_EXPECT_VECTOR_NEAR(index3, knownInitValuesPlus1, 0);

      index3 = index2 - knownAll1s; // operator- SizeType
      ITK_EXPECT_VECTOR_NEAR(index3, index1, 0);

      index2 -= knownAll1s; // operator-= SizeType
      ITK_EXPECT_VECTOR_NEAR(index2, index1, 0);

      index1.Fill(2); // Use ITK syntax
      ITK_EXPECT_VECTOR_NEAR(index1, knownAll2s, 0);

      index3 = index1 * knownAll2s;
      ITK_EXPECT_VECTOR_NEAR(index3, knownAll4s, 0);

      index3 = index1 * knownAll2s;
      ITK_EXPECT_VECTOR_NEAR(index3, knownAll4s, 0);
    }

    //============ Test math with Offsets ====================================
    {
      const OffsetType knownAll1sOffset{ { 1, 1, 1, 1 } };

      index2 = knownInitValues;
      index2 += knownAll1sOffset; // operator+= SizeType
      ITK_EXPECT_VECTOR_NEAR(index2, knownInitValuesPlus1, 0);

      index1 = knownInitValues;
      index3 = index1 + knownAll1sOffset; // operator+ SizeType
      ITK_EXPECT_VECTOR_NEAR(index3, knownInitValuesPlus1, 0);

      index3 = index2 - knownAll1sOffset; // operator- SizeType
      ITK_EXPECT_VECTOR_NEAR(index3, index1, 0);

      index2 -= knownAll1sOffset; // operator-= SizeType
      ITK_EXPECT_VECTOR_NEAR(index2, index1, 0);
    }

    //============ Test math with Aggregate Type ====================================
    {
      const AggregateType                      knownAll2sAgg{ { 2, 2, 2, 2 } };
      const AggregateType                      knownAll4sAgg{ { 4, 4, 4, 4 } };
      const typename AggregateType::OffsetType knownOffset = { { -2, -2, -2, -2 } };
      ITK_EXPECT_VECTOR_NEAR(knownAll2sAgg - knownAll4sAgg, knownOffset, 0);
    }

    //============ Test Copy with Round/Cast Type ====================================
    {
      AggregateType known3s{ { 3, 3, 3, 3 } };
      AggregateType threes;

      threes.Fill(0);
      AggregateType         known4s{ { 4, 4, 4, 4 } };
      itk::Point<double, 4> p1;
      p1.Fill(3.5);
      threes.CopyWithRound(p1);
      ITK_EXPECT_VECTOR_NEAR(threes, known4s, 0);

      threes.Fill(0);
      threes.CopyWithCast(p1);
      ITK_EXPECT_VECTOR_NEAR(threes, known3s, 0);
    }
  }
};

} // namespace


TEST(AllCommonIndexSizeOffset, FourD)
{

  CommonTests<itk::Index<4>> testIndex; // Only dim 4 supported
  testIndex.doTest();
  CommonTests<itk::Size<4>> testSize; // Only dim 4 supported
  testSize.doTest();
  CommonTests<itk::Offset<4>> testOffset; // Only dim 4 supported
  testOffset.doTest();
}

TEST(Specialized, IndexOffset)
{
  using IndexType = itk::Index<4>;
  CommonIndexOffsetMathOps<IndexType> myIndexTester;
  myIndexTester.doTest();
}

TEST(Specialized, Index)
{
  EXPECT_EQ(std::is_pod<itk::Index<13>>::value, true);
  EXPECT_EQ(itk::Index<2>::GetIndexDimension(), 2);

  using IndexType = itk::Index<4>;

  const IndexType zeroBasis = { { 1, 0, 0, 0 } };
  const IndexType oneBasis = { { 0, 1, 0, 0 } };
  const IndexType twoBasis = { { 0, 0, 1, 0 } };
  const IndexType threeBasis = { { 0, 0, 0, 1 } };
  ITK_EXPECT_VECTOR_NEAR(IndexType::GetBasisIndex(0), zeroBasis, 0);
  ITK_EXPECT_VECTOR_NEAR(IndexType::GetBasisIndex(1), oneBasis, 0);
  ITK_EXPECT_VECTOR_NEAR(IndexType::GetBasisIndex(2), twoBasis, 0);
  ITK_EXPECT_VECTOR_NEAR(IndexType::GetBasisIndex(3), threeBasis, 0);

  IndexType                 known3s{ { 3, 3, 3, 3 } };
  IndexType                 threes;
  IndexType::IndexValueType raw3s[4] = { 3, 3, 3, 3 };
  threes.SetIndex(raw3s);
  ITK_EXPECT_VECTOR_NEAR(threes, known3s, 0);
}

TEST(Specialized, Offset)
{

  EXPECT_EQ(std::is_pod<itk::Offset<13>>::value, true);
  EXPECT_EQ(itk::Offset<13>::GetOffsetDimension(), 13);

  using OffsetType = itk::Offset<4>;

  const OffsetType zeroBasis = { { 1, 0, 0, 0 } };
  const OffsetType oneBasis = { { 0, 1, 0, 0 } };
  const OffsetType twoBasis = { { 0, 0, 1, 0 } };
  const OffsetType threeBasis = { { 0, 0, 0, 1 } };
  ITK_EXPECT_VECTOR_NEAR(OffsetType::GetBasisOffset(0), zeroBasis, 0);
  ITK_EXPECT_VECTOR_NEAR(OffsetType::GetBasisOffset(1), oneBasis, 0);
  ITK_EXPECT_VECTOR_NEAR(OffsetType::GetBasisOffset(2), twoBasis, 0);
  ITK_EXPECT_VECTOR_NEAR(OffsetType::GetBasisOffset(3), threeBasis, 0);

  OffsetType                  known3s{ { 3, 3, 3, 3 } };
  OffsetType                  threes;
  OffsetType::OffsetValueType raw3s[4] = { 3, 3, 3, 3 };
  threes.SetOffset(raw3s);
  ITK_EXPECT_VECTOR_NEAR(threes, known3s, 0);
}

TEST(Specialized, Size)
{
  EXPECT_EQ(std::is_pod<itk::Size<13>>::value, true);
  EXPECT_EQ(itk::Size<7>::GetSizeDimension(), 7);

  using SizeType = itk::Size<4>;
  SizeType                known3s{ { 3, 3, 3, 3 } };
  SizeType                threes;
  SizeType::SizeValueType raw3s[4] = { 3, 3, 3, 3 };
  threes.SetSize(raw3s);
  ITK_EXPECT_VECTOR_NEAR(threes, known3s, 0);
}
