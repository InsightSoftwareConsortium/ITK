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

#include <iostream>
#include <type_traits>
#include <array>

#include "itkGTest.h"

#include "itkIndex.h"

namespace {
template<unsigned int VIndexDimension>
std::ostream &operator<<(std::ostream &os,
  const std::array<typename itk::Index<VIndexDimension>::IndexValueType, VIndexDimension> &ind) {
  os << "[";
  for (unsigned int i = 0; i + 1 < VIndexDimension; ++i) {
    os << ind[i] << ", ";
  }
  if (VIndexDimension >= 1) {
    os << ind[VIndexDimension - 1];
  }
  os << "]";
  return os;
}
}

TEST(AllIndex, FourD)
{

  // The following line should throw a compile-time error
  // itk::Index<0> zeroSizedIndex;
  // itk::Index<-3> negativeSizedIndex;

  EXPECT_EQ(std::is_pod< itk::Index<13> >::value, true );

  //TODO:  This class needs better testing, as does
  //       itkSize and itkOffset
  //
  using IndexType = itk::Index<4>;
  using SizeType = IndexType::SizeType;
  //using OffsetType = IndexType::OffsetType;

  IndexType index1 = {{ 10, 20, 30, 40} };

  const IndexType knownInitValues{{ 10,20,30,40 }};
  for( auto i : { 0,1,2,3} )
  {
    EXPECT_EQ( index1[i], knownInitValues[i] );
    EXPECT_EQ( index1.GetElement(i), knownInitValues[i]);
  }

  EXPECT_VECTOR_NEAR(index1, knownInitValues, 0);

  const IndexType knownInitValuesPlus1{{ 11,21,31,41 }};
  const SizeType  knownAll1s = {{ 1, 1, 1, 1 }};

  IndexType index2 = index1;
  EXPECT_VECTOR_NEAR(index2, index1, 0);

  index2 += knownAll1s;
  EXPECT_VECTOR_NEAR(index2, knownInitValuesPlus1, 0);

  IndexType index3 = index1 + knownAll1s;
  EXPECT_VECTOR_NEAR(index3, knownInitValuesPlus1, 0);

  index3 = index2 - knownAll1s;
  EXPECT_VECTOR_NEAR(index3, index1, 0);

  index2 -= knownAll1s;
  EXPECT_VECTOR_NEAR(index2, index1, 0);

  const SizeType knownAll2s{{ 2,2,2,2 }};

  index1.Fill(2);   // Use ITK syntax
  EXPECT_VECTOR_NEAR(index1, knownAll2s, 0);

  const IndexType knownAll4s{{ 4,4,4,4 }};

  index3 = index1 * knownAll2s;
  EXPECT_VECTOR_NEAR(index3, knownAll4s, 0);

  index2 = index1;
  EXPECT_EQ( index1 == index2 , true);

  const IndexType zeroBasis = {{ 1, 0, 0, 0 }};
  const IndexType oneBasis = {{ 0, 1, 0, 0 }};
  const IndexType twoBasis = {{ 0, 0, 1, 0 }};
  const IndexType threeBasis = {{ 0, 0, 0, 1 }};
  EXPECT_VECTOR_NEAR( IndexType::GetBasisIndex( 0 ), zeroBasis, 0 );
  EXPECT_VECTOR_NEAR( IndexType::GetBasisIndex( 1 ), oneBasis, 0 );
  EXPECT_VECTOR_NEAR( IndexType::GetBasisIndex( 2 ), twoBasis, 0 );
  EXPECT_VECTOR_NEAR( IndexType::GetBasisIndex( 3 ), threeBasis, 0 );
}
