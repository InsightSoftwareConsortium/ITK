/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

// First include the header file to be tested:
#include "itkArray.h"
#include "itkNumericTraits.h"
#include <gtest/gtest.h>
#include <iostream>

namespace
{
// Checks that `itk::Array` supports class template argument deduction (CTAD).
template <typename TValue>
constexpr bool
CheckClassTemplateArgumentDeduction()
{
  using ExpectedType = itk::Array<TValue>;

  const TValue constValue{};
  static_assert(std::is_same_v<decltype(itk::Array(&constValue, 1)), ExpectedType>,
                "The `Array(const ValueType *, ...)` constructor should support CTAD!");

  TValue nonConstValue{};
  static_assert(std::is_same_v<decltype(itk::Array(&nonConstValue, 1)), ExpectedType>,
                "The `Array(ValueType *, ...)` constructor should support CTAD!");
  return true;
}
} // namespace


static_assert(CheckClassTemplateArgumentDeduction<int>() && CheckClassTemplateArgumentDeduction<float>());


TEST(Array, MemoryManagement)
{
  using FloatArrayType = itk::Array<float>;
  using DoubleArrayType = itk::Array<double>;

  const FloatArrayType  fa(10);
  const DoubleArrayType da(10);

  // Create an itk::Array which manages its own memory
  FloatArrayType myOwnBoss;
  myOwnBoss.SetSize(5);
  myOwnBoss.Fill(2.0 + 1.0f / 3.0f);
  myOwnBoss[0] = 2.0f / 3.0f;
  myOwnBoss[1] = itk::NumericTraits<float>::max();
  myOwnBoss[2] = itk::NumericTraits<float>::min();
  myOwnBoss[3] = 1.0f;

  // Create an itk::Array which does not manage its own memory
  constexpr unsigned int n{ 7 };
  float                  buffer[n];
  FloatArrayType         notMyOwnBoss;
  notMyOwnBoss.SetSize(n);
  notMyOwnBoss.SetData(buffer, false);
  notMyOwnBoss.Fill(4.0);

  FloatArrayType notMyOwnBossToo;
  notMyOwnBossToo.SetSize(n);
  notMyOwnBossToo.SetData(buffer, false);

  // Copy an itk::Array which manages its own memory
  const FloatArrayType test1 = myOwnBoss;
  std::cout << test1 << std::endl;
  EXPECT_EQ(test1.GetSize(), myOwnBoss.GetSize());

  // Copy an itk::Array which does not manage its own memory
  FloatArrayType test2 = notMyOwnBoss;
  std::cout << test2 << std::endl;
  EXPECT_EQ(test2.GetSize(), notMyOwnBoss.GetSize());

  // itk::Array not managing its memory copying one that does
  notMyOwnBoss = myOwnBoss;
  std::cout << notMyOwnBoss << std::endl;
  EXPECT_EQ(notMyOwnBoss.GetSize(), myOwnBoss.GetSize());

  // Calling SetSize with same size
  notMyOwnBossToo.SetSize(notMyOwnBossToo.GetSize());

  // Calling SetSize with different size
  notMyOwnBossToo.SetSize(notMyOwnBossToo.GetSize() + 1);
  notMyOwnBossToo.Fill(6.0);
  std::cout << notMyOwnBossToo << std::endl;

  // Exercise operator=( VnlVectorType& )
  test2 = test1;
  EXPECT_EQ(test2.GetSize(), test1.GetSize());

  // Construct array pointing to user-allocated buffer (user manages deletion)
  constexpr size_t testSizeForArraySetDataSameSize{ 10 };
  FloatArrayType   objectToCopy(testSizeForArraySetDataSameSize);
  auto *           data = new float[testSizeForArraySetDataSameSize];
  objectToCopy.SetDataSameSize(data);

  // Copy of array not managing its own memory
  const FloatArrayType copy(objectToCopy);
  EXPECT_EQ(copy.GetSize(), objectToCopy.GetSize());

  // Double array managing its own memory
  DoubleArrayType myOwnDouble;
  myOwnDouble.SetSize(5);
  myOwnDouble.Fill(2.0 + 1.0 / 3.0);
  myOwnDouble[0] = 2.0 / 3.0;
  myOwnDouble[1] = itk::NumericTraits<double>::max();
  myOwnDouble[2] = itk::NumericTraits<double>::min();
  myOwnDouble[3] = 1.0;
  std::cout << myOwnDouble << std::endl;

  delete[] data;
}
