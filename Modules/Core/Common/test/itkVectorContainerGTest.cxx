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
#include "itkVectorContainer.h"

#include <gtest/gtest.h>
#include <cstdlib>
#include <string>

// The tested ElementIdentifier type.
using TestedElementIdentifierType = size_t;

// Test template instantiations for various TElement template arguments:
template class itk::VectorContainer<TestedElementIdentifierType, int>;
template class itk::VectorContainer<TestedElementIdentifierType, bool>;
template class itk::VectorContainer<TestedElementIdentifierType, std::string>;


namespace
{
template <typename T1, typename T2>
constexpr void
AssertSameType()
{
  static_assert(std::is_same_v<T1, T2>);
}


template <typename TElementIdentifier, typename TElement>
constexpr bool
AssertVectorContainerHasSamePublicNestedTypesAsStdVector()
{
  using VectorContainerType = itk::VectorContainer<TElementIdentifier, TElement>;
  using StdVectorType = std::vector<TElement>;

  // Check the list of nested types of `std::vector` from section [vector.overview] of the C++ Standard Working Draft of
  // 2023-05-10, from https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2023/n4950.pdf.
  AssertSameType<typename VectorContainerType::value_type, typename StdVectorType::value_type>();
  AssertSameType<typename VectorContainerType::allocator_type, typename StdVectorType::allocator_type>();
  AssertSameType<typename VectorContainerType::pointer, typename StdVectorType::pointer>();
  AssertSameType<typename VectorContainerType::const_pointer, typename StdVectorType::const_pointer>();
  AssertSameType<typename VectorContainerType::reference, typename StdVectorType::reference>();
  AssertSameType<typename VectorContainerType::const_reference, typename StdVectorType::const_reference>();
  AssertSameType<typename VectorContainerType::size_type, typename StdVectorType::size_type>();
  AssertSameType<typename VectorContainerType::difference_type, typename StdVectorType::difference_type>();
  AssertSameType<typename VectorContainerType::iterator, typename StdVectorType::iterator>();
  AssertSameType<typename VectorContainerType::const_iterator, typename StdVectorType::const_iterator>();
  AssertSameType<typename VectorContainerType::reverse_iterator, typename StdVectorType::reverse_iterator>();
  AssertSameType<typename VectorContainerType::const_reverse_iterator,
                 typename StdVectorType::const_reverse_iterator>();
  return true;
}


static_assert(AssertVectorContainerHasSamePublicNestedTypesAsStdVector<TestedElementIdentifierType, int>());
static_assert(AssertVectorContainerHasSamePublicNestedTypesAsStdVector<TestedElementIdentifierType, bool>());


template <typename TElementIdentifier, typename TElement>
void
ExpectContainerHasValueOfCreatedElementAtIdentifier(const TElementIdentifier identifier, const TElement value)
{
  const auto vectorContainer = itk::VectorContainer<TElementIdentifier, TElement>::New();

  vectorContainer->CreateElementAt(identifier) = value;

  EXPECT_EQ(vectorContainer->ElementAt(identifier), value);
}
} // namespace


// Tests that a VectorContainer has the value of an element created by
// CreateElementAt(identifier) at the position specified by the identifier.
TEST(VectorContainer, HasValueOfCreatedElementAtIdentifier)
{
  // Just pick a "pseudo-random" (magic) number as ElementIdentifier.
  constexpr TestedElementIdentifierType magicIdentifier = 42;

  ExpectContainerHasValueOfCreatedElementAtIdentifier(magicIdentifier, true);
  ExpectContainerHasValueOfCreatedElementAtIdentifier(magicIdentifier, false);

  for (int i = 0; i < 3; ++i)
  {
    ExpectContainerHasValueOfCreatedElementAtIdentifier(magicIdentifier, i);
  }
}
