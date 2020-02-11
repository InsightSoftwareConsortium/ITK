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

// First include the header file to be tested:
#include "itkVectorContainer.h"

#include <gtest/gtest.h>
#include <cstdlib>
#include <string>

// The tested ElementIdentifier type.
using TestedElementIdentifierType = std::size_t;

// Test template instantiations for various TElement template arguments:
template class itk::VectorContainer<TestedElementIdentifierType, int>;
template class itk::VectorContainer<TestedElementIdentifierType, bool>;
template class itk::VectorContainer<TestedElementIdentifierType, std::string>;


namespace
{
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
