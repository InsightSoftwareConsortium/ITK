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
#include "itkMakeUniqueForOverwrite.h"
#include <gtest/gtest.h>
#include <numeric> // For iota.


// Tests that `make_unique_for_overwrite` creates an array of the specified dynamic size, whose elements can all be
// written to.
TEST(MakeUniqueForOverwrite, CreatesAnArrayThatCanBeWrittenTo)
{
  for (std::size_t numberOfElements = 1; numberOfElements < 4; ++numberOfElements)
  {
    const auto data = itk::make_unique_for_overwrite<int[]>(numberOfElements);
    ASSERT_NE(data, nullptr);

    // Write the values { 0, 1, ... , N-1 }.
    std::iota(data.get(), data.get() + numberOfElements, 0);

    // Check that each value is correctly written to the corresponding element.
    for (std::size_t i = 0; i < numberOfElements; ++i)
    {
      EXPECT_EQ(data[i], i);
    }
  }
}
