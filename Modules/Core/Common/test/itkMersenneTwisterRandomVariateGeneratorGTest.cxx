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
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include <gtest/gtest.h>
#include <random> // For mt19937.


// Tests that GetIntegerVariate() conforms with the C++11 requirement for std::mt19937,
// when the ITK generator uses the default seed of std::mt19937:
// "The 10000th consecutive invocation of a default-constructed object of type mt19937
// shall produce the value 4123659995."
// (C++11 section "Engines and engine adaptors with predefined parameters", [rand.predef])
TEST(MersenneTwisterRandomVariateGenerator, GetIntegerVariateConformsWithStdMt19937Requirement)
{
  const auto generator = itk::Statistics::MersenneTwisterRandomVariateGenerator::New();
  generator->SetSeed(std::mt19937::default_seed);

  for (int i = 1; i < 10000; ++i)
  {
    generator->GetIntegerVariate();
  }

  // Call GetIntegerVariate() for the 10000th time:
  const auto actualValue = generator->GetIntegerVariate();

  // The value required for std::mt19937 (C++11):
  const auto requiredValue = 4123659995UL;
  ASSERT_EQ(actualValue, requiredValue);
}


// Tests that repeated calls to GetIntegerVariate() yield the same values
// as std::mt19937, when std::mt19937 uses the same seed as the ITK generator.
TEST(MersenneTwisterRandomVariateGenerator, GetIntegerVariateReturnsSameAsStdMt19937)
{
  const auto   generator = itk::Statistics::MersenneTwisterRandomVariateGenerator::New();
  std::mt19937 stdMt19937{ generator->GetSeed() };

  // Just repeat a few times, assuming that that should be enough.
  for (int i = 0; i < 10; ++i)
  {
    EXPECT_EQ(generator->GetIntegerVariate(), stdMt19937());
  }
}
