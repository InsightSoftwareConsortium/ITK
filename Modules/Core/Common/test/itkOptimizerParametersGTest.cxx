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
#include "itkOptimizerParameters.h"
#include <gtest/gtest.h>

#include <algorithm> // For count and equal.
#include <array>
#include <vector>


// Tests constructing OptimizerParameters of the specified size and initial value.
TEST(OptimizerParameters, ConstructWithSpecifiedSizeAndInitialValue)
{
  using OptimizerParametersType = itk::OptimizerParameters<double>;

  for (double initialValue{ -1.0 }; initialValue <= 1.0; ++initialValue)
  {
    EXPECT_EQ(OptimizerParametersType(0, initialValue).size(), 0);

    for (size_t size{ 1 }; size <= 4; ++size)
    {
      const OptimizerParametersType optimizerParameters(size, initialValue);

      EXPECT_EQ(optimizerParameters.size(), size);
      EXPECT_EQ(std::count(optimizerParameters.begin(), optimizerParameters.end(), initialValue), size);
    }
  }
}


// Tests constructing OptimizerParameters with the specified data and size.
TEST(OptimizerParameters, ConstructWithSpecifiedDataAndSize)
{
  using OptimizerParametersType = itk::OptimizerParameters<double>;

  // First test for size = zero.
  EXPECT_EQ(OptimizerParametersType(std::vector<double>{}.data(), 0), OptimizerParametersType{});
  EXPECT_EQ(OptimizerParametersType(std::array<double, 0>().data(), 0), OptimizerParametersType{});

  // Test for an arbitrary size:
  const auto testConstructOptimizerParameters = [](const double * const inputData, const itk::SizeValueType dimension) {
    const OptimizerParametersType optimizerParameters(inputData, dimension);
    ASSERT_EQ(optimizerParameters.size(), dimension);
    EXPECT_TRUE(std::equal(optimizerParameters.begin(), optimizerParameters.end(), inputData));
  };

  testConstructOptimizerParameters(std::array<double, 1>().data(), 1);
  testConstructOptimizerParameters(std::vector<double>{ 1.0 }.data(), 1);

  const std::vector<double> stdVector{ 1.0, 2.0, 4.0 };
  testConstructOptimizerParameters(stdVector.data(), stdVector.size());
}
