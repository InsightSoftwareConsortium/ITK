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
#include "itkOptimizerParameters.h"
#include <gtest/gtest.h>
#include <algorithm> // For std::count.


// Tests constructing OptimizerParameters of the specified size and initial value.
TEST(OptimizerParameters, ConstructWithSpecifiedSizeAndInitialValue)
{
  using OptimizerParametersType = itk::OptimizerParameters<double>;

  for (double initialValue{ -1.0 }; initialValue <= 1.0; ++initialValue)
  {
    EXPECT_EQ(OptimizerParametersType(0, initialValue).size(), 0);

    for (std::size_t size{ 1 }; size <= 4; ++size)
    {
      const OptimizerParametersType optimizerParameters(size, initialValue);

      EXPECT_EQ(optimizerParameters.size(), size);
      EXPECT_EQ(std::count(optimizerParameters.begin(), optimizerParameters.end(), initialValue), size);
    }
  }
}
