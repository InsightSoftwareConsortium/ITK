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
#include "itkSobelOperator.h"
#include "itkGTest.h"

// Standard C++ header files:
#include <array>
#include <string>
#include <vector>


namespace
{
using ExpectedKernelType = std::vector<int>;

template <unsigned int VDimension>
void
CheckKernelCoordinates(const bool                                         useLegacyCoordinates,
                       const std::array<ExpectedKernelType, VDimension> & expectedKernels)
{
  using PixelType = float;
  for (unsigned int direction{}; direction < VDimension; ++direction)
  {
    SCOPED_TRACE("`VDimension` = " + std::to_string(VDimension) + ", `useLegacyCoordinates` = " +
                 (useLegacyCoordinates ? "true" : "false") + ", `direction` = " + std::to_string(direction));

    itk::SobelOperator<PixelType, VDimension> sobelOperator;
    sobelOperator.SetDirection(direction);
    EXPECT_EQ(sobelOperator.GetDirection(), direction);

    sobelOperator.UseLegacyCoefficients(useLegacyCoordinates);
    EXPECT_EQ(sobelOperator.IsUsingLegacyCoefficients(), useLegacyCoordinates);

    sobelOperator.CreateToRadius(itk::Size<VDimension>::Filled(1));

    const ExpectedKernelType & expectedKernel = expectedKernels[direction];

    const unsigned int numberOfCoordinates{ sobelOperator.GetBufferReference().size() };

    ASSERT_EQ(numberOfCoordinates, expectedKernel.size());

    for (itk::SizeValueType i = 0; i < numberOfCoordinates; ++i)
    {
      EXPECT_EQ(sobelOperator[i], expectedKernel[i]) << "  with `i` = " << i;
    }
  }
}
} // namespace


TEST(SobelOperator, ExerciseBasicObjectMethods)
{
  itk::SobelOperator<float, 2> sobelOperator;
  auto * const                 sobelOperatorPtr = &sobelOperator;
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(sobelOperatorPtr, SobelOperator, NeighborhoodOperator);
}


// Checks that the operator uses legacy coefficients by default.
TEST(SobelOperator, IsUsingLegacyCoefficientsByDefault)
{
  itk::SobelOperator<float, 2> sobelOperator;
  EXPECT_TRUE(sobelOperator.IsUsingLegacyCoefficients());
}


// Checks that the coordinates of the kernels have the expected values.
TEST(SobelOperator, CheckKernelCoordinates)
{
  for (const bool useLegacyCoordinates : { false, true })
  {
    CheckKernelCoordinates<1>(useLegacyCoordinates, { ExpectedKernelType{ -1, 0, 1 } });
    CheckKernelCoordinates<2>(
      useLegacyCoordinates,
      { ExpectedKernelType{ -1, 0, 1, -2, 0, 2, -1, 0, 1 }, ExpectedKernelType{ -1, -2, -1, 0, 0, 0, 1, 2, 1 } });
    CheckKernelCoordinates<4>(
      useLegacyCoordinates,
      { ExpectedKernelType{ -1, 0, 1, -2, 0, 2, -1, 0, 1, -2, 0, 2, -4, 0, 4, -2, 0, 2, -1, 0, 1, -2, 0, 2, -1, 0, 1,
                            -2, 0, 2, -4, 0, 4, -2, 0, 2, -4, 0, 4, -8, 0, 8, -4, 0, 4, -2, 0, 2, -4, 0, 4, -2, 0, 2,
                            -1, 0, 1, -2, 0, 2, -1, 0, 1, -2, 0, 2, -4, 0, 4, -2, 0, 2, -1, 0, 1, -2, 0, 2, -1, 0, 1 },
        ExpectedKernelType{ -1, -2, -1, 0, 0, 0, 1, 2, 1, -2, -4, -2, 0, 0, 0, 2, 4, 2, -1, -2, -1, 0, 0, 0, 1, 2, 1,
                            -2, -4, -2, 0, 0, 0, 2, 4, 2, -4, -8, -4, 0, 0, 0, 4, 8, 4, -2, -4, -2, 0, 0, 0, 2, 4, 2,
                            -1, -2, -1, 0, 0, 0, 1, 2, 1, -2, -4, -2, 0, 0, 0, 2, 4, 2, -1, -2, -1, 0, 0, 0, 1, 2, 1 },
        ExpectedKernelType{ -1, -2, -1, -2, -4, -2, -1, -2, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 2, 4, 2, 1, 2, 1,
                            -2, -4, -2, -4, -8, -4, -2, -4, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 4, 2, 4, 8, 4, 2, 4, 2,
                            -1, -2, -1, -2, -4, -2, -1, -2, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 2, 4, 2, 1, 2, 1 },
        ExpectedKernelType{ -1, -2, -1, -2, -4, -2, -1, -2, -1, -2, -4, -2, -4, -8, -4, -2, -4, -2, -1, -2, -1,
                            -2, -4, -2, -1, -2, -1, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                            0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  1,  2,  4,  2,  1,  2,  1,
                            2,  4,  2,  4,  8,  4,  2,  4,  2,  1,  2,  1,  2,  4,  2,  1,  2,  1 } });
  }

  // For 3D, the legacy and the non-legacy coordinates are different.
  CheckKernelCoordinates<3>(
    true,
    { ExpectedKernelType{ -1, 0, 1, -3, 0, 3, -1, 0, 1, -3, 0, 3, -6, 0, 6, -3, 0, 3, -1, 0, 1, -3, 0, 3, -1, 0, 1 },
      ExpectedKernelType{ -1, -3, -1, 0, 0, 0, 1, 3, 1, -3, -6, -3, 0, 0, 0, 3, 6, 3, -1, -3, -1, 0, 0, 0, 1, 3, 1 },
      ExpectedKernelType{ -1, -3, -1, -3, -6, -3, -1, -3, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 1, 3, 6, 3, 1, 3, 1 } });
  CheckKernelCoordinates<3>(
    false,
    { ExpectedKernelType{ -1, 0, 1, -2, 0, 2, -1, 0, 1, -2, 0, 2, -4, 0, 4, -2, 0, 2, -1, 0, 1, -2, 0, 2, -1, 0, 1 },
      ExpectedKernelType{ -1, -2, -1, 0, 0, 0, 1, 2, 1, -2, -4, -2, 0, 0, 0, 2, 4, 2, -1, -2, -1, 0, 0, 0, 1, 2, 1 },
      ExpectedKernelType{ -1, -2, -1, -2, -4, -2, -1, -2, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 2, 4, 2, 1, 2, 1 } });
}
