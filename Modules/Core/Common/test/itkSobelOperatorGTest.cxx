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

#include "itkSobelOperator.h"
#include "itkGTest.h"


TEST(SobelOperator, Test)
{

  constexpr unsigned int Dimension2D{ 2 };
  constexpr unsigned int Dimension3D{ 3 };

  using PixelType = float;

  {
    constexpr unsigned int Length{ 9 };

    using SobelOperatorType = itk::SobelOperator<PixelType, Dimension2D>;
    SobelOperatorType sobelOperator;

    auto * const sobelOperatorPtr = &sobelOperator;
    ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(sobelOperatorPtr, SobelOperator, NeighborhoodOperator);


    // Horizontal
    unsigned long direction = 0;
    sobelOperator.SetDirection(direction);
    EXPECT_EQ(sobelOperator.GetDirection(), direction);

    auto radius = itk::Size<Dimension2D>::Filled(1);
    sobelOperator.CreateToRadius(radius);

    itk::FixedArray<SobelOperatorType::PixelType, Length> expectedValuesHoriz{
      { { -1.0, 0.0, 1.0, -2.0, 0.0, 2.0, -1.0, 0.0, 1.0 } }
    };

    const unsigned int size = sobelOperator.GetBufferReference().size();
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      EXPECT_EQ(expectedValuesHoriz[i], sobelOperator[i]);
      EXPECT_EQ(expectedValuesHoriz[i], sobelOperator.GetElement(i));
    }

    // Vertical
    direction = 1;
    sobelOperator.SetDirection(direction);
    EXPECT_EQ(sobelOperator.GetDirection(), direction);

    sobelOperator.CreateDirectional();

    itk::FixedArray<SobelOperatorType::PixelType, Length> expectedValuesVert{
      { { -1.0, -2.0, -1.0, 0.0, 0.0, 0.0, 1.0, 2.0, 1.0 } }
    };
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      EXPECT_EQ(expectedValuesVert[i], sobelOperator[i]);
      EXPECT_EQ(expectedValuesVert[i], sobelOperator.GetElement(i));
    }
  }

  {
    constexpr unsigned int Length{ 27 };

    using SobelOperatorType = itk::SobelOperator<PixelType, Dimension3D>;
    SobelOperatorType sobelOperator;

    unsigned long direction = 0;
    sobelOperator.SetDirection(direction);
    EXPECT_EQ(sobelOperator.GetDirection(), direction);

    auto radius = itk::Size<Dimension3D>::Filled(1);
    sobelOperator.CreateToRadius(radius);

    itk::FixedArray<SobelOperatorType::PixelType, Length> expectedValuesX{
      { { -1.0, 0.0,  1.0, -3.0, 0.0,  3.0, -1.0, 0.0,  1.0, -3.0, 0.0,  3.0, -6.0, 0.0,
          6.0,  -3.0, 0.0, 3.0,  -1.0, 0.0, 1.0,  -3.0, 0.0, 3.0,  -1.0, 0.0, 1.0 } }
    };
    const unsigned int size = sobelOperator.GetBufferReference().size();
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      EXPECT_EQ(expectedValuesX[i], sobelOperator[i]);
      EXPECT_EQ(expectedValuesX[i], sobelOperator.GetElement(i));
    }

    direction = 1;
    sobelOperator.SetDirection(direction);

    sobelOperator.CreateDirectional();

    EXPECT_EQ(sobelOperator.GetDirection(), direction);
    itk::FixedArray<SobelOperatorType::PixelType, Length> expectedValuesY{
      { { -1.0, -3.0, -1.0, 0.0, 0.0,  0.0,  1.0,  3.0, 1.0, -3.0, -6.0, -3.0, 0.0, 0.0,
          0.0,  3.0,  6.0,  3.0, -1.0, -3.0, -1.0, 0.0, 0.0, 0.0,  1.0,  3.0,  1.0 } }
    };
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      EXPECT_EQ(expectedValuesY[i], sobelOperator[i]);
      EXPECT_EQ(expectedValuesY[i], sobelOperator.GetElement(i));
    }

    direction = 2;
    sobelOperator.SetDirection(direction);
    EXPECT_EQ(sobelOperator.GetDirection(), direction);

    sobelOperator.CreateDirectional();

    itk::FixedArray<SobelOperatorType::PixelType, Length> expectedValuesZ{
      { { -1.0, -3.0, -1.0, -3.0, -6.0, -3.0, -1.0, -3.0, -1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
          0.0,  0.0,  0.0,  0.0,  1.0,  3.0,  1.0,  3.0,  6.0,  3.0, 1.0, 3.0, 1.0 } }
    };
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      EXPECT_EQ(expectedValuesZ[i], sobelOperator[i]);
      EXPECT_EQ(expectedValuesZ[i], sobelOperator.GetElement(i));
    }
  }
}
