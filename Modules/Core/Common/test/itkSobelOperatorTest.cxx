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
#include "itkTestingMacros.h"


int
itkSobelOperatorTest(int, char *[])
{

  constexpr unsigned int Dimension2D = 2;
  constexpr unsigned int Dimension3D = 3;
  constexpr unsigned int Dimension4D = 4;

  using PixelType = float;

  {
    constexpr unsigned int Length = 9;

    using SobelOperatorType = itk::SobelOperator<PixelType, Dimension2D>;
    SobelOperatorType sobelOperator;

    ITK_EXERCISE_BASIC_OBJECT_METHODS((&sobelOperator), SobelOperator, NeighborhoodOperator);


    // Horizontal
    unsigned long direction = 0;
    sobelOperator.SetDirection(direction);
    ITK_TEST_SET_GET_VALUE(direction, sobelOperator.GetDirection());

    itk::Size<Dimension2D> radius;
    radius.Fill(1);
    sobelOperator.CreateToRadius(radius);

    itk::FixedArray<SobelOperatorType::PixelType, Length> expectedValuesHoriz{
      { { -1.0, 0.0, 1.0, -2.0, 0.0, 2.0, -1.0, 0.0, 1.0 } }
    };

    const unsigned int size = sobelOperator.GetBufferReference().size();
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      ITK_TEST_EXPECT_EQUAL(expectedValuesHoriz[i], sobelOperator[i]);
      ITK_TEST_EXPECT_EQUAL(expectedValuesHoriz[i], sobelOperator.GetElement(i));
    }

    // Vertical
    direction = 1;
    sobelOperator.SetDirection(direction);
    ITK_TEST_SET_GET_VALUE(direction, sobelOperator.GetDirection());

    sobelOperator.CreateDirectional();

    itk::FixedArray<SobelOperatorType::PixelType, Length> expectedValuesVert{
      { { -1.0, -2.0, -1.0, 0.0, 0.0, 0.0, 1.0, 2.0, 1.0 } }
    };
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      ITK_TEST_EXPECT_EQUAL(expectedValuesVert[i], sobelOperator[i]);
      ITK_TEST_EXPECT_EQUAL(expectedValuesVert[i], sobelOperator.GetElement(i));
    }
  }

  {
    constexpr unsigned int Length = 27;

    using SobelOperatorType = itk::SobelOperator<PixelType, Dimension3D>;
    SobelOperatorType sobelOperator;

    unsigned long direction = 0;
    sobelOperator.SetDirection(direction);
    ITK_TEST_SET_GET_VALUE(direction, sobelOperator.GetDirection());

    itk::Size<Dimension3D> radius;
    radius.Fill(1);
    sobelOperator.CreateToRadius(radius);

    itk::FixedArray<SobelOperatorType::PixelType, Length> expectedValuesX{
      { { -1.0, 0.0,  1.0, -3.0, 0.0,  3.0, -1.0, 0.0,  1.0, -3.0, 0.0,  3.0, -6.0, 0.0,
          6.0,  -3.0, 0.0, 3.0,  -1.0, 0.0, 1.0,  -3.0, 0.0, 3.0,  -1.0, 0.0, 1.0 } }
    };
    const unsigned int size = sobelOperator.GetBufferReference().size();
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      ITK_TEST_EXPECT_EQUAL(expectedValuesX[i], sobelOperator[i]);
      ITK_TEST_EXPECT_EQUAL(expectedValuesX[i], sobelOperator.GetElement(i));
    }

    direction = 1;
    sobelOperator.SetDirection(direction);

    sobelOperator.CreateDirectional();

    ITK_TEST_SET_GET_VALUE(direction, sobelOperator.GetDirection());
    itk::FixedArray<SobelOperatorType::PixelType, Length> expectedValuesY{
      { { -1.0, -3.0, -1.0, 0.0, 0.0,  0.0,  1.0,  3.0, 1.0, -3.0, -6.0, -3.0, 0.0, 0.0,
          0.0,  3.0,  6.0,  3.0, -1.0, -3.0, -1.0, 0.0, 0.0, 0.0,  1.0,  3.0,  1.0 } }
    };
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      ITK_TEST_EXPECT_EQUAL(expectedValuesY[i], sobelOperator[i]);
      ITK_TEST_EXPECT_EQUAL(expectedValuesY[i], sobelOperator.GetElement(i));
    }

    direction = 2;
    sobelOperator.SetDirection(direction);
    ITK_TEST_SET_GET_VALUE(direction, sobelOperator.GetDirection());

    sobelOperator.CreateDirectional();

    itk::FixedArray<SobelOperatorType::PixelType, Length> expectedValuesZ{
      { { -1.0, -3.0, -1.0, -3.0, -6.0, -3.0, -1.0, -3.0, -1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
          0.0,  0.0,  0.0,  0.0,  1.0,  3.0,  1.0,  3.0,  6.0,  3.0, 1.0, 3.0, 1.0 } }
    };
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      ITK_TEST_EXPECT_EQUAL(expectedValuesZ[i], sobelOperator[i]);
      ITK_TEST_EXPECT_EQUAL(expectedValuesZ[i], sobelOperator.GetElement(i));
    }
  }

  {
    using SobelOperatorType = itk::SobelOperator<PixelType, Dimension4D>;
    SobelOperatorType sobelOperator;

    unsigned long direction = 0;
    sobelOperator.SetDirection(direction);
    itk::Size<Dimension4D> radius;
    radius.Fill(1);
    ITK_TRY_EXPECT_EXCEPTION(sobelOperator.CreateToRadius(radius));
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
