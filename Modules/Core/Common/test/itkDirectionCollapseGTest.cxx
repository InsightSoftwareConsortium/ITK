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

#include "itkDirectionCollapse.h"
#include <gtest/gtest.h>
#include <array>
#include <sstream>
#include <vector>

namespace
{
using StrategyEnum = itk::ExtractImageFilterEnums::DirectionCollapseStrategy;
using Matrix2 = itk::Matrix<double, 2, 2>;
using Matrix3 = itk::Matrix<double, 3, 3>;
using Matrix4 = itk::Matrix<double, 4, 4>;

Matrix3
MakeRotatedXY()
{
  // 90 degree rotation in the xy plane, z unchanged.
  Matrix3 direction = Matrix3::GetIdentity();
  direction[0][0] = 0.0;
  direction[0][1] = -1.0;
  direction[1][0] = 1.0;
  direction[1][1] = 0.0;
  return direction;
}

Matrix2
MakeRotation2D()
{
  Matrix2 rotation = Matrix2::GetIdentity();
  rotation[0][0] = 0.0;
  rotation[0][1] = -1.0;
  rotation[1][0] = 1.0;
  rotation[1][1] = 0.0;
  return rotation;
}
} // namespace

TEST(DirectionCollapse, FixedDimension)
{
  const Matrix3 direction = MakeRotatedXY();
  const Matrix2 rotation = MakeRotation2D();
  const Matrix2 identity = Matrix2::GetIdentity();

  // Collapsing z retains the invertible xy block; collapsing x leaves a singular yz block.
  const itk::FixedArray<bool, 3> collapseZ(std::array<bool, 3>{ false, false, true });
  const itk::FixedArray<bool, 3> collapseX(std::array<bool, 3>{ true, false, false });

  EXPECT_EQ(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOIDENTITY), identity);

  EXPECT_EQ(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX), rotation);
  EXPECT_THROW(itk::CollapseDirection<2>(direction, collapseX, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX),
               itk::ExceptionObject);

  EXPECT_EQ(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOGUESS), rotation);
  EXPECT_EQ(itk::CollapseDirection<2>(direction, collapseX, StrategyEnum::DIRECTIONCOLLAPSETOGUESS), identity);

  EXPECT_THROW(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOUNKOWN),
               itk::ExceptionObject);
  EXPECT_THROW(itk::CollapseDirection<2>(direction, collapseZ, static_cast<StrategyEnum>(255)), itk::ExceptionObject);

  // The retained dimension count must match the output dimension.
  const itk::FixedArray<bool, 3> collapseNone(false);
  EXPECT_THROW(itk::CollapseDirection<2>(direction, collapseNone, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX),
               itk::ExceptionObject);

  // Same dimension in and out with nothing collapsed returns the input for the matrix strategies.
  EXPECT_EQ(itk::CollapseDirection<3>(direction, collapseNone, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX), direction);
  EXPECT_EQ(itk::CollapseDirection<3>(direction, collapseNone, StrategyEnum::DIRECTIONCOLLAPSETOIDENTITY),
            Matrix3::GetIdentity());
}

TEST(DirectionCollapse, SingularAndNearSingularBlocks)
{
  struct TestCase
  {
    const char * name;
    double       values[2][2];
    bool         singular;
  };
  const TestCase cases[] = { { "Singular", { { 1.0, 2.0 }, { 2.0, 4.0 } }, true },
                             { "Zero", { { 0.0, 0.0 }, { 0.0, 0.0 } }, true },
                             { "PositiveNearSingular", { { 1.0, 0.0 }, { 0.0, 1e-14 } }, false },
                             { "NegativeNearSingular", { { 1.0, 0.0 }, { 0.0, -1e-14 } }, false },
                             { "Reflection", { { -1.0, 0.0 }, { 0.0, 1.0 } }, false },
                             { "Rotation", { { 0.0, -1.0 }, { 1.0, 0.0 } }, false } };

  for (const auto & testCase : cases)
  {
    SCOPED_TRACE(testCase.name);
    Matrix3 direction = Matrix3::GetIdentity();
    Matrix2 block;
    for (unsigned int row = 0; row < 2; ++row)
    {
      for (unsigned int col = 0; col < 2; ++col)
      {
        direction[row][col] = testCase.values[row][col];
        block[row][col] = testCase.values[row][col];
      }
    }
    const itk::FixedArray<bool, 3> collapseZ(std::array<bool, 3>{ false, false, true });

    EXPECT_EQ(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOIDENTITY),
              Matrix2::GetIdentity());
    if (testCase.singular)
    {
      EXPECT_THROW(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX),
                   itk::ExceptionObject);
      EXPECT_EQ(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOGUESS),
                Matrix2::GetIdentity());
    }
    else
    {
      EXPECT_EQ(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX), block);
      EXPECT_EQ(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOGUESS), block);
    }
  }
}

TEST(DirectionCollapse, FloatDirection)
{
  // ITK_USE_FLOAT_SPACE_PRECISION makes DirectionType a float matrix.
  const Matrix3                  doubleDirection = MakeRotatedXY();
  const Matrix2                  doubleRotation = MakeRotation2D();
  itk::Matrix<float, 3, 3>       direction;
  itk::Matrix<float, 2, 2>       rotation;
  const itk::FixedArray<bool, 3> collapseZ(std::array<bool, 3>{ false, false, true });
  for (unsigned int i = 0; i < 3; ++i)
  {
    for (unsigned int j = 0; j < 3; ++j)
    {
      direction[i][j] = static_cast<float>(doubleDirection[i][j]);
      if (i < 2 && j < 2)
      {
        rotation[i][j] = static_cast<float>(doubleRotation[i][j]);
      }
    }
  }

  EXPECT_EQ(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX), rotation);
}

TEST(DirectionCollapse, FloatSingularAfterConversion)
{
  // The retained block is invertible in double but rounds to a singular float matrix.
  itk::VariableSizeMatrix<double> direction(3, 3);
  direction.SetIdentity();
  direction(0, 1) = 1.0;
  direction(1, 0) = 1.0;
  direction(1, 1) = 1.0 + 1e-8;
  const std::vector<bool> collapseZ{ false, false, true };

  EXPECT_EQ((itk::CollapseDirection<2, float>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOGUESS)),
            (itk::Matrix<float, 2, 2>::GetIdentity()));
  EXPECT_THROW((itk::CollapseDirection<2, float>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX)),
               itk::ExceptionObject);
  EXPECT_NE(itk::CollapseDirection<2>(direction, collapseZ, StrategyEnum::DIRECTIONCOLLAPSETOGUESS),
            Matrix2::GetIdentity());
}

// ImageIOBase stores direction vectors as columns.
TEST(DirectionCollapse, RuntimeDimension)
{
  // Oblique 4-D direction as an ImageIO would report it: column k is axis k.
  const unsigned int                     ioDimension = 4;
  const std::vector<std::vector<double>> ioAxes{
    { 0.0, 1.0, 0.0, 0.0 }, { -1.0, 0.0, 0.0, 0.0 }, { 0.0, 0.0, 1.0, 0.0 }, { 0.0, 0.0, 0.0, 1.0 }
  };

  itk::VariableSizeMatrix<double> direction(ioDimension, ioDimension);
  for (unsigned int col = 0; col < ioDimension; ++col)
  {
    for (unsigned int row = 0; row < ioDimension; ++row)
    {
      direction(row, col) = ioAxes[col][row];
    }
  }

  // ImageFileReader keeps the leading output-dimension axes and drops the trailing ones.
  std::vector<bool> collapseTrailing(ioDimension, false);
  collapseTrailing[3] = true;

  const Matrix3 expected = MakeRotatedXY();

  EXPECT_EQ(itk::CollapseDirection<3>(direction, collapseTrailing, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX),
            expected);
  EXPECT_EQ(itk::CollapseDirection<3>(direction, collapseTrailing, StrategyEnum::DIRECTIONCOLLAPSETOIDENTITY),
            Matrix3::GetIdentity());

  // The compile-time overload gives the same answer for the same data.
  Matrix4 fixedDirection;
  for (unsigned int row = 0; row < ioDimension; ++row)
  {
    for (unsigned int col = 0; col < ioDimension; ++col)
    {
      fixedDirection[row][col] = direction(row, col);
    }
  }
  const itk::FixedArray<bool, 4> fixedCollapse(std::array<bool, 4>{ false, false, false, true });
  EXPECT_EQ(itk::CollapseDirection<3>(direction, collapseTrailing, StrategyEnum::DIRECTIONCOLLAPSETOGUESS),
            itk::CollapseDirection<3>(fixedDirection, fixedCollapse, StrategyEnum::DIRECTIONCOLLAPSETOGUESS));

  // Dropping the first axis leaves a singular block; the strategies disagree as documented.
  std::vector<bool> collapseLeading(ioDimension, false);
  collapseLeading[0] = true;
  EXPECT_THROW(itk::CollapseDirection<3>(direction, collapseLeading, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX),
               itk::ExceptionObject);
  EXPECT_EQ(itk::CollapseDirection<3>(direction, collapseLeading, StrategyEnum::DIRECTIONCOLLAPSETOGUESS),
            Matrix3::GetIdentity());

  // Shape errors are reported, not silently truncated.
  EXPECT_THROW(
    itk::CollapseDirection<3>(direction, std::vector<bool>(3, false), StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX),
    itk::ExceptionObject);
  const itk::VariableSizeMatrix<double> notSquare(4, 3);
  EXPECT_THROW(itk::CollapseDirection<3>(notSquare, collapseTrailing, StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX),
               itk::ExceptionObject);
}

TEST(DirectionCollapse, StreamStrategy)
{
  std::ostringstream stream;
  stream << StrategyEnum::DIRECTIONCOLLAPSETOGUESS;
  EXPECT_EQ(stream.str(), "ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOGUESS");
}
