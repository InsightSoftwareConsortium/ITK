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
#include "itkExtractImageFilter.h"
#include "itkMatrix.h"
#include "itkVariableSizeMatrix.h"

#include <gtest/gtest.h>

namespace
{
using Strategy = itk::ExtractImageFilterEnums::DirectionCollapseStrategy;
using Result = itk::detail::DirectionCollapseResult;

template <typename TDirection>
void
CheckDirectionCollapsePolicies(TDirection & direction)
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
  const Strategy strategies[] = { Strategy::DIRECTIONCOLLAPSETOIDENTITY,
                                  Strategy::DIRECTIONCOLLAPSETOSUBMATRIX,
                                  Strategy::DIRECTIONCOLLAPSETOGUESS,
                                  Strategy::DIRECTIONCOLLAPSETOUNKOWN,
                                  static_cast<Strategy>(255) };

  for (const auto & testCase : cases)
  {
    SCOPED_TRACE(testCase.name);
    for (const Strategy strategy : strategies)
    {
      SCOPED_TRACE(static_cast<unsigned int>(strategy));
      for (unsigned int row = 0; row < 2; ++row)
      {
        for (unsigned int column = 0; column < 2; ++column)
        {
          direction(row, column) = testCase.values[row][column];
        }
      }

      const bool invalid = strategy == Strategy::DIRECTIONCOLLAPSETOUNKOWN || strategy == static_cast<Strategy>(255);

      Result expectedResult = Result::Success;
      if (invalid)
      {
        expectedResult = Result::InvalidStrategy;
      }
      else if (testCase.singular && strategy == Strategy::DIRECTIONCOLLAPSETOSUBMATRIX)
      {
        expectedResult = Result::Singular;
      }
      EXPECT_EQ(itk::detail::ResolveDirectionCollapse(direction, strategy), expectedResult);

      const bool expectIdentity = strategy == Strategy::DIRECTIONCOLLAPSETOIDENTITY ||
                                  (testCase.singular && strategy == Strategy::DIRECTIONCOLLAPSETOGUESS);
      for (unsigned int row = 0; row < 2; ++row)
      {
        for (unsigned int column = 0; column < 2; ++column)
        {
          const double expected = expectIdentity ? (row == column ? 1.0 : 0.0) : testCase.values[row][column];
          EXPECT_EQ(direction(row, column), expected) << "row " << row << ", column " << column;
        }
      }
    }
  }
}
} // namespace

TEST(DirectionCollapse, FixedMatrixPolicies)
{
  itk::Matrix<double, 2, 2> direction;
  CheckDirectionCollapsePolicies(direction);
}

TEST(DirectionCollapse, VariableSizeMatrixPolicies)
{
  itk::VariableSizeMatrix<double> direction;
  direction.SetSize(2, 2);
  CheckDirectionCollapsePolicies(direction);
}
