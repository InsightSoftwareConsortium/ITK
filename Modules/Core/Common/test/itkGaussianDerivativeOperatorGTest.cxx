// SPDX-FileCopyrightText: Copyright NumFOCUS
// SPDX-License-Identifier: Apache-2.0
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
#include "itkGaussianDerivativeOperator.h"
#include "itkStdStreamStateSave.h"
#include "itkGTest.h"
#include <set>

namespace
{

bool
TestGaussianOperator(double variance, double error, unsigned int width, unsigned int order, double spacing)
{

  using GaussianOp = itk::GaussianDerivativeOperator<double, 1>;

  std::cout << "Testing variance: " << variance << " error: " << error << " width: " << width << " order: " << order
            << " spacing: " << spacing << std::endl;

  GaussianOp op;

  constexpr bool normalizeAcrossScale{ false };
  ITK_GTEST_SET_GET_BOOLEAN((&op), NormalizeAcrossScale, normalizeAcrossScale);

  op.SetVariance(variance);
  EXPECT_EQ(op.GetVariance(), variance);

  op.SetMaximumError(error);
  EXPECT_EQ(op.GetMaximumError(), error);

  op.SetMaximumKernelWidth(width);
  EXPECT_EQ(op.GetMaximumKernelWidth(), width);

  op.SetOrder(order);
  EXPECT_EQ(op.GetOrder(), order);

  op.SetSpacing(spacing);
  EXPECT_EQ(op.GetSpacing(), spacing);

  op.CreateDirectional();

  const double total = std::accumulate(op.Begin(), op.End(), 0.0);
  std::cout << "total: " << total << std::endl;

  std::cout.precision(16);

  constexpr double epsilon{ itk::NumericTraits<double>::epsilon() * 32 };
  if (order == 0 && itk::Math::Absolute(total - 1.0) > epsilon)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in coefficients." << std::endl;
    std::cerr << "Expected coefficients to sum to 1.0: " << std::endl;
    std::cerr << "Actual value: " << total << std::endl;
    std::cerr << " differs from 1.0 ";
    std::cerr << " by more than " << epsilon << std::endl;
  }
  else if (order != 0 && itk::Math::Absolute(total) > epsilon)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in coefficients." << std::endl;
    std::cerr << "Expected coefficients to sum to 0.0." << std::endl;
    std::cerr << "Actual value: " << total << std::endl;
    std::cerr << " differs from 0.0 ";
    std::cerr << " by more than " << epsilon << std::endl;
  }
  else
  {
    return true;
  }

  std::cout << "---operator---" << std::endl;
  GaussianOp::Iterator i = op.Begin();
  i += op.Size() / 2;
  for (; i != op.End(); ++i)
  {
    std::cout << *i << std::endl;
  }
  std::cout << "---end--" << std::endl;

  return false;
}

} // namespace


TEST(GaussianDerivativeOperator, ConvertedLegacyTest)
{

  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope.
  const itk::StdStreamStateSave coutState(std::cout);

  // Exercise code

  using GaussianOp = itk::GaussianDerivativeOperator<double, 3>;

  GaussianOp   op1;
  GaussianOp * op1Ptr = &op1;

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(op1Ptr, GaussianDerivativeOperator, NeighborhoodOperator);

  // Check assignment
  EXPECT_TRUE(TestGaussianOperator(.2, .001, 30, 0, 1.0));
  EXPECT_TRUE(TestGaussianOperator(.2, .001, 30, 1, 1.0));
  EXPECT_TRUE(TestGaussianOperator(.2, .001, 30, 2, 1.0));
  EXPECT_TRUE(TestGaussianOperator(.2, .001, 30, 3, 1.0));
  EXPECT_TRUE(TestGaussianOperator(.2, .001, 30, 4, 1.0));

  EXPECT_TRUE(TestGaussianOperator(1, .001, 30, 0, 1.0));
  EXPECT_TRUE(TestGaussianOperator(1, .001, 30, 1, 1.0));
  EXPECT_TRUE(TestGaussianOperator(1, .001, 30, 2, 1.0));
  EXPECT_TRUE(TestGaussianOperator(1, .001, 30, 3, 1.0));
  EXPECT_TRUE(TestGaussianOperator(1, .001, 30, 4, 1.0));

  EXPECT_TRUE(TestGaussianOperator(10, .001, 30, 0, 1.0));
  EXPECT_TRUE(TestGaussianOperator(10, .001, 30, 1, 1.0));

  EXPECT_TRUE(TestGaussianOperator(10, .0001, 100, 1, 1.0));

  EXPECT_TRUE(TestGaussianOperator(50, .001, 300, 0, 1.0));

  // Test streaming enumeration for GaussianDerivativeOperatorEnums::InterpolationMode elements
  const std::set<itk::GaussianDerivativeOperatorEnums::InterpolationMode> allInterpolationMode{
    itk::GaussianDerivativeOperatorEnums::InterpolationMode::NearestNeighbourInterpolation,
    itk::GaussianDerivativeOperatorEnums::InterpolationMode::LinearInterpolation
  };
  for (const auto & ee : allInterpolationMode)
  {
    std::cout << "STREAMED ENUM VALUE GaussianDerivativeOperatorEnums::InterpolationMode: " << ee << std::endl;
  }
}
