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

#include "itkLevenbergMarquardtOptimizer.h"
#include "itkMultipleValuedCostFunction.h"
#include <gtest/gtest.h>
#include <cmath>

// Cases scavenged from the VXL test_levenberg_marquardt.cxx: the Rosenbrock
// banana (with and without an analytic Jacobian) and a linear least-squares
// fit. Both the VNL and Eigen engines must reach the analytic solution, and
// the two engines must agree (the Eigen backend swap is numerically
// equivalent to vnl_levenberg_marquardt; both are MINPACK lmdif/lmder ports).

namespace
{
using OptimizerType = itk::LevenbergMarquardtOptimizer;

// r0 = 10 (x1 - x0^2), r1 = 1 - x0 ; minimum at (1, 1) with zero residual.
class RosenbrockCostFunction : public itk::MultipleValuedCostFunction
{
public:
  using Self = RosenbrockCostFunction;
  using Superclass = itk::MultipleValuedCostFunction;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(RosenbrockCostFunction);

  using ParametersType = Superclass::ParametersType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  explicit RosenbrockCostFunction() = default;

  unsigned int
  GetNumberOfParameters() const override
  {
    return 2;
  }
  unsigned int
  GetNumberOfValues() const override
  {
    return 2;
  }

  MeasureType
  GetValue(const ParametersType & p) const override
  {
    MeasureType measure(2);
    measure[0] = 10.0 * (p[1] - p[0] * p[0]);
    measure[1] = 1.0 - p[0];
    return measure;
  }

  void
  GetDerivative(const ParametersType & p, DerivativeType & derivative) const override
  {
    // Layout is derivative[parameterIndex][valueIndex] = d r_value / d x_param.
    derivative.SetSize(2, 2);
    derivative[0][0] = -20.0 * p[0]; // d r0 / d x0
    derivative[0][1] = -1.0;         // d r1 / d x0
    derivative[1][0] = 10.0;         // d r0 / d x1
    derivative[1][1] = 0.0;          // d r1 / d x1
  }
};

// Linear least squares: residual = A x - b, with A overdetermined (m > n).
// Minimum is the normal-equation solution; here data are generated from a
// known x* so the residual is zero at the solution.
class LinearCostFunction : public itk::MultipleValuedCostFunction
{
public:
  using Self = LinearCostFunction;
  using Superclass = itk::MultipleValuedCostFunction;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(LinearCostFunction);

  using ParametersType = Superclass::ParametersType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  unsigned int
  GetNumberOfParameters() const override
  {
    return 2;
  }
  unsigned int
  GetNumberOfValues() const override
  {
    return 4;
  }

  // x* = (2, -1); rows of A and b = A x*.
  MeasureType
  GetValue(const ParametersType & p) const override
  {
    MeasureType measure(4);
    for (unsigned int i = 0; i < 4; ++i)
    {
      const double a0 = 1.0 + i;
      const double a1 = 2.0 - 0.5 * i;
      const double b = a0 * 2.0 + a1 * (-1.0);
      measure[i] = a0 * p[0] + a1 * p[1] - b;
    }
    return measure;
  }

  void
  GetDerivative(const ParametersType & p, DerivativeType & derivative) const override
  {
    (void)p;
    derivative.SetSize(2, 4);
    for (unsigned int i = 0; i < 4; ++i)
    {
      derivative[0][i] = 1.0 + i;
      derivative[1][i] = 2.0 - 0.5 * i;
    }
  }
};

OptimizerType::ParametersType
RunOptimizer(itk::MultipleValuedCostFunction *     costFunction,
             const OptimizerType::ParametersType & initialPosition,
             bool                                  useGradient)
{
  auto optimizer = OptimizerType::New();
  optimizer->SetUseCostFunctionGradient(useGradient);
  optimizer->SetCostFunction(costFunction);
  optimizer->SetNumberOfIterations(2000);
  optimizer->SetValueTolerance(1e-10);
  optimizer->SetGradientTolerance(1e-10);
  optimizer->SetEpsilonFunction(1e-12);
  optimizer->SetInitialPosition(initialPosition);
  optimizer->StartOptimization();
  return optimizer->GetCurrentPosition();
}
} // namespace

TEST(LevenbergMarquardtOptimizer, RosenbrockWithGradient)
{
  auto                          cost = RosenbrockCostFunction::New();
  OptimizerType::ParametersType x0(2);
  x0[0] = -1.2;
  x0[1] = 1.0;

  const auto x = RunOptimizer(cost, x0, /*useGradient=*/true);
  EXPECT_NEAR(x[0], 1.0, 1e-5);
  EXPECT_NEAR(x[1], 1.0, 1e-5);
}

TEST(LevenbergMarquardtOptimizer, RosenbrockWithoutGradient)
{
  auto                          cost = RosenbrockCostFunction::New();
  OptimizerType::ParametersType x0(2);
  x0[0] = -1.2;
  x0[1] = 1.0;

  const auto x = RunOptimizer(cost, x0, /*useGradient=*/false);
  EXPECT_NEAR(x[0], 1.0, 1e-4);
  EXPECT_NEAR(x[1], 1.0, 1e-4);
}

TEST(LevenbergMarquardtOptimizer, LinearLeastSquares)
{
  auto                          cost = LinearCostFunction::New();
  OptimizerType::ParametersType x0(2);
  x0[0] = 0.0;
  x0[1] = 0.0;

  const auto x = RunOptimizer(cost, x0, /*useGradient=*/true);
  EXPECT_NEAR(x[0], 2.0, 1e-6);
  EXPECT_NEAR(x[1], -1.0, 1e-6);
}
