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
#include <set>
#include "itkGaussianDerivativeOperator.h"
#include "itkStdStreamStateSave.h"
#include "itkTestingMacros.h"

namespace
{

bool
TestGaussianOperator(double variance, double error, unsigned int width, unsigned int order, double spacing)
{

  using GaussianOp = itk::GaussianDerivativeOperator<double, 1>;

  std::cout << "Testing variance: " << variance << " error: " << error << " width: " << width << " order: " << order
            << " spacing: " << spacing << std::endl;

  GaussianOp op;

  const bool normalizeAcrossScale = false;
  ITK_TEST_SET_GET_BOOLEAN((&op), NormalizeAcrossScale, normalizeAcrossScale);

  op.SetVariance(variance);
  ITK_TEST_SET_GET_VALUE(variance, op.GetVariance());

  op.SetMaximumError(error);
  ITK_TEST_SET_GET_VALUE(error, op.GetMaximumError());

  op.SetMaximumKernelWidth(width);
  ITK_TEST_SET_GET_VALUE(width, op.GetMaximumKernelWidth());

  op.SetOrder(order);
  ITK_TEST_SET_GET_VALUE(order, op.GetOrder());

  op.SetSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, op.GetSpacing());

  op.CreateDirectional();

  double total = std::accumulate(op.Begin(), op.End(), 0.0);
  std::cout << "total: " << total << std::endl;

  std::cout.precision(16);

  const double epsilon = itk::NumericTraits<double>::epsilon() * 32;
  if (order == 0 && itk::Math::abs(total - 1.0) > epsilon)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in coefficients." << std::endl;
    std::cerr << "Expected coefficients to sum to 1.0: " << std::endl;
    std::cerr << "Actual value: " << total << std::endl;
    std::cerr << " differs from 1.0 ";
    std::cerr << " by more than " << epsilon << std::endl;
  }
  else if (order != 0 && itk::Math::abs(total) > epsilon)
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

int
itkGaussianDerivativeOperatorTest(int argc, char * argv[])
{

  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope.
  itk::StdStreamStateSave coutState(std::cout);

  if (argc == 6)
  {
    double       variance = std::stod(argv[1]);
    double       error = std::stod(argv[2]);
    unsigned int width = std::stoi(argv[3]);
    unsigned int order = std::stoi(argv[4]);
    double       spacing = std::stod(argv[5]);

    TestGaussianOperator(variance, error, width, order, spacing);

    std::cout << "Test finished." << std::endl;
    return EXIT_SUCCESS;
  }
  else if (argc > 1)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " [variance error width order spacing]"
              << std::endl;
    return EXIT_FAILURE;
  }

  // At this point, obviously, argc <= 1. In some scenarios, argc == 0, typically when
  // the test function is called from the interactive TestDriver commandline interface,
  // by having the user entering its test number. On the other hand, argc == 1 when the
  // the TestDriver has the name of the test function as its only commandline argument.
  // In either way the tests below here should be performed.

  // Exercise code

  using GaussianOp = itk::GaussianDerivativeOperator<double, 3>;

  GaussianOp op1;

  ITK_EXERCISE_BASIC_OBJECT_METHODS((&op1), GaussianDerivativeOperator, NeighborhoodOperator);

  GaussianOp op2;

  // Check assignment
  op2 = op1;


  bool testStatus = true;

  testStatus &= TestGaussianOperator(.2, .001, 30, 0, 1.0);
  testStatus &= TestGaussianOperator(.2, .001, 30, 1, 1.0);
  testStatus &= TestGaussianOperator(.2, .001, 30, 2, 1.0);
  testStatus &= TestGaussianOperator(.2, .001, 30, 3, 1.0);
  testStatus &= TestGaussianOperator(.2, .001, 30, 4, 1.0);

  testStatus &= TestGaussianOperator(1, .001, 30, 0, 1.0);
  testStatus &= TestGaussianOperator(1, .001, 30, 1, 1.0);
  testStatus &= TestGaussianOperator(1, .001, 30, 2, 1.0);
  testStatus &= TestGaussianOperator(1, .001, 30, 3, 1.0);
  testStatus &= TestGaussianOperator(1, .001, 30, 4, 1.0);

  testStatus &= TestGaussianOperator(10, .001, 30, 0, 1.0);
  testStatus &= TestGaussianOperator(10, .001, 30, 1, 1.0);

  testStatus &= TestGaussianOperator(10, .0001, 100, 1, 1.0);

  testStatus &= TestGaussianOperator(50, .001, 300, 0, 1.0);

  // Test streaming enumeration for GaussianDerivativeOperatorEnums::InterpolationMode elements
  const std::set<itk::GaussianDerivativeOperatorEnums::InterpolationMode> allInterpolationMode{
    itk::GaussianDerivativeOperatorEnums::InterpolationMode::NearestNeighbourInterpolation,
    itk::GaussianDerivativeOperatorEnums::InterpolationMode::LinearInterpolation
  };
  for (const auto & ee : allInterpolationMode)
  {
    std::cout << "STREAMED ENUM VALUE GaussianDerivativeOperatorEnums::InterpolationMode: " << ee << std::endl;
  }

  std::cout << "Test finished." << std::endl;
  if (testStatus)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}
