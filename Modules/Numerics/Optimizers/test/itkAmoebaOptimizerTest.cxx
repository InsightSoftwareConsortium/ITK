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

#include "itkAmoebaOptimizer.h"
#include "itkTestingMacros.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "itkMath.h"
#include <iostream>

/**
 *  The objective function is the quadratic form:
 *
 *  1/2 x^T A x - b^T x
 *
 *  Where A is represented as an itkMatrix and
 *  b is represented as an itkVector
 *
 *  The system in this example is:
 *
 *     | 3  2 ||x|   | 2|   |0|
 *     | 2  6 ||y| + |-8| = |0|
 *
 *
 *   the solution is the vector | 2 -2 |
 *
 *   and the expected final value of the function is 10.0
 *
 * \class amoebaTestF1
 */
class amoebaTestF1 : public itk::SingleValuedCostFunction
{
public:
  using Self = amoebaTestF1;
  using Superclass = itk::SingleValuedCostFunction;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(amoebaTestF1, SingleValuedCostFunction);

  enum
  {
    SpaceDimension = 2
  };

  using ParametersType = Superclass::ParametersType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  using VectorType = vnl_vector<double>;
  using MatrixType = vnl_matrix<double>;


  amoebaTestF1()
    : m_A(SpaceDimension, SpaceDimension)
    , m_B(SpaceDimension)
  {
    m_A[0][0] = 3;
    m_A[0][1] = 2;
    m_A[1][0] = 2;
    m_A[1][1] = 6;

    m_B[0] = 2;
    m_B[1] = -8;
    m_Negate = false;
  }

  double
  GetValue(const ParametersType & parameters) const override
  {

    VectorType v(parameters.Size());
    for (unsigned int i = 0; i < SpaceDimension; ++i)
    {
      v[i] = parameters[i];
    }
    VectorType Av = m_A * v;
    double     val = (inner_product<double>(Av, v)) / 2.0;
    val -= inner_product<double>(m_B, v);
    if (m_Negate)
    {
      val *= -1.0;
    }
    return val;
  }

  void
  GetDerivative(const ParametersType & parameters, DerivativeType & derivative) const override
  {

    VectorType v(parameters.Size());
    for (unsigned int i = 0; i < SpaceDimension; ++i)
    {
      v[i] = parameters[i];
    }
    std::cout << "GetDerivative( " << v << " ) = ";
    VectorType gradient = m_A * v - m_B;
    std::cout << gradient << std::endl;
    derivative = DerivativeType(SpaceDimension);
    for (unsigned int i = 0; i < SpaceDimension; ++i)
    {
      if (!m_Negate)
      {
        derivative[i] = gradient[i];
      }
      else
      {
        derivative[i] = -gradient[i];
      }
    }
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

  // Used to switch between maximization and minimization.
  void
  SetNegate(bool flag)
  {
    m_Negate = flag;
  }

private:
  MatrixType m_A;
  VectorType m_B;
  bool       m_Negate;
};


/**
 * Function we want to optimize, comprised of two parabolas with C0 continuity
 * at 0:
 * f(x) = if(x<0) x^2+4x; else 2x^2-8x
 *
 * Minima are at -2 and 2 with function values of -4 and -8 respectively.
 */
class amoebaTestF2 : public itk::SingleValuedCostFunction
{
public:
  using Self = amoebaTestF2;
  using Superclass = itk::SingleValuedCostFunction;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(amoebaTestF1, SingleValuedCostFunction);

  using ParametersType = Superclass::ParametersType;
  using MeasureType = Superclass::MeasureType;

  amoebaTestF2() = default;

  double
  GetValue(const ParametersType & parameters) const override
  {
    double val;
    if (parameters[0] < 0)
    {
      val = parameters[0] * parameters[0] + 4 * parameters[0];
    }
    else
    {
      val = 2 * parameters[0] * parameters[0] - 8 * parameters[0];
    }
    return val;
  }

  void
  GetDerivative(const ParametersType & itkNotUsed(parameters), DerivativeType & itkNotUsed(derivative)) const override
  {
    throw itk::ExceptionObject(__FILE__, __LINE__, "no derivative available");
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return 1;
  }
};

class CommandIterationUpdateAmoeba : public itk::Command
{
public:
  using Self = CommandIterationUpdateAmoeba;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

  void
  Reset()
  {
    m_IterationNumber = 0;
  }

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    const auto * optimizer = static_cast<const itk::AmoebaOptimizer *>(object);
    if (dynamic_cast<const itk::FunctionEvaluationIterationEvent *>(&event))
    {
      std::cout << m_IterationNumber++ << ":  ";
      std::cout << "x: " << optimizer->GetCachedCurrentPosition() << "  ";
      std::cout << "f(x): " << optimizer->GetCachedValue() << std::endl;
    }
  }

protected:
  CommandIterationUpdateAmoeba() { m_IterationNumber = 0; }

private:
  unsigned long m_IterationNumber;
};

/**
 * Test Amoeba with a 2D quadratic function - happy day scenario.
 */
int
AmoebaTest1();

/**
 * Test Amoeba and Amoeba with restarts on a function with two minima.
 */
int
AmoebaTest2();

int
itkAmoebaOptimizerTest(int, char *[])
{
  int result1 = AmoebaTest1();
  int result2 = AmoebaTest2();

  std::cout << "All Tests Completed." << std::endl;

  if (result1 == EXIT_FAILURE || result2 == EXIT_FAILURE)
  {
    std::cerr << "[FAILURE]\n";
    return EXIT_FAILURE;
  }
  std::cout << "[SUCCESS]\n";
  return EXIT_SUCCESS;
}

int
AmoebaTest1()
{

  std::cout << "Amoeba Optimizer Test 1\n \n";

  using OptimizerType = itk::AmoebaOptimizer;

  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, AmoebaOptimizer, SingleValuedNonLinearVnlOptimizer);


  // set optimizer parameters
  typename OptimizerType::NumberOfIterationsType numberOfIterations = 10;
  itkOptimizer->SetMaximumNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, itkOptimizer->GetMaximumNumberOfIterations());

  auto automaticInitialSimplex = true;
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, AutomaticInitialSimplex, automaticInitialSimplex);

  auto optimizeWithRestarts = false;
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, OptimizeWithRestarts, optimizeWithRestarts);

  double xTolerance = -0.01;
  itkOptimizer->SetParametersConvergenceTolerance(xTolerance);
  ITK_TEST_SET_GET_VALUE(xTolerance, itkOptimizer->GetParametersConvergenceTolerance());

  double fTolerance = -0.001;
  itkOptimizer->SetFunctionConvergenceTolerance(fTolerance);
  ITK_TEST_SET_GET_VALUE(fTolerance, itkOptimizer->GetFunctionConvergenceTolerance());

  auto costFunction = amoebaTestF1::New();
  itkOptimizer->SetCostFunction(costFunction);
  std::cout << "itkOptimizer->GetCostFunction(): " << itkOptimizer->GetCostFunction() << std::endl;

  OptimizerType::ParametersType initialValue(2); // constructor requires vector size

  initialValue[0] = 100; // We start not far from  | 2 -2 |
  initialValue[1] = -100;

  OptimizerType::ParametersType currentValue(2);

  currentValue = initialValue;

  itkOptimizer->SetInitialPosition(currentValue);

  // Test exceptions
  ITK_TRY_EXPECT_EXCEPTION(itkOptimizer->StartOptimization());

  xTolerance = 0.01;
  itkOptimizer->SetParametersConvergenceTolerance(xTolerance);
  ITK_TEST_SET_GET_VALUE(xTolerance, itkOptimizer->GetParametersConvergenceTolerance());

  ITK_TRY_EXPECT_EXCEPTION(itkOptimizer->StartOptimization());

  fTolerance = 0.001;
  itkOptimizer->SetFunctionConvergenceTolerance(fTolerance);
  ITK_TEST_SET_GET_VALUE(fTolerance, itkOptimizer->GetFunctionConvergenceTolerance());

  try
  {

    std::cout << "Run for " << itkOptimizer->GetMaximumNumberOfIterations();
    std::cout << " iterations or less." << std::endl;

    itkOptimizer->StartOptimization();


    itkOptimizer->SetMaximumNumberOfIterations(100);
    std::cout << "Continue for " << itkOptimizer->GetMaximumNumberOfIterations();
    std::cout << " iterations or less." << std::endl;
    itkOptimizer->SetInitialPosition(itkOptimizer->GetCurrentPosition());
    itkOptimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation() << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    std::cerr << "[TEST 1 FAILURE]\n";
    return EXIT_FAILURE;
  }


  std::cout << "Optimizer: " << itkOptimizer;

  //
  // check results to see if it is within range
  //

  OptimizerType::ParametersType finalPosition;
  finalPosition = itkOptimizer->GetCurrentPosition();

  double trueParameters[2] = { 2, -2 };
  bool   pass = true;

  std::cout << "Right answer   = " << trueParameters[0] << " , " << trueParameters[1] << std::endl;
  std::cout << "Final position = " << finalPosition << std::endl;

  for (unsigned int j = 0; j < 2; ++j)
  {
    if (itk::Math::abs(finalPosition[j] - trueParameters[j]) > xTolerance)
    {
      pass = false;
    }
  }

  if (!pass)
  {
    std::cerr << "[TEST 1 FAILURE]\n";
    return EXIT_FAILURE;
  }

  // Get the final value of the optimizer
  std::cout << "Testing optimizers GetValue() : ";
  OptimizerType::MeasureType finalValue = itkOptimizer->GetValue();
  if (itk::Math::abs(finalValue + 9.99998) > 0.01)
  {
    std::cerr << "failed\n";
    std::cerr << "[TEST 1 FAILURE]\n";
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "succeeded\n";
  }

  // Set now the function to maximize
  //
  { // add a block-scope to have local variables

    std::cout << "Testing Maximization " << std::endl;

    currentValue = initialValue;

    itkOptimizer->SetInitialPosition(currentValue);

    auto observer = CommandIterationUpdateAmoeba::New();
    itkOptimizer->AddObserver(itk::FunctionEvaluationIterationEvent(), observer);

    try
    {
      // These two following statement should compensate each other
      // and allow us to get to the same result as the test above.
      costFunction->SetNegate(true);
      itkOptimizer->MaximizeOn();

      std::cout << "Run for " << itkOptimizer->GetMaximumNumberOfIterations();
      std::cout << " iterations or less." << std::endl;

      itkOptimizer->StartOptimization();

      itkOptimizer->SetMaximumNumberOfIterations(100);
      itkOptimizer->SetInitialPosition(itkOptimizer->GetCurrentPosition());

      std::cout << "Continue for " << itkOptimizer->GetMaximumNumberOfIterations();
      std::cout << " iterations or less, starting from previous position.";
      std::cout << std::endl;
      itkOptimizer->StartOptimization();
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cerr << "Exception thrown ! " << std::endl;
      std::cerr << "An error occurred during Optimization" << std::endl;
      std::cerr << "Location    = " << e.GetLocation() << std::endl;
      std::cerr << "Description = " << e.GetDescription() << std::endl;
      std::cerr << "[TEST 1 FAILURE]\n";
      return EXIT_FAILURE;
    }

    finalPosition = itkOptimizer->GetCurrentPosition();
    std::cout << "Right answer   = " << trueParameters[0] << " , " << trueParameters[1] << std::endl;
    std::cout << "Final position = " << finalPosition << std::endl;

    for (unsigned int j = 0; j < 2; ++j)
    {
      if (itk::Math::abs(finalPosition[j] - trueParameters[j]) > xTolerance)
      {
        pass = false;
      }
    }

    if (!pass)
    {
      std::cerr << "[TEST 1 FAILURE]\n";
      return EXIT_FAILURE;
    }

    // Get the final value of the optimizer
    std::cout << "Testing optimizer's GetValue() [invokes additional function evaluation]: ";
    finalValue = itkOptimizer->GetValue();
    if (itk::Math::abs(finalValue + 9.99998) > 0.01)
    {
      std::cerr << "failed\n";
      std::cerr << "[TEST 1 FAILURE]\n";
      return EXIT_FAILURE;
    }
    else
    {
      std::cout << "succeeded\n";
    }
  }
  std::cout << "[TEST 1 SUCCESS]\n";
  return EXIT_SUCCESS;
}

int
AmoebaTest2()
{
  std::cout << "Amoeba Optimizer Test 2\n \n";

  using OptimizerType = itk::AmoebaOptimizer;
  auto itkOptimizer = OptimizerType::New();

  // set optimizer parameters
  unsigned int maxIterations = 100;
  itkOptimizer->SetMaximumNumberOfIterations(maxIterations);

  double xTolerance = 0.01;
  itkOptimizer->SetParametersConvergenceTolerance(xTolerance);

  double fTolerance = 0.001;
  itkOptimizer->SetFunctionConvergenceTolerance(fTolerance);

  // the initial simplex is constructed as:
  // x,
  // x_i = [x[0], ... , x[i]+initialSimplexDelta[i], ... , x[n]]
  //
  OptimizerType::ParametersType initialSimplexDelta(1);
  initialSimplexDelta[0] = 10;
  itkOptimizer->SetInitialSimplexDelta(initialSimplexDelta);
  ITK_TEST_SET_GET_VALUE(initialSimplexDelta, itkOptimizer->GetInitialSimplexDelta());

  OptimizerType::ParametersType initialParameters(1), finalParameters;
  // starting position
  initialParameters[0] = -100;

  itkOptimizer->SetInitialPosition(initialParameters);

  // the function we want to optimize
  auto costFunction = amoebaTestF2::New();
  itkOptimizer->SetCostFunction(costFunction);

  // observe the iterations
  auto observer = CommandIterationUpdateAmoeba::New();
  itkOptimizer->AddObserver(itk::IterationEvent(), observer);

  try
  {
    itkOptimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation() << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    std::cerr << "[TEST 2 FAILURE]\n";
    return EXIT_FAILURE;
  }

  // we should have converged to the local minimum, -2
  finalParameters = itkOptimizer->GetCurrentPosition();
  double knownParameters = -2.0;
  std::cout << "Standard Amoeba:\n";
  std::cout << "Known parameters   = " << knownParameters << "   ";
  std::cout << "Estimated parameters = " << finalParameters << std::endl;
  std::cout << "Converged to local minimum." << std::endl;
  if (itk::Math::abs(finalParameters[0] - knownParameters) > xTolerance)
  {
    std::cerr << "[TEST 2 FAILURE]\n";
    return EXIT_FAILURE;
  }

  // run again using multiple restarts
  observer->Reset();
  itkOptimizer->SetInitialPosition(initialParameters);
  itkOptimizer->OptimizeWithRestartsOn();

  try
  {
    itkOptimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation() << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    std::cerr << "[TEST 2 FAILURE]\n";
    return EXIT_FAILURE;
  }

  // we should have converged to the global minimum, 2
  finalParameters = itkOptimizer->GetCurrentPosition();
  knownParameters = 2.0;
  std::cout << "Amoeba with restarts:\n";
  std::cout << "Known parameters   = " << knownParameters << "   ";
  std::cout << "Estimated parameters = " << finalParameters << std::endl;
  std::cout << "Converged to global minimum." << std::endl;

  if (itk::Math::abs(finalParameters[0] - knownParameters) > xTolerance)
  {
    std::cerr << "[TEST 2 FAILURE]\n";
    return EXIT_FAILURE;
  }
  std::cout << "[TEST 1 SUCCESS]\n";
  return EXIT_SUCCESS;
}
