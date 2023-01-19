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
#include "itkMath.h"

using MatrixType = vnl_matrix<double>;
using VectorType = vnl_vector<double>;


constexpr double ra = 11.0;
constexpr double rb = 17.0;
constexpr double rc = 29.0;

/**
 *
 *   This example minimize the equation:
 *
 *   sum { [   (  a * x +  b * y +  c )
 *            -( 11 * x + 17 * y + 29 ) ] ^ 2  }
 *
 *   for the (a,b,c) parameters
 *
 *   the solution is the vector |  11  17  29  |
 *
 *   (x,y) values are sampled over a rectangular region
 *   whose size is defined by XRange and YRange
 * \class LMCostFunction
 *
 */
class LMCostFunction : public itk::MultipleValuedCostFunction
{
public:
  using Self = LMCostFunction;
  using Superclass = itk::MultipleValuedCostFunction;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);

  enum
  {
    XRange = 2,
    YRange = 2
  }; // size of the region to sample the cost function

  enum
  {
    SpaceDimension = 3
  };
  enum
  {
    RangeDimension = (2 * XRange + 1) * (2 * YRange + 1)
  };

  using ParametersType = Superclass::ParametersType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  LMCostFunction()
    : m_Measure(RangeDimension)
    , m_Derivative(SpaceDimension, RangeDimension)
    , m_TheoreticalData(SpaceDimension)
  {

    m_Measure.SetSize(RangeDimension);
    m_Derivative.SetSize(SpaceDimension, RangeDimension);
    m_TheoreticalData.SetSize(RangeDimension);

    // Compute points of the function over a square region
    unsigned int valueindex = 0;
    for (int y = -YRange; y <= YRange; ++y)
    {
      const auto yd = static_cast<double>(y);
      for (int x = -XRange; x <= XRange; ++x)
      {
        const auto xd = static_cast<double>(x);
        m_TheoreticalData[valueindex] = ra * xd + rb * yd + rc;
        valueindex++;
      }
    }
  }


  MeasureType
  GetValue(const ParametersType & parameters) const override
  {

    std::cout << "GetValue( ";
    double a = parameters[0];
    double b = parameters[1];
    double c = parameters[2];

    std::cout << a << " , ";
    std::cout << b << " , ";
    std::cout << c << ")  " << std::endl;

    // Compute points of the function over a square region
    unsigned int valueindex = 0;
    for (int y = -YRange; y <= YRange; ++y)
    {
      const auto yd = static_cast<double>(y);
      for (int x = -XRange; x <= XRange; ++x)
      {
        const auto xd = static_cast<double>(x);
        double     value = a * xd + b * yd + c;
        value -= m_TheoreticalData[valueindex];
        m_Measure[valueindex] = value;
        valueindex++;
      }
    }

    return m_Measure;
  }

  void
  GetDerivative(const ParametersType & parameters, DerivativeType & derivative) const override
  {

    std::cout << "GetDerivative( ";
    double a = parameters[0];
    double b = parameters[1];
    double c = parameters[2];

    std::cout << a << " , ";
    std::cout << b << " , ";
    std::cout << c << ") " << std::endl;

    // Compute points of the function over a square region
    unsigned int valueindex = 0;
    for (int y = -YRange; y <= YRange; ++y)
    {
      const auto yd = static_cast<double>(y);
      for (int x = -XRange; x <= XRange; ++x)
      {
        const auto xd = static_cast<double>(x);
        m_Derivative[0][valueindex] = xd;
        m_Derivative[1][valueindex] = yd;
        m_Derivative[2][valueindex] = 1.0;
        valueindex++;
      }
    }

    derivative = m_Derivative;
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

  unsigned int
  GetNumberOfValues() const override
  {
    return RangeDimension;
  }

private:
  mutable MeasureType    m_Measure;
  mutable DerivativeType m_Derivative;
  MeasureType            m_TheoreticalData;
};

class CommandIterationUpdateLevenbergMarquardt : public itk::Command
{
public:
  using Self = CommandIterationUpdateLevenbergMarquardt;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  CommandIterationUpdateLevenbergMarquardt() { m_IterationNumber = 0; }

public:
  using OptimizerType = itk::LevenbergMarquardtOptimizer;
  using OptimizerPointer = const OptimizerType *;

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    std::cout << "Observer::Execute() " << std::endl;
    auto optimizer = static_cast<OptimizerPointer>(object);
    if (m_FunctionEvent.CheckEvent(&event))
    {
      std::cout << m_IterationNumber++ << "   ";
      std::cout << optimizer->GetCachedValue() << "   ";
      std::cout << optimizer->GetCachedCurrentPosition() << std::endl;
    }
    else if (m_GradientEvent.CheckEvent(&event))
    {
      std::cout << "Gradient " << optimizer->GetCachedDerivative() << "   ";
    }
  }

private:
  unsigned long m_IterationNumber;

  itk::FunctionEvaluationIterationEvent m_FunctionEvent;
  itk::GradientEvaluationIterationEvent m_GradientEvent;
};

int
itkRunLevenbergMarquardOptimization(bool   useGradient,
                                    double fTolerance,
                                    double gTolerance,
                                    double xTolerance,
                                    double epsilonFunction,
                                    int    maxIterations)
{
  std::cout << "Levenberg Marquardt optimizer test \n \n";

  using OptimizerType = itk::LevenbergMarquardtOptimizer;

  using vnlOptimizerType = OptimizerType::InternalOptimizerType;

  // Declaration of an itkOptimizer
  auto optimizer = OptimizerType::New();

  // Declaration of the CostFunction adaptor
  auto costFunction = LMCostFunction::New();

  using ParametersType = LMCostFunction::ParametersType;
  ParametersType parameters(LMCostFunction::SpaceDimension);
  parameters.Fill(0.0);
  costFunction->GetValue(parameters);

  std::cout << "Number of Values = " << costFunction->GetNumberOfValues() << "\n";

  try
  {
    optimizer->SetCostFunction(costFunction);
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << e << std::endl;
    return EXIT_FAILURE;
  }

  // this following call is equivalent to invoke: costFunction->SetUseGradient( useGradient );
  optimizer->GetUseCostFunctionGradient();
  optimizer->UseCostFunctionGradientOn();
  optimizer->UseCostFunctionGradientOff();
  optimizer->SetUseCostFunctionGradient(useGradient);


  vnlOptimizerType * vnlOptimizer = optimizer->GetOptimizer();

  vnlOptimizer->set_f_tolerance(fTolerance);
  vnlOptimizer->set_g_tolerance(gTolerance);
  vnlOptimizer->set_x_tolerance(xTolerance);
  vnlOptimizer->set_epsilon_function(epsilonFunction);
  vnlOptimizer->set_max_function_evals(maxIterations);

  // We start not so far from the solution
  using ParametersType = LMCostFunction::ParametersType;
  ParametersType initialValue(LMCostFunction::SpaceDimension);

  initialValue[0] = 200;
  initialValue[1] = 300;
  initialValue[2] = 400;

  OptimizerType::ParametersType currentValue(LMCostFunction::SpaceDimension);

  currentValue = initialValue;

  optimizer->SetInitialPosition(currentValue);

  auto observer = CommandIterationUpdateLevenbergMarquardt::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);
  optimizer->AddObserver(itk::FunctionEvaluationIterationEvent(), observer);

  try
  {
    optimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation() << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
  }


  // Error codes taken from vxl/vnl/vnl_nonlinear_minimizer.h
  std::cout << "End condition   = ";
  switch (vnlOptimizer->get_failure_code())
  {
    case vnl_nonlinear_minimizer::ERROR_FAILURE:
      std::cout << " Error Failure";
      break;
    case vnl_nonlinear_minimizer::ERROR_DODGY_INPUT:
      std::cout << " Error Dogy Input";
      break;
#if VXL_VERSION_MAJOR >= 4
    // ABNORMAL_TERMINATION_IN_LNSRCH stop condition added in VXL 4.0
    case vnl_nonlinear_minimizer::ABNORMAL_TERMINATION_IN_LNSRCH:
      std::cout << "Abnormal termination in line search.  Often caused by "
                << "rounding errors dominating computation.  This can occur if the function is a very "
                << "flat surface, or has oscillations.";
      break;
#endif
    case vnl_nonlinear_minimizer::CONVERGED_FTOL:
      std::cout << " Converged F  Tolerance";
      break;
    case vnl_nonlinear_minimizer::CONVERGED_XTOL:
      std::cout << " Converged X  Tolerance";
      break;
    case vnl_nonlinear_minimizer::CONVERGED_XFTOL:
      std::cout << " Converged XF Tolerance";
      break;
    case vnl_nonlinear_minimizer::CONVERGED_GTOL:
      std::cout << " Converged G  Tolerance";
      break;
    case vnl_nonlinear_minimizer::FAILED_TOO_MANY_ITERATIONS:
      std::cout << " Too many iterations   ";
      break;
    case vnl_nonlinear_minimizer::FAILED_FTOL_TOO_SMALL:
      std::cout << " Failed F Tolerance too small ";
      break;
    case vnl_nonlinear_minimizer::FAILED_XTOL_TOO_SMALL:
      std::cout << " Failed X Tolerance too small ";
      break;
    case vnl_nonlinear_minimizer::FAILED_GTOL_TOO_SMALL:
      std::cout << " Failed G Tolerance too small ";
      break;
    case vnl_nonlinear_minimizer::FAILED_USER_REQUEST:
      std::cout << " Failed user request ";
      break;
  }
  std::cout << std::endl;
  std::cout << "Stop description   = " << optimizer->GetStopConditionDescription() << std::endl;
  std::cout << "Number of iters = " << vnlOptimizer->get_num_iterations() << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer->get_num_evaluations() << std::endl;
  std::cout << std::endl;


  OptimizerType::ParametersType finalPosition;
  finalPosition = optimizer->GetCurrentPosition();

  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ",";
  std::cout << finalPosition[1] << ",";
  std::cout << finalPosition[2] << ")" << std::endl;


  //
  // check results to see if it is within range
  //
  bool   pass = true;
  double trueParameters[3] = { ra, rb, rc };
  for (unsigned int j = 0; j < LMCostFunction::SpaceDimension; ++j)
  {
    if (itk::Math::abs(finalPosition[j] - trueParameters[j]) > 0.01)
    {
      pass = false;
    }
  }

  if (!pass)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  // Get the final value of the optimizer
  std::cout << "Testing GetValue() : ";
  OptimizerType::MeasureType finalValue = optimizer->GetValue();

  // We compare only the first value for this test
  if (itk::Math::abs(finalValue[0] - 0.0) > 0.01)
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[SUCCESS]" << std::endl;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}


int
itkLevenbergMarquardtOptimizerTest(int argc, char * argv[])
{
  std::cout << "Levenberg Marquardt optimizer test \n \n";


  bool useGradient;
  int  result;

  double F_Tolerance = 1e-2;      // Function value tolerance
  double G_Tolerance = 1e-2;      // Gradient magnitude tolerance
  double X_Tolerance = 1e-5;      // Search space tolerance
  double Epsilon_Function = 1e-9; // Step
  int    Max_Iterations = 200;    // Maximum number of iterations

  if (argc > 1)
  {
    F_Tolerance = std::stod(argv[1]);
  }

  if (argc > 2)
  {
    G_Tolerance = std::stod(argv[2]);
  }

  if (argc > 3)
  {
    X_Tolerance = std::stod(argv[3]);
  }

  if (argc > 4)
  {
    Epsilon_Function = std::stod(argv[4]);
  }

  if (argc > 5)
  {
    Max_Iterations = std::stoi(argv[5]);
  }

  std::cout << "F_Tolerance      = " << F_Tolerance << std::endl;
  std::cout << "G_Tolerance      = " << G_Tolerance << std::endl;
  std::cout << "X_Tolerance      = " << X_Tolerance << std::endl;
  std::cout << "Epsilon_Function = " << Epsilon_Function << std::endl;
  std::cout << "Max_Iterations   = " << Max_Iterations << std::endl;

  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << "Running using the Gradient computed by vnl " << std::endl;
  useGradient = false;
  result = itkRunLevenbergMarquardOptimization(
    useGradient, F_Tolerance, G_Tolerance, X_Tolerance, Epsilon_Function, Max_Iterations);
  if (result == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << "Running using the Gradient provided by the Cost function" << std::endl;
  useGradient = true;
  result = itkRunLevenbergMarquardOptimization(
    useGradient, F_Tolerance, G_Tolerance, X_Tolerance, Epsilon_Function, Max_Iterations);
  if (result == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
