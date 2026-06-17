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
#include "itkEigenLevenbergMarquardtEngine.h"

#include <algorithm>
#include <sstream>

namespace itk
{
LevenbergMarquardtOptimizer::LevenbergMarquardtOptimizer()
  : m_NumberOfIterations(2000)
  , m_ValueTolerance(1e-8)
  , m_GradientTolerance(1e-5)
  , m_EpsilonFunction(1e-11)
{}

LevenbergMarquardtOptimizer::~LevenbergMarquardtOptimizer() = default;

/**
 * Connect a Cost Function
 */
void
LevenbergMarquardtOptimizer::SetCostFunction(MultipleValuedCostFunction * costFunction)
{
  const unsigned int numberOfParameters = costFunction->GetNumberOfParameters();
  const unsigned int numberOfValues = costFunction->GetNumberOfValues();

  auto * adaptor = new CostFunctionAdaptorType(numberOfParameters, numberOfValues);
  adaptor->SetCostFunction(costFunction);
  this->SetCostFunctionAdaptor(adaptor);

  m_OptimizerInitialized = true;
}

/** Return Current Value */
LevenbergMarquardtOptimizer::MeasureType
LevenbergMarquardtOptimizer::GetValue() const
{
  MeasureType measures;

  const CostFunctionAdaptorType * adaptor = this->GetCostFunctionAdaptor();

  if (adaptor)
  {
    const MultipleValuedCostFunction * costFunction = adaptor->GetCostFunction();
    if (costFunction)
    {
      const unsigned int numberOfValues = costFunction->GetNumberOfValues();
      measures.SetSize(numberOfValues);
      ParametersType parameters = this->GetCurrentPosition();
      if (m_ScalesInitialized)
      {
        const ScalesType & scales = this->GetScales();
        for (unsigned int i = 0; i < parameters.size(); ++i)
        {
          parameters[i] *= scales[i];
        }
      }
      this->GetNonConstCostFunctionAdaptor()->f(parameters, measures);
    }
  }
  return measures;
}

/**
 * Start the optimization
 */
void
LevenbergMarquardtOptimizer::StartOptimization()
{
  this->InvokeEvent(StartEvent());

  const ParametersType initialPosition = GetInitialPosition();
  ParametersType       parameters(initialPosition);

  // If the user provides the scales then we set otherwise we don't
  // for computation speed.
  // We also scale the initial parameters up if scales are defined.
  // This compensates for later scaling them down in the cost function adaptor
  // and at the end of this function.
  if (m_ScalesInitialized)
  {
    const ScalesType & scales = this->GetScales();
    this->GetNonConstCostFunctionAdaptor()->SetScales(scales);
    for (unsigned int i = 0; i < parameters.size(); ++i)
    {
      parameters[i] *= scales[i];
    }
  }

  CostFunctionAdaptorType * adaptor = this->GetNonConstCostFunctionAdaptor();
  const unsigned int        numberOfParameters = parameters.size();
  const unsigned int        numberOfResiduals = adaptor->get_number_of_residuals();

  auto residual = [adaptor, numberOfParameters, numberOfResiduals](const double * x, double * r) {
    const vnl_vector<double> vx(x, numberOfParameters);
    vnl_vector<double>       vr(numberOfResiduals);
    adaptor->f(vx, vr);
    std::copy_n(vr.data_block(), numberOfResiduals, r);
  };

  std::function<void(const double *, double *)> jacobian;
  if (adaptor->GetUseGradient())
  {
    jacobian = [adaptor, numberOfParameters, numberOfResiduals](const double * x, double * j) {
      const vnl_vector<double> vx(x, numberOfParameters);
      vnl_matrix<double>       vj(numberOfResiduals, numberOfParameters);
      adaptor->gradf(vx, vj);
      // vnl_matrix and the engine both store the Jacobian row-major (m-by-n).
      std::copy_n(vj.data_block(), static_cast<size_t>(numberOfResiduals) * numberOfParameters, j);
    };
  }

  detail::EigenLevenbergMarquardtOptions options;
  options.maxFunctionEvaluations = m_NumberOfIterations;
  options.xTolerance = m_ValueTolerance;
  options.gradientTolerance = m_GradientTolerance;
  options.epsilonFunction = m_EpsilonFunction;
  // Fixed ftol matching vnl_nonlinear_minimizer's default (xtol 1e-8 * 0.01).
  options.functionTolerance = 1e-10;

  const detail::EigenLevenbergMarquardtResult result = detail::EigenLevenbergMarquardtSolve(
    numberOfParameters, numberOfResiduals, residual, jacobian, parameters, options);

  parameters = result.solution;
  m_StopConditionDescription = result.converged ? "Levenberg-Marquardt converged"
                                                : "Levenberg-Marquardt did not converge (iteration/tolerance limit)";

  // we scale the parameters down if scales are defined
  if (m_ScalesInitialized)
  {
    const ScalesType & invScales = this->GetInverseScales();
    for (unsigned int i = 0; i < parameters.size(); ++i)
    {
      parameters[i] *= invScales[i];
    }
  }

  this->SetCurrentPosition(parameters);

  this->InvokeEvent(EndEvent());
}

void
LevenbergMarquardtOptimizer::SetNumberOfIterations(unsigned int iterations)
{
  m_NumberOfIterations = iterations;
}

void
LevenbergMarquardtOptimizer::SetValueTolerance(double tol)
{
  m_ValueTolerance = tol;
}

void
LevenbergMarquardtOptimizer::SetGradientTolerance(double tol)
{
  m_GradientTolerance = tol;
}

void
LevenbergMarquardtOptimizer::SetEpsilonFunction(double epsilon)
{
  m_EpsilonFunction = epsilon;
}

#if !defined(ITK_LEGACY_REMOVE)
vnl_levenberg_marquardt *
LevenbergMarquardtOptimizer::GetOptimizer() const
{
  // Eigen-engine based; no vnl_levenberg_marquardt instance is exposed.
  return nullptr;
}
#endif

std::string
LevenbergMarquardtOptimizer::GetStopConditionDescription() const
{
  std::ostringstream reason;
  reason << this->GetNameOfClass() << ": " << m_StopConditionDescription;
  return reason.str();
}
} // end namespace itk
