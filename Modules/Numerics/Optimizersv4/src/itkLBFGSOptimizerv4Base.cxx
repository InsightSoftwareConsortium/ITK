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
#define ITK_TEMPLATE_EXPLICIT_LBFGSOptimizerBasev4
#include "itkLBFGSOptimizerBasev4.h"

extern "C"
{
  extern double
  v3p_netlib_dpmeps_();
}

namespace itk
{


template <typename TInternalVnlOptimizerType>
LBFGSOptimizerBasev4<TInternalVnlOptimizerType>::LBFGSOptimizerBasev4()

{
  Superclass::SetNumberOfIterations(500);
}

template <typename TInternalVnlOptimizerType>
void
LBFGSOptimizerBasev4<TInternalVnlOptimizerType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Trace: ";
  if (m_Trace)
  {
    os << "On";
  }
  else
  {
    os << "Off";
  }
  os << std::endl;
  os << indent << "MaximumNumberOfFunctionEvaluations: " << m_MaximumNumberOfFunctionEvaluations << std::endl;
  os << indent << "GradientConvergenceTolerance: " << m_GradientConvergenceTolerance << std::endl;
}

template <typename TInternalVnlOptimizerType>
void
LBFGSOptimizerBasev4<TInternalVnlOptimizerType>::SetTrace(bool flag)
{
  if (flag == m_Trace)
  {
    return;
  }

  m_Trace = flag;
  if (m_OptimizerInitialized)
  {
    m_VnlOptimizer->set_trace(m_Trace);
  }

  this->Modified();
}

template <typename TInternalVnlOptimizerType>
void
LBFGSOptimizerBasev4<TInternalVnlOptimizerType>::SetMaximumNumberOfFunctionEvaluations(unsigned int n)
{
  if (n == m_MaximumNumberOfFunctionEvaluations)
  {
    return;
  }

  m_MaximumNumberOfFunctionEvaluations = n;
  if (m_OptimizerInitialized)
  {
    m_VnlOptimizer->set_max_function_evals(static_cast<int>(m_MaximumNumberOfFunctionEvaluations));
  }

  this->Modified();
}

template <typename TInternalVnlOptimizerType>
void
LBFGSOptimizerBasev4<TInternalVnlOptimizerType>::SetGradientConvergenceTolerance(double f)
{
  if (Math::ExactlyEquals(f, m_GradientConvergenceTolerance))
  {
    return;
  }

  m_GradientConvergenceTolerance = f;
  if (m_OptimizerInitialized)
  {
    m_VnlOptimizer->set_g_tolerance(m_GradientConvergenceTolerance);
  }

  this->Modified();
}

template <typename TInternalVnlOptimizerType>
void
LBFGSOptimizerBasev4<TInternalVnlOptimizerType>::SetMetric(MetricType * metric)
{
  // assign to base class
  this->m_Metric = metric;

  // assign to vnl cost-function adaptor
  const unsigned int numberOfParameters = metric->GetNumberOfParameters();

  auto * adaptor = new CostFunctionAdaptorType(numberOfParameters);

  adaptor->SetCostFunction(metric);

  this->SetCostFunctionAdaptor(adaptor);

  m_VnlOptimizer = std::make_unique<InternalOptimizerType>(*adaptor, this);
}

template <typename TInternalVnlOptimizerType>
void
LBFGSOptimizerBasev4<TInternalVnlOptimizerType>::StartOptimization(bool /* doOnlyInitialization */)
{
  // Check for a local-support transform.
  // These aren't currently supported, see main class documentation.
  if (this->GetMetric()->HasLocalSupport())
  {
    itkExceptionMacro("The assigned transform has local-support. This is not supported for this optimizer. See the "
                      "optimizer documentation.");
  }

  // Perform some verification, check scales,
  // pass settings to cost-function adaptor.
  Superclass::StartOptimization();
}

template <typename TInternalVnlOptimizerType>
auto
LBFGSOptimizerBasev4<TInternalVnlOptimizerType>::GetOptimizer() -> InternalOptimizerType *
{
  return m_VnlOptimizer.get();
}

template <typename TInternalVnlOptimizerType>
const std::string
LBFGSOptimizerBasev4<TInternalVnlOptimizerType>::GetStopConditionDescription() const
{
  m_StopConditionDescription.str("");
  m_StopConditionDescription << this->GetNameOfClass() << ": ";
  if (m_VnlOptimizer)
  {
    switch (m_VnlOptimizer->get_failure_code())
    {
      case vnl_nonlinear_minimizer::ERROR_FAILURE:
        m_StopConditionDescription << "Failure";
        break;
      case vnl_nonlinear_minimizer::ERROR_DODGY_INPUT:
        m_StopConditionDescription << "Dodgy input";
        break;
#if VXL_VERSION_MAJOR >= 4
      // ABNORMAL_TERMINATION_IN_LNSRCH stop condition added in VXL 4.0
      case vnl_nonlinear_minimizer::ABNORMAL_TERMINATION_IN_LNSRCH:
        m_StopConditionDescription
          << "Abnormal termination in line search.  Often caused by "
          << "rounding errors dominating computation.  This can occur if the function is a very "
          << "flat surface, or has oscillations.";
        break;
#endif
      case vnl_nonlinear_minimizer::CONVERGED_FTOL:
        m_StopConditionDescription << "Function tolerance reached after " << m_CurrentIteration << " iterations. "
                                   << "The relative reduction of the cost function <= "
                                   << m_CostFunctionConvergenceFactor * v3p_netlib_dpmeps_()
                                   << " = CostFunctionConvergenceFactor (" << m_CostFunctionConvergenceFactor
                                   << ") * machine precision (" << v3p_netlib_dpmeps_() << ").";
        break;
      case vnl_nonlinear_minimizer::CONVERGED_XTOL:
        m_StopConditionDescription << "Solution tolerance reached";
        break;
      case vnl_nonlinear_minimizer::CONVERGED_XFTOL:
        m_StopConditionDescription << "Solution and Function tolerance both reached";
        break;
      case vnl_nonlinear_minimizer::CONVERGED_GTOL:
        m_StopConditionDescription << "Gradient tolerance reached. "
                                   << "Gradient tolerance is " << m_GradientConvergenceTolerance;
        break;
      case vnl_nonlinear_minimizer::FAILED_TOO_MANY_ITERATIONS:
        m_StopConditionDescription << "Too many function evaluations. Function evaluations  = "
                                   << m_MaximumNumberOfFunctionEvaluations;
        break;
      case vnl_nonlinear_minimizer::FAILED_FTOL_TOO_SMALL:
        m_StopConditionDescription << "Function tolerance too small";
        break;
      case vnl_nonlinear_minimizer::FAILED_XTOL_TOO_SMALL:
        m_StopConditionDescription << "Solution tolerance too small";
        break;
      case vnl_nonlinear_minimizer::FAILED_GTOL_TOO_SMALL:
        m_StopConditionDescription << "Gradient tolerance too small";
        break;
      case vnl_nonlinear_minimizer::FAILED_USER_REQUEST:
        m_StopConditionDescription << "User requested";
        break;
    }
    return m_StopConditionDescription.str();
  }
  else
  {
    return std::string("");
  }
}

template class ITKOptimizersv4_EXPORT LBFGSOptimizerBasev4<vnl_lbfgs>;
template class ITKOptimizersv4_EXPORT LBFGSOptimizerBasev4<vnl_lbfgsb>;

} // end namespace itk
