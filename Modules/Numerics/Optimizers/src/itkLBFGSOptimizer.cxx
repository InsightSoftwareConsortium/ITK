/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef _itkLBFGSOptimizer_hxx
#define _itkLBFGSOptimizer_hxx

#include "itkLBFGSOptimizer.h"
#include "itkMath.h"

namespace itk
{
/**
 * Constructor
 */
LBFGSOptimizer
::LBFGSOptimizer()
{
  m_OptimizerInitialized    = false;
  m_VnlOptimizer            = ITK_NULLPTR;
  m_Trace                              = false;
  m_MaximumNumberOfFunctionEvaluations = 2000;
  m_GradientConvergenceTolerance       = 1e-5;
  m_LineSearchAccuracy                 = 0.9;
  m_DefaultStepLength                  = 1.0;
}

/**
 * Destructor
 */
LBFGSOptimizer
::~LBFGSOptimizer()
{
  delete m_VnlOptimizer;
}

/**
 * PrintSelf
 */
void
LBFGSOptimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Trace: ";
  if ( m_Trace )
    {
    os << "On";
    }
  else
    {
    os << "Off";
    }
  os << std::endl;
  os << indent << "MaximumNumberOfFunctionEvaluations: "
     << m_MaximumNumberOfFunctionEvaluations << std::endl;
  os << indent << "GradientConvergenceTolerance: "
     << m_GradientConvergenceTolerance << std::endl;
  os << indent << "LineSearchAccuracy: "
     << m_LineSearchAccuracy << std::endl;
  os << indent << "DefaultStepLength: "
     << m_DefaultStepLength << std::endl;
}

/**
 * Set the optimizer trace flag
 */
void
LBFGSOptimizer
::SetTrace(bool flag)
{
  if ( flag == m_Trace )
    {
    return;
    }

  m_Trace = flag;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_trace(m_Trace);
    }

  this->Modified();
}

/**
 * Set the maximum number of function evalutions
 */
void
LBFGSOptimizer
::SetMaximumNumberOfFunctionEvaluations(unsigned int n)
{
  if ( n == m_MaximumNumberOfFunctionEvaluations )
    {
    return;
    }

  m_MaximumNumberOfFunctionEvaluations = n;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_max_function_evals(
      static_cast< int >( m_MaximumNumberOfFunctionEvaluations ) );
    }

  this->Modified();
}

/**
 * Set the gradient convergence tolerance
 */
void
LBFGSOptimizer
::SetGradientConvergenceTolerance(double f)
{
  if ( Math::ExactlyEquals(f, m_GradientConvergenceTolerance) )
    {
    return;
    }

  m_GradientConvergenceTolerance = f;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_g_tolerance(m_GradientConvergenceTolerance);
    }

  this->Modified();
}

/**
 * Set the line search accuracy
 */
void
LBFGSOptimizer
::SetLineSearchAccuracy(double f)
{
  if ( Math::ExactlyEquals(f, m_LineSearchAccuracy) )
    {
    return;
    }

  m_LineSearchAccuracy = f;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->line_search_accuracy = m_LineSearchAccuracy;
    }

  this->Modified();
}

/**
 * Set the default step length
 */
void
LBFGSOptimizer
::SetDefaultStepLength(double f)
{
  if ( Math::ExactlyEquals(f, m_DefaultStepLength) )
    {
    return;
    }

  m_DefaultStepLength = f;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->default_step_length = m_DefaultStepLength;
    }

  this->Modified();
}

/** Return Current Value */
LBFGSOptimizer::MeasureType
LBFGSOptimizer
::GetValue() const
{
  return this->GetCachedValue();
}

/**
 * Connect a Cost Function
 */
void
LBFGSOptimizer
::SetCostFunction(SingleValuedCostFunction *costFunction)
{
  const unsigned int numberOfParameters =
    costFunction->GetNumberOfParameters();

  CostFunctionAdaptorType *adaptor =
    new CostFunctionAdaptorType(numberOfParameters);

  adaptor->SetCostFunction(costFunction);

  if ( m_OptimizerInitialized )
    {
    delete m_VnlOptimizer;
    }

  this->SetCostFunctionAdaptor(adaptor);

  m_VnlOptimizer = new vnl_lbfgs(*adaptor);

  // set the optimizer parameters
  m_VnlOptimizer->set_trace(m_Trace);
  m_VnlOptimizer->set_max_function_evals(
    static_cast< int >( m_MaximumNumberOfFunctionEvaluations ) );
  m_VnlOptimizer->set_g_tolerance(m_GradientConvergenceTolerance);
  m_VnlOptimizer->line_search_accuracy = m_LineSearchAccuracy;
  m_VnlOptimizer->default_step_length  = m_DefaultStepLength;

  m_OptimizerInitialized = true;

  this->Modified();
}

/**
 * Start the optimization
 */
void
LBFGSOptimizer
::StartOptimization(void)
{
  this->InvokeEvent( StartEvent() );

  if ( this->GetMaximize() )
    {
    this->GetNonConstCostFunctionAdaptor()->NegateCostFunctionOn();
    }

  ParametersType currentPositionInternalValue = this->GetInitialPosition();

  // We also scale the initial vnlCompatibleParameters up if scales are defined.
  // This compensates for later scaling them down in the cost function adaptor
  // and at the end of this function.
  InternalParametersType vnlCompatibleParameters(currentPositionInternalValue.size());
  const ScalesType & scales = this->GetScales();
  if ( m_ScalesInitialized )
    {
    this->GetNonConstCostFunctionAdaptor()->SetScales(scales);
    }
  for ( unsigned int i = 0; i < vnlCompatibleParameters.size(); ++i )
    {
    vnlCompatibleParameters[i] = (m_ScalesInitialized)
      ? currentPositionInternalValue[i] * scales[i]
      : currentPositionInternalValue[i];
    }

  // vnl optimizers return the solution by reference
  // in the variable provided as initial position
  m_VnlOptimizer->minimize(vnlCompatibleParameters);

  if ( vnlCompatibleParameters.size() != currentPositionInternalValue.size() )
    {
    // set current position to initial position and throw an exception
    this->SetCurrentPosition(currentPositionInternalValue);
    itkExceptionMacro(<< "Error occurred in optimization");
    }

  // we scale the vnlCompatibleParameters down if scales are defined
  const ScalesType & invScales = this->GetInverseScales();
  for ( unsigned int i = 0; i < vnlCompatibleParameters.size(); ++i )
    {
    currentPositionInternalValue[i] = (m_ScalesInitialized)
      ? vnlCompatibleParameters[i] * invScales[i]
      : vnlCompatibleParameters[i];
    }

  this->SetCurrentPosition(currentPositionInternalValue);
  this->InvokeEvent( EndEvent() );
}

/**
 * Get the Optimizer
 */
vnl_lbfgs *
LBFGSOptimizer
::GetOptimizer()
{
  return m_VnlOptimizer;
}

const std::string
LBFGSOptimizer::GetStopConditionDescription() const
{
  m_StopConditionDescription.str("");
  m_StopConditionDescription << this->GetNameOfClass() << ": ";
  if ( m_VnlOptimizer )
    {
    switch ( m_VnlOptimizer->get_failure_code() )
      {
      case vnl_nonlinear_minimizer::ERROR_FAILURE:
        m_StopConditionDescription << "Failure";
        break;
      case vnl_nonlinear_minimizer::ERROR_DODGY_INPUT:
        m_StopConditionDescription << "Dodgy input";
        break;
      case vnl_nonlinear_minimizer::CONVERGED_FTOL:
        m_StopConditionDescription << "Function tolerance reached";
        break;
      case vnl_nonlinear_minimizer::CONVERGED_XTOL:
        m_StopConditionDescription << "Solution tolerance reached";
        break;
      case vnl_nonlinear_minimizer::CONVERGED_XFTOL:
        m_StopConditionDescription << "Solution and Function tolerance both reached";
        break;
      case vnl_nonlinear_minimizer::CONVERGED_GTOL:
        m_StopConditionDescription << "Gradient tolerance reached";
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
} // end namespace itk

#endif
