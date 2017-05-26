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
#include "itkLBFGSBOptimizer.h"
#include "vnl/algo/vnl_lbfgsb.h"

extern "C" {
extern double v3p_netlib_dpmeps_();
}

namespace itk
{
/** \class LBFGSBOptimizerHelper
 * \brief Wrapper helper around vnl_lbfgsb.
 *
 * This class is used to translate iteration events, etc, from
 * vnl_lbfgsb into iteration events in ITK.
 */
class ITKOptimizers_EXPORT LBFGSBOptimizerHelper:
  public vnl_lbfgsb
{
public:
  typedef LBFGSBOptimizerHelper Self;
  typedef vnl_lbfgsb            Superclass;

  /** Create with a reference to the ITK object */
  LBFGSBOptimizerHelper(vnl_cost_function & f,
                        LBFGSBOptimizer * const itkObj);

  /** Handle new iteration event */
  virtual bool report_iter() ITK_OVERRIDE;

private:
  LBFGSBOptimizer * const m_ItkObj;
};

/**
 * Constructor
 */
LBFGSBOptimizer
::LBFGSBOptimizer():
  m_Trace(false),
  m_OptimizerInitialized(false),
  m_CostFunctionConvergenceFactor(1e+7),
  m_ProjectedGradientTolerance(1e-5),
  m_MaximumNumberOfIterations(500),
  m_MaximumNumberOfEvaluations(500),
  m_MaximumNumberOfCorrections(5),
  m_CurrentIteration(0),
  m_InfinityNormOfProjectedGradient(0.0),
  m_VnlOptimizer(ITK_NULLPTR)
{
  m_LowerBound       = InternalBoundValueType(0);
  m_UpperBound       = InternalBoundValueType(0);
  m_BoundSelection   = InternalBoundSelectionType(0);
}

/**
 * Destructor
 */
LBFGSBOptimizer
::~LBFGSBOptimizer()
{
  delete m_VnlOptimizer;
}

/**
 * PrintSelf
 */
void
LBFGSBOptimizer
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

  os << indent << "LowerBound: " << m_LowerBound << std::endl;
  os << indent << "UpperBound: " << m_UpperBound << std::endl;
  os << indent << "BoundSelection: " << m_BoundSelection << std::endl;

  os << indent << "CostFunctionConvergenceFactor: "
     << m_CostFunctionConvergenceFactor << std::endl;

  os << indent << "ProjectedGradientTolerance: "
     << m_ProjectedGradientTolerance << std::endl;

  os << indent << "MaximumNumberOfIterations: "
     << m_MaximumNumberOfIterations << std::endl;

  os << indent << "MaximumNumberOfEvaluations: "
     << m_MaximumNumberOfEvaluations << std::endl;

  os << indent << "MaximumNumberOfCorrections: "
     << m_MaximumNumberOfCorrections << std::endl;

  os << indent << "CurrentIteration: "
     << m_CurrentIteration << std::endl;

  os << indent << "Value: "
     << this->GetValue() << std::endl;

  os << indent << "InfinityNormOfProjectedGradient: "
     << m_InfinityNormOfProjectedGradient << std::endl;

  if ( this->m_VnlOptimizer )
    {
    os << indent << "Vnl LBFGSB Failure Code: "
       << this->m_VnlOptimizer->get_failure_code() << std::endl;
    }
}

/**
 * Set the optimizer trace flag
 */
void
LBFGSBOptimizer
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
 * Set lower bound
 */
void
LBFGSBOptimizer
::SetLowerBound(
  const BoundValueType & value)
{
  this->m_LowerBound = value;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_lower_bound(m_LowerBound);
    }
  this->Modified();
}

/**
 * Set upper bound
 */
void
LBFGSBOptimizer
::SetUpperBound(
  const BoundValueType & value)
{
  this->m_UpperBound = value;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_upper_bound(m_UpperBound);
    }
  this->Modified();
}

/**
 * Return Current Value
 */
LBFGSBOptimizer::MeasureType
LBFGSBOptimizer
::GetValue() const
{
  return this->GetCachedValue();
}

/**
 * Set bound selection array
 */
void
LBFGSBOptimizer
::SetBoundSelection(
  const BoundSelectionType & value)
{
  m_BoundSelection = value;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_bound_selection(m_BoundSelection);
    }
  this->Modified();
}

/** Set/Get the CostFunctionConvergenceFactor. Algorithm terminates
 * when the reduction in cost function is less than factor * epsmcj
 * where epsmch is the machine precision.
 * Typical values for factor: 1e+12 for low accuracy;
 * 1e+7 for moderate accuracy and 1e+1 for extremely high accuracy.
 */
void
LBFGSBOptimizer
::SetCostFunctionConvergenceFactor(double value)
{
  if ( value < 0.0 )
    {
    itkExceptionMacro("Value " << value
                               << " is too small for SetCostFunctionConvergenceFactor()"
                               << "a typical range would be from 0.0 to 1e+12");
    }
  m_CostFunctionConvergenceFactor = value;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_cost_function_convergence_factor(
      m_CostFunctionConvergenceFactor);
    }
  this->Modified();
}

/** Set/Get the ProjectedGradientTolerance. Algorithm terminates
 * when the project gradient is below the tolerance. Default value
 * is 1e-5.
 */
void
LBFGSBOptimizer
::SetProjectedGradientTolerance(double value)
{
  m_ProjectedGradientTolerance = value;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_projected_gradient_tolerance(
      m_ProjectedGradientTolerance);
    }
  this->Modified();
}

/** Set/Get the MaximumNumberOfIterations. Default is 500 */
void
LBFGSBOptimizer
::SetMaximumNumberOfIterations(unsigned int value)
{
  m_MaximumNumberOfIterations = value;
  this->Modified();
}

/** Set/Get the MaximumNumberOfEvaluations. Default is 500 */
void
LBFGSBOptimizer
::SetMaximumNumberOfEvaluations(unsigned int value)
{
  m_MaximumNumberOfEvaluations = value;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_max_function_evals(m_MaximumNumberOfEvaluations);
    }
  this->Modified();
}

/** Set/Get the MaximumNumberOfCorrections. Default is 5 */
void
LBFGSBOptimizer
::SetMaximumNumberOfCorrections(unsigned int value)
{
  m_MaximumNumberOfCorrections = value;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_max_variable_metric_corrections(
      m_MaximumNumberOfCorrections);
    }
  this->Modified();
}

/**
 * Connect a Cost Function
 */
void
LBFGSBOptimizer::SetCostFunction(SingleValuedCostFunction *costFunction)
{
  m_CostFunction = costFunction;

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

  m_VnlOptimizer = new InternalOptimizerType(*adaptor, this);

  // set the optimizer parameters
  m_VnlOptimizer->set_lower_bound(m_LowerBound);
  m_VnlOptimizer->set_upper_bound(m_UpperBound);
  m_VnlOptimizer->set_bound_selection(m_BoundSelection);
  m_VnlOptimizer->set_cost_function_convergence_factor(
    m_CostFunctionConvergenceFactor);
  m_VnlOptimizer->set_projected_gradient_tolerance(
    m_ProjectedGradientTolerance);
  m_VnlOptimizer->set_max_function_evals(m_MaximumNumberOfEvaluations);
  m_VnlOptimizer->set_max_variable_metric_corrections(
    m_MaximumNumberOfCorrections);

  m_OptimizerInitialized = true;

  this->Modified();
}

/**
 * Start the optimization
 */
void
LBFGSBOptimizer
::StartOptimization(void)
{
  // Check if all the bounds parameters are the same size as the initial
  // parameters.
  unsigned int numberOfParameters = m_CostFunction->GetNumberOfParameters();

  if ( this->GetInitialPosition().Size() < numberOfParameters )
    {
    itkExceptionMacro(<< "InitialPosition array does not have sufficient number of elements");
    }

  if ( m_LowerBound.size() < numberOfParameters )
    {
    itkExceptionMacro(<< "LowerBound array does not have sufficient number of elements");
    }

  if ( m_UpperBound.size() < numberOfParameters )
    {
    itkExceptionMacro(<< "UppperBound array does not have sufficient number of elements");
    }

  if ( m_BoundSelection.size() < numberOfParameters )
    {
    itkExceptionMacro(<< "BoundSelection array does not have sufficient number of elements");
    }

  if ( this->GetMaximize() )
    {
    this->GetNonConstCostFunctionAdaptor()->NegateCostFunctionOn();
    }
  if(this->m_CostFunctionConvergenceFactor == 0.0 && this->m_ProjectedGradientTolerance == 0.0)
    {
    itkExceptionMacro("LBFGSB Optimizer cannot function if both CostFunctionConvergenceFactor"
                      " and ProjectedGradienctTolerance are zero.");
    }
  this->SetCurrentPosition( this->GetInitialPosition() );

  ParametersType parameters( this->GetInitialPosition() );

  this->InvokeEvent( StartEvent() );

  // vnl optimizers return the solution by reference
  // in the variable provided as initial position
  m_VnlOptimizer->minimize(parameters);

  if ( parameters.size() != this->GetInitialPosition().Size() )
    {
    // set current position to initial position and throw an exception
    this->SetCurrentPosition( this->GetInitialPosition() );
    itkExceptionMacro(<< "Error occurred in optimization");
    }

  this->SetCurrentPosition(parameters);

  this->InvokeEvent( EndEvent() );
}

/*-------------------------------------------------------------------------
 * helper class
 *-------------------------------------------------------------------------
 */

/** Create with a reference to the ITK object */
LBFGSBOptimizerHelper
::LBFGSBOptimizerHelper(vnl_cost_function & f,
                        LBFGSBOptimizer * const itkObj):
  vnl_lbfgsb(f),
  m_ItkObj(itkObj)
{}

/** Handle new iteration event */
bool
LBFGSBOptimizerHelper
::report_iter()
{
  Superclass::report_iter();

  m_ItkObj->m_InfinityNormOfProjectedGradient = this->get_inf_norm_projected_gradient();
  m_ItkObj->InvokeEvent( IterationEvent() );
  m_ItkObj->m_CurrentIteration = this->num_iterations_;

  // Return true to terminate the optimization loop.
  if ( this->num_iterations_ > m_ItkObj->m_MaximumNumberOfIterations )
    {
    return true;
    }
  else
    {
    return false;
    }
}

const std::string
LBFGSBOptimizer::GetStopConditionDescription() const
{
  std::ostringstream stopConditionDescription;
  if ( this->m_CurrentIteration > this->m_MaximumNumberOfIterations)
    {
    stopConditionDescription << "Too many iterations. Iterations = "
    << this->m_CurrentIteration << std::endl;
    }
  if ( m_VnlOptimizer )
    {
    stopConditionDescription << this->GetNameOfClass() << ": ";
    switch ( m_VnlOptimizer->get_failure_code() )
      {
      case vnl_nonlinear_minimizer::ERROR_FAILURE:
        stopConditionDescription << "Failure";
        break;
      case vnl_nonlinear_minimizer::ERROR_DODGY_INPUT:
        stopConditionDescription << "Dodgy input";
        break;
      case vnl_nonlinear_minimizer::CONVERGED_FTOL:
        stopConditionDescription << "Function tolerance reached after "
                                   << m_CurrentIteration
                                   << " iterations. "
                                   << "The relative reduction of the cost function <= "
                                   << m_CostFunctionConvergenceFactor * v3p_netlib_dpmeps_()
                                   << " = CostFunctionConvergenceFactor ("
                                   << m_CostFunctionConvergenceFactor
                                   << ") * machine precision ("
                                   << v3p_netlib_dpmeps_()
                                   << ").";

        break;
      case vnl_nonlinear_minimizer::CONVERGED_XTOL:
        stopConditionDescription << "Solution tolerance reached";
        break;
      case vnl_nonlinear_minimizer::CONVERGED_XFTOL:
        stopConditionDescription << "Solution and Function tolerance both reached";
        break;
      case vnl_nonlinear_minimizer::CONVERGED_GTOL:
        stopConditionDescription << "Gradient tolerance reached. "
                                   << "Projected gradient tolerance is "
                                   << m_ProjectedGradientTolerance;
        break;
      case vnl_nonlinear_minimizer::FAILED_TOO_MANY_ITERATIONS:
        stopConditionDescription << "Too many iterations. Iterations = "
                                   << m_MaximumNumberOfEvaluations;
        break;
      case vnl_nonlinear_minimizer::FAILED_FTOL_TOO_SMALL:
        stopConditionDescription << "Function tolerance too small";
        break;
      case vnl_nonlinear_minimizer::FAILED_XTOL_TOO_SMALL:
        stopConditionDescription << "Solution tolerance too small";
        break;
      case vnl_nonlinear_minimizer::FAILED_GTOL_TOO_SMALL:
        stopConditionDescription << "Gradient tolerance too small";
        break;
      case vnl_nonlinear_minimizer::FAILED_USER_REQUEST:
        break;
      }
    }
  return stopConditionDescription.str();
}
} // end namespace itk
