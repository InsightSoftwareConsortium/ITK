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
#include "itkLBFGSBOptimizerv4.h"

namespace itk
{
/** \class LBFGSBOptimizerHelperv4
 * \brief Wrapper helper around vnl_lbfgsb.
 *
 * This class is used to translate iteration events, etc, from
 * vnl_lbfgsb into iteration events in ITK.
 */
class ITKOptimizersv4_EXPORT LBFGSBOptimizerHelperv4:
public LBFGSOptimizerBaseHelperv4<vnl_lbfgsb>
{
  public:
  typedef LBFGSBOptimizerHelperv4                  Self;
  typedef LBFGSOptimizerBaseHelperv4<vnl_lbfgsb>   Superclass;

  /** Create with a reference to the ITK object */
  LBFGSBOptimizerHelperv4(vnl_cost_function & f,
                          LBFGSBOptimizerv4 * const itkObj);

  protected:
  /** Handle new iteration event */
  virtual bool report_iter() ITK_OVERRIDE;
};

/** Create with a reference to the ITK object */
LBFGSBOptimizerHelperv4
::LBFGSBOptimizerHelperv4(vnl_cost_function & f, LBFGSBOptimizerv4 * const itkObj):
  Superclass::LBFGSOptimizerBaseHelperv4(f, itkObj)
{
}

/** Handle new iteration event */
bool
LBFGSBOptimizerHelperv4
::report_iter()
{
  const bool ret = Superclass::report_iter();
  m_ItkObj->m_InfinityNormOfProjectedGradient = this->get_inf_norm_projected_gradient();
  return ret;
}
//-------------------------------------------------------------------------

LBFGSBOptimizerv4
  ::LBFGSBOptimizerv4():
  m_MaximumNumberOfCorrections(5),
  m_InitialPosition(0),
  m_LowerBound(0),
  m_UpperBound(0),
  m_BoundSelection(0)
{
}

LBFGSBOptimizerv4
::~LBFGSBOptimizerv4()
{
}

void
LBFGSBOptimizerv4
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "InitialPosition: " << m_InitialPosition << std::endl;
  os << indent << "CurrentPosition: " << this->GetCurrentPosition() << std::endl;

  os << indent << "LowerBound: " << m_LowerBound << std::endl;
  os << indent << "UpperBound: " << m_UpperBound << std::endl;
  os << indent << "BoundSelection: " << m_BoundSelection << std::endl;

  os << indent << "CostFunctionConvergenceFactor: "
  << m_CostFunctionConvergenceFactor << std::endl;

  os << indent << "MaximumNumberOfEvaluations: "
  << m_MaximumNumberOfFunctionEvaluations << std::endl;

  os << indent << "MaximumNumberOfCorrections: "
  << m_MaximumNumberOfCorrections << std::endl;

  os << indent << "Value: "
  << this->GetValue() << std::endl;

  os << indent << "InfinityNormOfProjectedGradient: "
  << this->m_InfinityNormOfProjectedGradient << std::endl;

  if ( this->m_VnlOptimizer )
    {
    os << indent << "Vnl LBFGSB Failure Code: "
    << this->m_VnlOptimizer->get_failure_code() << std::endl;
    }
}

void
LBFGSBOptimizerv4
::SetScales(const ScalesType &)
{
  itkWarningMacro( << "LBFGSB optimizer does not support scaling. All scales are set to one." )
  m_Scales.SetSize( this->m_Metric->GetNumberOfLocalParameters() );
  m_Scales.Fill( NumericTraits<ScalesType::ValueType>::OneValue() );
  this->m_ScalesAreIdentity = true;
}

void
LBFGSBOptimizerv4
::SetInitialPosition(const ParametersType & param)
{
  m_InitialPosition = param;
  this->Modified();
}

void
LBFGSBOptimizerv4
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

void
LBFGSBOptimizerv4
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

void
LBFGSBOptimizerv4
::SetBoundSelection(
  const BoundSelectionType & value)
{
  m_BoundSelection = value;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_bound_selection( m_BoundSelection );
    }
  this->Modified();
}

void
LBFGSBOptimizerv4
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
    m_VnlOptimizer->set_cost_function_convergence_factor( m_CostFunctionConvergenceFactor );
    }
  this->Modified();
}

void
LBFGSBOptimizerv4
::SetMaximumNumberOfCorrections(unsigned int value)
{
  m_MaximumNumberOfCorrections = value;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_max_variable_metric_corrections( m_MaximumNumberOfCorrections );
    }
  this->Modified();
}

void
LBFGSBOptimizerv4
::SetMetric(MetricType *metric)
{
  Superclass::SetMetric( metric );

  CostFunctionAdaptorType *adaptor = this->GetCostFunctionAdaptor();

  m_VnlOptimizer.Reset();
  m_VnlOptimizer.TakeOwnership( new InternalOptimizerType( *adaptor, this ) );

  // set the optimizer parameters
  m_VnlOptimizer->set_trace( m_Trace );
  m_VnlOptimizer->set_lower_bound( m_LowerBound );
  m_VnlOptimizer->set_upper_bound( m_UpperBound );
  m_VnlOptimizer->set_bound_selection( m_BoundSelection );
  m_VnlOptimizer->set_cost_function_convergence_factor( m_CostFunctionConvergenceFactor );
  m_VnlOptimizer->set_projected_gradient_tolerance( m_GradientConvergenceTolerance );
  m_VnlOptimizer->set_max_function_evals( static_cast< int >( m_MaximumNumberOfFunctionEvaluations ) );
  m_VnlOptimizer->set_max_variable_metric_corrections( m_MaximumNumberOfCorrections );

  m_OptimizerInitialized = true;

  this->Modified();
}

void
LBFGSBOptimizerv4
::StartOptimization(bool /*doOnlyInitialization*/ )
{
  // Perform some verification, check scales,
  // pass settings to cost-function adaptor.
  Superclass::StartOptimization();

  // Check if all the bounds parameters are the same size as the initial
  // parameters.
  unsigned int numberOfParameters = m_Metric->GetNumberOfParameters();

  if ( this->GetInitialPosition().Size() < numberOfParameters )
    {
    this->SetInitialPosition( m_Metric->GetParameters() );
    }

  if ( m_LowerBound.size() < numberOfParameters && !m_BoundSelection.is_zero() )
    {
    itkExceptionMacro(<< "LowerBound array does not have sufficient number of elements");
    }

  if ( m_UpperBound.size() < numberOfParameters && !m_BoundSelection.is_zero() )
    {
    itkExceptionMacro(<< "UppperBound array does not have sufficient number of elements");
    }

  if ( m_BoundSelection.size() < numberOfParameters )
    {
    itkExceptionMacro(<< "BoundSelection array does not have sufficient number of elements");
    }

  if(this->m_CostFunctionConvergenceFactor == 0.0 && this->m_GradientConvergenceTolerance == 0.0)
    {
    itkExceptionMacro("LBFGSB Optimizer cannot function if both CostFunctionConvergenceFactor"
                      " and ProjectedGradienctTolerance are zero.");
    }

  ParametersType parameters( this->GetInitialPosition() );

  this->InvokeEvent( StartEvent() );

  // vnl optimizers return the solution by reference
  // in the variable provided as initial position
  m_VnlOptimizer->minimize(parameters);

  if ( parameters.GetSize() != this->GetInitialPosition().Size() )
    {
    // set current position to initial position and throw an exception
    this->m_Metric->SetParameters( this->GetInitialPosition() );
    itkExceptionMacro(<< "Error occurred in optimization");
    }

  this->m_Metric->SetParameters( parameters );

  this->InvokeEvent( EndEvent() );
}

} // end namespace itk
