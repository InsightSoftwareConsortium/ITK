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
#ifndef _itkAmoebaOptimizer_hxx
#define _itkAmoebaOptimizer_hxx

#include "itkAmoebaOptimizer.h"

namespace itk
{
/**
 * Constructor
 */
AmoebaOptimizer
::AmoebaOptimizer():
  m_InitialSimplexDelta(1)    // initial size
{
  m_OptimizerInitialized           = false;
  m_VnlOptimizer                   = 0;
  m_MaximumNumberOfIterations      = 500;
  m_ParametersConvergenceTolerance = 1e-8;
  m_FunctionConvergenceTolerance   = 1e-4;
  m_AutomaticInitialSimplex        = true;
  m_InitialSimplexDelta.Fill(NumericTraits< ParametersType::ValueType >::One);
}

/**
 * Destructor
 */
AmoebaOptimizer
::~AmoebaOptimizer()
{
  delete m_VnlOptimizer;
}

const std::string
AmoebaOptimizer
::GetStopConditionDescription() const
{
  return m_StopConditionDescription.str();
}

/**
 * PrintSelf
 */
void
AmoebaOptimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MaximumNumberOfIterations: "
     << m_MaximumNumberOfIterations << std::endl;
  os << indent << "ParametersConvergenceTolerance: "
     << m_ParametersConvergenceTolerance << std::endl;
  os << indent << "FunctionConvergenceTolerance: "
     << m_FunctionConvergenceTolerance << std::endl;
  os << indent << "AutomaticInitialSimplex: "
     << ( m_AutomaticInitialSimplex ? "On" : "Off" ) << std::endl;
  os << indent << "InitialSimplexDelta: "
     << m_InitialSimplexDelta << std::endl;
}

/** Return Current Value */
AmoebaOptimizer::MeasureType
AmoebaOptimizer
::GetValue() const
{
  ParametersType parameters = this->GetCurrentPosition();

  if ( m_ScalesInitialized )
    {
    const ScalesType scales = this->GetScales();
    for ( unsigned int i = 0; i < parameters.size(); i++ )
      {
      parameters[i] *= scales[i];
      }
    }
  return this->GetNonConstCostFunctionAdaptor()->f(parameters);
}

/**
 * Set the maximum number of iterations
 */
void
AmoebaOptimizer
::SetMaximumNumberOfIterations(unsigned int n)
{
  if ( n == m_MaximumNumberOfIterations )
    {
    return;
    }

  m_MaximumNumberOfIterations = n;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_max_iterations( static_cast< int >( n ) );
    }

  this->Modified();
}

/**
 * Set the parameters convergence tolerance
 */
void
AmoebaOptimizer
::SetParametersConvergenceTolerance(double tol)
{
  if ( tol == m_ParametersConvergenceTolerance )
    {
    return;
    }

  m_ParametersConvergenceTolerance = tol;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_x_tolerance(tol);
    }

  this->Modified();
}

/**
 * Set the function convergence tolerance
 */
void
AmoebaOptimizer
::SetFunctionConvergenceTolerance(double tol)
{
  if ( tol == m_FunctionConvergenceTolerance )
    {
    return;
    }

  m_FunctionConvergenceTolerance = tol;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_f_tolerance(tol);
    }

  this->Modified();
}

/**
 * Connect a Cost Function
 */
void
AmoebaOptimizer
::SetCostFunction(SingleValuedCostFunction *costFunction)
{
  const unsigned int numberOfParameters =
    costFunction->GetNumberOfParameters();

  CostFunctionAdaptorType *adaptor =
    new CostFunctionAdaptorType(numberOfParameters);

  SingleValuedNonLinearOptimizer::SetCostFunction(costFunction);
  adaptor->SetCostFunction(costFunction);

  if ( m_OptimizerInitialized )
    {
    delete m_VnlOptimizer;
    }

  this->SetCostFunctionAdaptor(adaptor);

  m_VnlOptimizer = new vnl_amoeba(*adaptor);

  // set up optimizer parameters
  m_VnlOptimizer->set_max_iterations( static_cast< int >( m_MaximumNumberOfIterations ) );
  m_VnlOptimizer->set_x_tolerance(m_ParametersConvergenceTolerance);
  m_VnlOptimizer->set_f_tolerance(m_FunctionConvergenceTolerance);

  m_OptimizerInitialized = true;
}

/**
 * Start the optimization
 */
void
AmoebaOptimizer
::StartOptimization(void)
{
  this->InvokeEvent( StartEvent() );
  m_StopConditionDescription.str("");
  m_StopConditionDescription << this->GetNameOfClass() << ": Running";

  if ( this->GetMaximize() )
    {
    this->GetNonConstCostFunctionAdaptor()->NegateCostFunctionOn();
    }

  ParametersType initialPosition = this->GetInitialPosition();
  this->SetCurrentPosition(initialPosition);

  ParametersType parameters(initialPosition);

  // If the user provides the scales then we set otherwise we don't
  // for computation speed.
  // We also scale the initial parameters up if scales are defined.
  // This compensates for later scaling them down in the cost function adaptor
  // and at the end of this function.
  if ( m_ScalesInitialized )
    {
    ScalesType scales = this->GetScales();
    this->GetNonConstCostFunctionAdaptor()->SetScales(scales);
    for ( unsigned int i = 0; i < parameters.size(); i++ )
      {
      parameters[i] *= scales[i];
      }
    }

  // vnl optimizers return the solution by reference
  // in the variable provided as initial position
  if ( m_AutomaticInitialSimplex )
    {
    m_VnlOptimizer->minimize(parameters);
    }
  else
    {
    if ( m_InitialSimplexDelta.GetSize() != parameters.GetSize() )
      {
      itkExceptionMacro( << "Size of InitialSimplexDelta ("
                         << m_InitialSimplexDelta.GetSize()
                         << ") does not match number of parameters ("
                         << parameters.GetSize() << ")" );
      }
    InternalParametersType delta(m_InitialSimplexDelta);
    m_VnlOptimizer->minimize(parameters, delta);
    }

  // we scale the parameters down if scales are defined
  if ( m_ScalesInitialized )
    {
    ScalesType scales = this->GetScales();
    for ( unsigned int i = 0; i < parameters.size(); i++ )
      {
      parameters[i] /= scales[i];
      }
    }

  this->SetCurrentPosition(parameters);

  m_StopConditionDescription.str("");
  m_StopConditionDescription << this->GetNameOfClass() << ": ";
  if ( static_cast< unsigned int >( m_VnlOptimizer->get_num_evaluations() )
       < m_MaximumNumberOfIterations )
    {
    m_StopConditionDescription << "Both parameters convergence tolerance ("
                               << m_ParametersConvergenceTolerance
                               << ") and function convergence tolerance ("
                               << m_FunctionConvergenceTolerance
                               << ") have been met in "
                               << m_VnlOptimizer->get_num_evaluations()
                               << " iterations.";
    }
  else
    {
    m_StopConditionDescription << "Maximum number of iterations exceeded."
                               << " Number of iterations is "
                               << m_MaximumNumberOfIterations;
    }
  this->InvokeEvent( EndEvent() );
}

/**
 * Get the Optimizer
 */
vnl_amoeba *
AmoebaOptimizer
::GetOptimizer()
{
  return m_VnlOptimizer;
}
} // end namespace itk

#endif
