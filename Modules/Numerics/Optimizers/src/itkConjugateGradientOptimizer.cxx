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
#ifndef _itkConjugateGradientOptimizer_hxx
#define _itkConjugateGradientOptimizer_hxx

#include "itkConjugateGradientOptimizer.h"

namespace itk
{
/**
 * Constructor
 */
ConjugateGradientOptimizer
::ConjugateGradientOptimizer()
{
  m_OptimizerInitialized    = false;
  m_VnlOptimizer            = ITK_NULLPTR;
}

/**
 * Destructor
 */
ConjugateGradientOptimizer
::~ConjugateGradientOptimizer()
{
  delete m_VnlOptimizer;
}

/**
 * Get the Optimizer
 */
vnl_conjugate_gradient *
ConjugateGradientOptimizer
::GetOptimizer(void)
{
  return m_VnlOptimizer;
}

/**
 * Connect a Cost Function
 */
void
ConjugateGradientOptimizer
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

  m_VnlOptimizer = new vnl_conjugate_gradient(*adaptor);
  m_OptimizerInitialized = true;
}

/** Return Current Value */
ConjugateGradientOptimizer::MeasureType
ConjugateGradientOptimizer
::GetValue() const
{
  ParametersType parameters = this->GetCurrentPosition();

  if ( m_ScalesInitialized )
    {
    const ScalesType & scales = this->GetScales();
    for ( unsigned int i = 0; i < parameters.size(); i++ )
      {
      parameters[i] *= scales[i];
      }
    }
  return this->GetNonConstCostFunctionAdaptor()->f(parameters);
}

/**
 * Start the optimization
 */
void
ConjugateGradientOptimizer
::StartOptimization(void)
{
  this->InvokeEvent( StartEvent() );

  if ( this->GetMaximize() )
    {
    this->GetNonConstCostFunctionAdaptor()->NegateCostFunctionOn();
    }

  ParametersType initialPosition = this->GetInitialPosition();

  ParametersType parameters(initialPosition);

  // If the user provides the scales then we set otherwise we don't
  // for computation speed.
  // We also scale the initial parameters up if scales are defined.
  // This compensates for later scaling them down in the cost function adaptor
  // and at the end of this function.
  if ( m_ScalesInitialized )
    {
    const ScalesType & scales = this->GetScales();
    this->GetNonConstCostFunctionAdaptor()->SetScales(scales);
    for ( unsigned int i = 0; i < parameters.size(); i++ )
      {
      parameters[i] *= scales[i];
      }
    }

  // vnl optimizers return the solution by reference
  // in the variable provided as initial position
  m_VnlOptimizer->minimize(parameters);

  // we scale the parameters down if scales are defined
  if ( m_ScalesInitialized )
    {
    const ScalesType & invScales = this->GetInverseScales();
    for ( unsigned int i = 0; i < parameters.size(); ++i )
      {
      parameters[i] *= invScales[i];
      }
    }

  this->SetCurrentPosition(parameters);

  this->InvokeEvent( EndEvent() );
}

/**
 * Get the maximum number of evaluations of the function.
 * In vnl this is used instead of a maximum number of iterations
 * given that an iteration could imply several evaluations.
 */
SizeValueType
ConjugateGradientOptimizer
::GetNumberOfIterations(void) const
{
  return m_VnlOptimizer->get_max_function_evals();
}

/**
 * Get the number of iterations in the last optimization.
 */
SizeValueType
ConjugateGradientOptimizer
::GetCurrentIteration(void) const
{
  return m_VnlOptimizer->get_num_iterations();
}
} // end namespace itk

#endif
