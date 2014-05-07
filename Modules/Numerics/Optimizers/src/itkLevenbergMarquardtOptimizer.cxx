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
#ifndef _itkLevenbergMarquardtOptimizer_hxx
#define _itkLevenbergMarquardtOptimizer_hxx

#include "itkLevenbergMarquardtOptimizer.h"

namespace itk
{
/**
 * Constructor
 */
LevenbergMarquardtOptimizer
::LevenbergMarquardtOptimizer()
{
  m_OptimizerInitialized    = false;
  m_VnlOptimizer            = ITK_NULLPTR;
  m_NumberOfIterations      = 2000;
  m_ValueTolerance          = 1e-8;
  m_GradientTolerance       = 1e-5;
  m_EpsilonFunction         = 1e-11;
}

/**
 * Destructor
 */
LevenbergMarquardtOptimizer
::~LevenbergMarquardtOptimizer()
{
  delete m_VnlOptimizer;
}

/**
 * Connect a Cost Function
 */
void
LevenbergMarquardtOptimizer
::SetCostFunction(MultipleValuedCostFunction *costFunction)
{
  const unsigned int numberOfParameters = costFunction->GetNumberOfParameters();
  const unsigned int numberOfValues = costFunction->GetNumberOfValues();

  CostFunctionAdaptorType *adaptor =
    new CostFunctionAdaptorType(numberOfParameters, numberOfValues);

  adaptor->SetCostFunction(costFunction);

  if ( m_OptimizerInitialized )
    {
    delete m_VnlOptimizer;
    }

  this->SetCostFunctionAdaptor(adaptor);

  m_VnlOptimizer = new vnl_levenberg_marquardt(*adaptor);

  this->SetNumberOfIterations(m_NumberOfIterations);
  this->SetValueTolerance(m_ValueTolerance);
  this->SetGradientTolerance(m_GradientTolerance);
  this->SetEpsilonFunction(m_EpsilonFunction);

  m_OptimizerInitialized = true;
}

/** Return Current Value */
LevenbergMarquardtOptimizer::MeasureType
LevenbergMarquardtOptimizer
::GetValue() const
{
  MeasureType measures;

  const CostFunctionAdaptorType *adaptor =
    this->GetCostFunctionAdaptor();

  if ( adaptor )
    {
    const MultipleValuedCostFunction *costFunction =
      adaptor->GetCostFunction();
    if ( costFunction )
      {
      const unsigned int numberOfValues =
        costFunction->GetNumberOfValues();
      measures.SetSize(numberOfValues);
      ParametersType parameters = this->GetCurrentPosition();
      if ( m_ScalesInitialized )
        {
        const ScalesType & scales = this->GetScales();
        for ( unsigned int i = 0; i < parameters.size(); i++ )
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
LevenbergMarquardtOptimizer
::StartOptimization(void)
{
  this->InvokeEvent( StartEvent() );

  ParametersType initialPosition = GetInitialPosition();
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

  if ( this->GetCostFunctionAdaptor()->GetUseGradient() )
    {
    m_VnlOptimizer->minimize_using_gradient(parameters);
    }
  else
    {
    m_VnlOptimizer->minimize_without_gradient(parameters);
    }

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

/** Set the maximum number of iterations */
void
LevenbergMarquardtOptimizer
::SetNumberOfIterations(unsigned int iterations)
{
  if ( m_VnlOptimizer )
    {
    m_VnlOptimizer->set_max_function_evals(iterations);
    }

  m_NumberOfIterations = iterations;
}

/** Set the maximum number of iterations */
void
LevenbergMarquardtOptimizer
::SetValueTolerance(double tol)
{
  if ( m_VnlOptimizer )
    {
    m_VnlOptimizer->set_x_tolerance(tol);
    }

  m_ValueTolerance = tol;
}

/** Set Gradient Tolerance */
void
LevenbergMarquardtOptimizer
::SetGradientTolerance(double tol)
{
  if ( m_VnlOptimizer )
    {
    m_VnlOptimizer->set_g_tolerance(tol);
    }

  m_GradientTolerance = tol;
}

/** Set Epsilon function */
void
LevenbergMarquardtOptimizer
::SetEpsilonFunction(double epsilon)
{
  if ( m_VnlOptimizer )
    {
    m_VnlOptimizer->set_epsilon_function(epsilon);
    }

  m_EpsilonFunction = epsilon;
}

/** Get the Optimizer */
vnl_levenberg_marquardt *
LevenbergMarquardtOptimizer
::GetOptimizer() const
{
  return m_VnlOptimizer;
}

const std::string
LevenbergMarquardtOptimizer
::GetStopConditionDescription() const
{
  std::ostringstream reason, outcome;

  outcome.str("");
  if ( GetOptimizer() )
    {
    GetOptimizer()->diagnose_outcome(outcome);
    }
  reason << this->GetNameOfClass() << ": " << ( ( outcome.str().size() > 0 ) ? outcome.str().c_str() : "" );
  return reason.str();
}
} // end namespace itk

#endif
