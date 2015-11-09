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
#include "itkLBFGSOptimizerv4.h"
#include "itkMacro.h"
#include "itkMath.h"

namespace itk
{
LBFGSOptimizerv4
  ::LBFGSOptimizerv4():
  m_Verbose(false),
  m_LineSearchAccuracy(0.9),
  m_DefaultStepLength(1.0)
{
}

LBFGSOptimizerv4
::~LBFGSOptimizerv4()
{
}

void
LBFGSOptimizerv4
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "LineSearchAccuracy: "
     << m_LineSearchAccuracy << std::endl;
  os << indent << "DefaultStepLength: "
     << m_DefaultStepLength << std::endl;

  if ( this->m_VnlOptimizer )
    {
    os << indent << "Vnl LBFGS Failure Code: "
    << this->m_VnlOptimizer->get_failure_code() << std::endl;
    }
}

void
LBFGSOptimizerv4
::VerboseOn()
{
  if ( m_Verbose == true )
    {
    return;
    }

  m_Verbose = true;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_verbose(true);
    }

  this->Modified();
}

void
LBFGSOptimizerv4
::VerboseOff()
{
  if ( m_Verbose == false )
    {
    return;
    }

  m_Verbose = false;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_verbose(false);
    }

  this->Modified();
}

void
LBFGSOptimizerv4
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

void
LBFGSOptimizerv4
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

void
LBFGSOptimizerv4
::SetMetric(MetricType *metric)
{
  Superclass::SetMetric( metric );

  // set the optimizer parameters
  m_VnlOptimizer->set_trace( m_Trace );
  m_VnlOptimizer->set_verbose( m_Verbose );
  m_VnlOptimizer->set_max_function_evals( static_cast< int >( m_MaximumNumberOfFunctionEvaluations ) );
  m_VnlOptimizer->set_g_tolerance( m_GradientConvergenceTolerance );
  m_VnlOptimizer->line_search_accuracy = m_LineSearchAccuracy;
  m_VnlOptimizer->default_step_length  = m_DefaultStepLength;
  // set for debugging
  //m_VnlOptimizer->set_check_derivatives( true );

  m_OptimizerInitialized = true;

  this->Modified();
}

void
LBFGSOptimizerv4
::StartOptimization(bool /* doOnlyInitialization */)
{
  // Perform some verification, check scales,
  // pass settings to cost-function adaptor.
  Superclass::StartOptimization();

  // Note that it's tempting to use a reference to the parameters stored
  // in the metric for efficiency.
  ParametersType parameters = this->m_Metric->GetParameters();

  if( parameters.GetSize() == 0 )
    {
    itkExceptionMacro(<<"Optimizer parameters are not initialized.");
    }

  // Scale the initial parameters up if scales are defined.
  // This compensates for later scaling them down in "compute" function of
  // the cost function adaptor and at the end of this function.
  InternalParametersType vnlCompatibleParameters(parameters.GetSize());
  const ScalesType & scales = this->GetScales();
  for ( SizeValueType i = 0; i < parameters.GetSize(); ++i )
    {
    if( this->GetScalesAreIdentity() )
      {
      vnlCompatibleParameters[i] = parameters[i];
      }
    else
      {
      vnlCompatibleParameters[i] = parameters[i] * scales[i];
      }
    }

  // vnl optimizers return the solution by reference
  // in the variable provided as initial position.
  // Also note that v4 registration always minimizes because v4 metrics return the negate value
  // if the cost function should be maximized.
  m_VnlOptimizer->minimize( vnlCompatibleParameters );

  // Check if the output parameters are not null.
  if ( vnlCompatibleParameters.size() != parameters.GetSize() )
    {
    itkExceptionMacro(<< "Error occurred in optimization. Optimized parameters and initial parameters are not the same size: "
                      << vnlCompatibleParameters.size() << " vs. " << parameters.GetSize() );
    }

  // we scale the parameters down if scales are defined
  for ( SizeValueType i = 0; i < vnlCompatibleParameters.size(); ++i )
    {
    if( this->GetScalesAreIdentity() )
     {
     parameters[i] = vnlCompatibleParameters[i];
     }
    else
     {
     parameters[i] = vnlCompatibleParameters[i] / scales[i];
     }
    }

  this->m_Metric->SetParameters( parameters );
}
} // end namespace itk
