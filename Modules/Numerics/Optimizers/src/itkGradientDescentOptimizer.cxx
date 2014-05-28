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
#ifndef _itkGradientDescentOptimizer_hxx
#define _itkGradientDescentOptimizer_hxx

#include "itkGradientDescentOptimizer.h"

namespace itk
{
/**
 * Constructor
 */
GradientDescentOptimizer
::GradientDescentOptimizer() :
  m_Maximize(false),
  m_LearningRate(1.0),
  m_Stop(false),
  m_Value(0.0),
  m_StopCondition(MaximumNumberOfIterations),
  m_NumberOfIterations(100),
  m_CurrentIteration(0)
{
  itkDebugMacro("Constructor");

  m_StopConditionDescription << this->GetNameOfClass() << ": ";
}

const std::string
GradientDescentOptimizer
::GetStopConditionDescription() const
{
  return m_StopConditionDescription.str();
}

void
GradientDescentOptimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "LearningRate: "
     << m_LearningRate << std::endl;
  os << indent << "NunberOfIterations: "
     << m_NumberOfIterations << std::endl;
  os << indent << "Maximize: "
     << m_Maximize << std::endl;
  os << indent << "CurrentIteration: "
     << m_CurrentIteration;
  os << indent << "Value: "
     << m_Value;
  if ( m_CostFunction )
    {
    os << indent << "CostFunction: "
       << m_CostFunction;
    }
  os << indent << "StopCondition: "
     << m_StopCondition;
  os << std::endl;
  os << indent << "Gradient: "
     << m_Gradient;
  os << std::endl;
}

/**
 * Start the optimization
 */
void
GradientDescentOptimizer
::StartOptimization(void)
{
  itkDebugMacro("StartOptimization");

  m_CurrentIteration   = 0;

  this->SetCurrentPosition( this->GetInitialPosition() );
  this->ResumeOptimization();
}

/**
 * Resume the optimization
 */
void
GradientDescentOptimizer
::ResumeOptimization(void)
{
  itkDebugMacro("ResumeOptimization");

  m_Stop = false;

  m_StopConditionDescription.str("");
  m_StopConditionDescription << this->GetNameOfClass() << ": ";
  InvokeEvent( StartEvent() );
  while ( !m_Stop )
    {
    try
      {
      m_CostFunction->GetValueAndDerivative(
        this->GetCurrentPosition(), m_Value, m_Gradient);
      }
    catch ( ExceptionObject & err )
      {
      // An exception has occurred.
      // Terminate immediately.
      m_StopCondition = MetricError;
      m_StopConditionDescription << "Metric error";
      StopOptimization();

      // Pass exception to caller
      throw err;
      }

    if ( m_Stop )
      {
      m_StopConditionDescription << "StopOptimization() called";
      break;
      }

    AdvanceOneStep();

    m_CurrentIteration++;

    if ( m_CurrentIteration >= m_NumberOfIterations )
      {
      m_StopConditionDescription << "Maximum number of iterations ("
                                 << m_NumberOfIterations
                                 << ") exceeded.";
      m_StopCondition = MaximumNumberOfIterations;
      StopOptimization();
      break;
      }
    }
}

/**
 * Stop optimization
 */
void
GradientDescentOptimizer
::StopOptimization(void)
{
  itkDebugMacro("StopOptimization");

  m_Stop = true;
  InvokeEvent( EndEvent() );
}

/**
 * Advance one Step following the gradient direction
 */
void
GradientDescentOptimizer
::AdvanceOneStep(void)
{
  itkDebugMacro("AdvanceOneStep");

  double direction;
  if ( this->m_Maximize )
    {
    direction = 1.0;
    }
  else
    {
    direction = -1.0;
    }

  const unsigned int spaceDimension =  m_CostFunction->GetNumberOfParameters();

  const ParametersType & currentPosition = this->GetCurrentPosition();

  const ScalesType & scales = this->GetScales();

  // Make sure the scales have been set properly
  if ( scales.size() != spaceDimension )
    {
    itkExceptionMacro(<< "The size of Scales is "
                      << scales.size()
                      << ", but the NumberOfParameters for the CostFunction is "
                      << spaceDimension
                      << ".");
    }

  DerivativeType transformedGradient(spaceDimension);

  for ( unsigned int j = 0; j < spaceDimension; j++ )
    {
    transformedGradient[j] = m_Gradient[j] / scales[j];
    }

  ParametersType newPosition(spaceDimension);
  for ( unsigned int j = 0; j < spaceDimension; j++ )
    {
    newPosition[j] = currentPosition[j]
                     + direction * m_LearningRate * transformedGradient[j];
    }

  this->SetCurrentPosition(newPosition);

  this->InvokeEvent( IterationEvent() );
}
} // end namespace itk

#endif
