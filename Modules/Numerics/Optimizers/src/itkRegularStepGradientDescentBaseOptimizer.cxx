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
#ifndef _itkRegularStepGradientDescentBaseOptimizer_hxx
#define _itkRegularStepGradientDescentBaseOptimizer_hxx

#include "itkRegularStepGradientDescentBaseOptimizer.h"

namespace itk
{
/**
 * Constructor
 */
RegularStepGradientDescentBaseOptimizer
::RegularStepGradientDescentBaseOptimizer():
  m_Stop(false)
{
  itkDebugMacro("Constructor");

  m_MaximumStepLength = 1.0;
  m_MinimumStepLength = 1e-3;
  m_GradientMagnitudeTolerance = 1e-4;
  m_NumberOfIterations = 100;
  m_CurrentIteration   =   0;
  m_Value = 0;
  m_Maximize = false;
  m_CostFunction = ITK_NULLPTR;
  m_CurrentStepLength   =   0;
  m_StopCondition = Unknown;
  m_Gradient.Fill(0.0f);
  m_PreviousGradient.Fill(0.0f);
  m_RelaxationFactor = 0.5;
  m_StopConditionDescription.str("");
}

/**
 * Start the optimization
 */
void
RegularStepGradientDescentBaseOptimizer
::StartOptimization(void)
{
  itkDebugMacro("StartOptimization");

  m_CurrentStepLength         = m_MaximumStepLength;
  m_CurrentIteration          = 0;

  m_StopCondition = Unknown;
  m_StopConditionDescription.str("");
  m_StopConditionDescription << this->GetNameOfClass() << ": ";

  // validity check for the value of GradientMagnitudeTolerance
  if ( m_GradientMagnitudeTolerance < 0.0 )
    {
    itkExceptionMacro(<< "Gradient magnitude tolerance must be"
                         "greater or equal 0.0. Current value is " << m_GradientMagnitudeTolerance);
    }

  const unsigned int spaceDimension = m_CostFunction->GetNumberOfParameters();

  m_Gradient = DerivativeType(spaceDimension);
  m_PreviousGradient = DerivativeType(spaceDimension);
  m_Gradient.Fill(0.0f);
  m_PreviousGradient.Fill(0.0f);

  this->SetCurrentPosition( GetInitialPosition() );
  this->ResumeOptimization();
}

/**
 * Resume the optimization
 */
void
RegularStepGradientDescentBaseOptimizer
::ResumeOptimization(void)
{
  itkDebugMacro("ResumeOptimization");

  m_Stop = false;

  this->InvokeEvent( StartEvent() );

  while ( !m_Stop )
    {
    if ( m_CurrentIteration >= m_NumberOfIterations )
      {
      m_StopCondition = MaximumNumberOfIterations;
      m_StopConditionDescription << "Maximum number of iterations ("
                                 << m_NumberOfIterations
                                 << ") exceeded.";
      this->StopOptimization();
      break;
      }

    m_PreviousGradient = m_Gradient;

    try
      {
      m_CostFunction->GetValueAndDerivative(
        this->GetCurrentPosition(), m_Value, m_Gradient);
      }
    catch ( ExceptionObject & excp )
      {
      m_StopCondition = CostFunctionError;
      m_StopConditionDescription << "Cost function error after "
                                 << m_CurrentIteration
                                 << " iterations. "
                                 << excp.GetDescription();
      this->StopOptimization();
      throw excp;
      }

    if ( m_Stop )
      {
      break;
      }

    this->AdvanceOneStep();

    m_CurrentIteration++;
    }
}

/**
 * Stop optimization
 */
void
RegularStepGradientDescentBaseOptimizer
::StopOptimization(void)
{
  itkDebugMacro("StopOptimization");

  m_Stop = true;
  this->InvokeEvent( EndEvent() );
}

/**
 * Advance one Step following the gradient direction
 */
void
RegularStepGradientDescentBaseOptimizer
::AdvanceOneStep(void)
{
  itkDebugMacro("AdvanceOneStep");

  const unsigned int spaceDimension = m_CostFunction->GetNumberOfParameters();

  DerivativeType transformedGradient(spaceDimension);
  DerivativeType previousTransformedGradient(spaceDimension);
  const ScalesType & scales = this->GetScales();

  if ( m_RelaxationFactor < 0.0 )
    {
    itkExceptionMacro(<< "Relaxation factor must be positive. Current value is " << m_RelaxationFactor);
    }

  if ( m_RelaxationFactor >= 1.0 )
    {
    itkExceptionMacro(<< "Relaxation factor must less than 1.0. Current value is " << m_RelaxationFactor);
    }

  // Make sure the scales have been set properly
  if ( scales.size() != spaceDimension )
    {
    itkExceptionMacro(<< "The size of Scales is "
                      << scales.size()
                      << ", but the NumberOfParameters for the CostFunction is "
                      << spaceDimension
                      << ".");
    }

  for ( unsigned int i = 0; i < spaceDimension; i++ )
    {
    transformedGradient[i]  = m_Gradient[i] / scales[i];
    previousTransformedGradient[i] =
      m_PreviousGradient[i] / scales[i];
    }

  double magnitudeSquare = 0;
  for ( unsigned int dim = 0; dim < spaceDimension; dim++ )
    {
    const double weighted = transformedGradient[dim];
    magnitudeSquare += weighted * weighted;
    }

  const double gradientMagnitude = std::sqrt(magnitudeSquare);

  if ( gradientMagnitude < m_GradientMagnitudeTolerance )
    {
    m_StopCondition = GradientMagnitudeTolerance;
    m_StopConditionDescription << "Gradient magnitude tolerance met after "
                               << m_CurrentIteration
                               << " iterations. Gradient magnitude ("
                               << gradientMagnitude
                               << ") is less than gradient magnitude tolerance ("
                               << m_GradientMagnitudeTolerance
                               << ").";
    this->StopOptimization();
    return;
    }

  double scalarProduct = 0;

  for ( unsigned int i = 0; i < spaceDimension; i++ )
    {
    const double weight1 = transformedGradient[i];
    const double weight2 = previousTransformedGradient[i];
    scalarProduct += weight1 * weight2;
    }

  // If there is a direction change
  if ( scalarProduct < 0 )
    {
    m_CurrentStepLength *= m_RelaxationFactor;
    }

  if ( m_CurrentStepLength < m_MinimumStepLength )
    {
    m_StopCondition = StepTooSmall;
    m_StopConditionDescription << "Step too small after "
                               << m_CurrentIteration
                               << " iterations. Current step ("
                               << m_CurrentStepLength
                               << ") is less than minimum step ("
                               << m_MinimumStepLength
                               << ").";
    this->StopOptimization();
    return;
    }

  double direction;
  if ( this->m_Maximize )
    {
    direction = 1.0;
    }
  else
    {
    direction = -1.0;
    }

  const double factor =
    direction * m_CurrentStepLength / gradientMagnitude;

  // This method StepAlongGradient() will
  // be overloaded in non-vector spaces
  this->StepAlongGradient(factor, transformedGradient);

  this->InvokeEvent( IterationEvent() );
}

const std::string
RegularStepGradientDescentBaseOptimizer
::GetStopConditionDescription() const
{
  return m_StopConditionDescription.str();
}

void
RegularStepGradientDescentBaseOptimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MaximumStepLength: "
     << m_MaximumStepLength << std::endl;
  os << indent << "MinimumStepLength: "
     << m_MinimumStepLength << std::endl;
  os << indent << "RelaxationFactor: "
     << m_RelaxationFactor << std::endl;
  os << indent << "GradientMagnitudeTolerance: "
     << m_GradientMagnitudeTolerance << std::endl;
  os << indent << "NumberOfIterations: "
     << m_NumberOfIterations << std::endl;
  os << indent << "CurrentIteration: "
     << m_CurrentIteration   << std::endl;
  os << indent << "Value: "
     << m_Value << std::endl;
  os << indent << "Maximize: "
     << m_Maximize << std::endl;
  if ( m_CostFunction )
    {
    os << indent << "CostFunction: "
       << &m_CostFunction << std::endl;
    }
  else
    {
    os << indent << "CostFunction: "
       << "(None)" << std::endl;
    }
  os << indent << "CurrentStepLength: "
     << m_CurrentStepLength << std::endl;
  os << indent << "StopCondition: "
     << m_StopCondition << std::endl;
  os << indent << "Gradient: "
     << m_Gradient << std::endl;
}
} // end namespace itk

#endif
