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
#ifndef itkExhaustiveOptimizerv4_hxx
#define itkExhaustiveOptimizerv4_hxx

#include "itkExhaustiveOptimizerv4.h"

namespace itk
{

template<typename TInternalComputationValueType>
ExhaustiveOptimizerv4<TInternalComputationValueType>
::ExhaustiveOptimizerv4() :
  m_CurrentValue(0),
  m_NumberOfSteps(0),
  m_Stop(false),
  m_StepLength(1.0),
  m_CurrentIndex(0),
  m_MaximumMetricValue(0.0),
  m_MinimumMetricValue(0.0),
  m_StopConditionDescription("")
{
  this->m_NumberOfIterations = 0;
}

template<typename TInternalComputationValueType>
void
ExhaustiveOptimizerv4<TInternalComputationValueType>
::StartOptimization(bool /* doOnlyInitialization */)
{
  this->StartWalking();
}

template<typename TInternalComputationValueType>
void
ExhaustiveOptimizerv4<TInternalComputationValueType>
::StartWalking(void)
{
  itkDebugMacro("StartWalking");
  this->InvokeEvent( StartEvent() );
  m_StopConditionDescription.str("");
  m_StopConditionDescription << this->GetNameOfClass() << ": Running";

  ParametersType initialPos = this->m_Metric->GetParameters();
  m_MinimumMetricValuePosition = initialPos;
  m_MaximumMetricValuePosition = initialPos;

  this->SetInitialPosition( initialPos ); // store the initial position

  MeasureType initialValue = this->m_Metric->GetValue();
  m_MaximumMetricValue = initialValue;
  m_MinimumMetricValue = initialValue;

  this->m_CurrentIteration = 0;
  this->m_NumberOfIterations = 1;

  const unsigned int spaceDimension = this->m_Metric->GetParameters().GetSize();

  for ( unsigned int i = 0; i < spaceDimension; i++ )
    {
    this->m_NumberOfIterations *= ( 2 * m_NumberOfSteps[i] + 1 );
    }

  m_CurrentIndex.SetSize(spaceDimension);
  m_CurrentIndex.Fill(0);

  const ScalesType & scales = this->GetScales();
  // Make sure the scales have been set properly
  if ( scales.size() != spaceDimension )
    {
    itkExceptionMacro(<< "The size of Scales is "
                      << scales.size()
                      << ", but the NumberOfParameters is "
                      << spaceDimension
                      << ".");
    }

  // Setup first grid position.
  ParametersType position(spaceDimension);
  for ( unsigned int i = 0; i < spaceDimension; i++ )
    {
    position[i] = this->GetCurrentPosition()[i] - m_NumberOfSteps[i] * m_StepLength * scales[i];
    }
  this->m_Metric->SetParameters(position);

  itkDebugMacro("Calling ResumeWalking");

  this->ResumeWalking();
}

template<typename TInternalComputationValueType>
void
ExhaustiveOptimizerv4<TInternalComputationValueType>
::ResumeWalking(void)
{
  itkDebugMacro("ResumeWalk");
  m_Stop = false;

  while ( !m_Stop )
    {
    ParametersType currentPosition = this->GetCurrentPosition();

    if ( m_Stop )
      {
      StopWalking();
      break;
      }

    m_CurrentValue = this->m_Metric->GetValue();

    if ( m_CurrentValue > m_MaximumMetricValue )
      {
      m_MaximumMetricValue = m_CurrentValue;
      m_MaximumMetricValuePosition = currentPosition;
      }
    if ( m_CurrentValue < m_MinimumMetricValue )
      {
      m_MinimumMetricValue = m_CurrentValue;
      m_MinimumMetricValuePosition = currentPosition;
      }

    if ( m_Stop )
      {
      this->StopWalking();
      break;
      }

    m_StopConditionDescription.str("");
    m_StopConditionDescription << this->GetNameOfClass() << ": Running. ";
    m_StopConditionDescription << "@ index " << this->GetCurrentIndex() << " value is " << m_CurrentValue;

    this->InvokeEvent( IterationEvent() );
    this->AdvanceOneStep();
    this->m_CurrentIteration++;
    }
}

template<typename TInternalComputationValueType>
void
ExhaustiveOptimizerv4<TInternalComputationValueType>
::StopWalking(void)
{
  itkDebugMacro("StopWalking");

  m_Stop = true;
  this->InvokeEvent( EndEvent() );
}

template<typename TInternalComputationValueType>
void
ExhaustiveOptimizerv4<TInternalComputationValueType>
::AdvanceOneStep(void)
{
  itkDebugMacro("AdvanceOneStep");

  const unsigned int spaceDimension = this->m_Metric->GetParameters().GetSize();

  ParametersType newPosition(spaceDimension);
  this->IncrementIndex(newPosition);

  itkDebugMacro(<< "new position = " << newPosition);

  this->m_Metric->SetParameters(newPosition);
}

template<typename TInternalComputationValueType>
void
ExhaustiveOptimizerv4<TInternalComputationValueType>
::IncrementIndex(ParametersType & newPosition)
{
  unsigned int       idx = 0;
  const unsigned int spaceDimension = this->m_Metric->GetParameters().GetSize();

  while ( idx < spaceDimension )
    {
    m_CurrentIndex[idx]++;

    if ( m_CurrentIndex[idx] > ( 2 * m_NumberOfSteps[idx] ) )
      {
      m_CurrentIndex[idx] = 0;
      idx++;
      }
    else
      {
      break;
      }
    }

  if ( idx == spaceDimension )
    {
    m_Stop = true;
    m_StopConditionDescription.str("");
    m_StopConditionDescription << this->GetNameOfClass() << ": ";
    m_StopConditionDescription << "Completed sampling of parametric space of size " << spaceDimension;
    }

  const ScalesType & scales = this->GetScales();
  for ( unsigned int i = 0; i < spaceDimension; i++ )
    {
    newPosition[i] = ( m_CurrentIndex[i] - m_NumberOfSteps[i] )
                     * m_StepLength * scales[i]
                     + this->GetInitialPosition()[i];
    }
}

template<typename TInternalComputationValueType>
const std::string
ExhaustiveOptimizerv4<TInternalComputationValueType>
::GetStopConditionDescription() const
{
  return m_StopConditionDescription.str();
}

template<typename TInternalComputationValueType>
void
ExhaustiveOptimizerv4<TInternalComputationValueType>
::SetInitialPosition(const ParametersType & param)
{
  m_InitialPosition = param;
  this->Modified();
}

template<typename TInternalComputationValueType>
void
ExhaustiveOptimizerv4<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "InitialPosition = " << m_InitialPosition << std::endl;
  os << indent << "CurrentValue = " << m_CurrentValue << std::endl;
  os << indent << "NumberOfSteps = " << m_NumberOfSteps << std::endl;
  os << indent << "Stop = " << m_Stop << std::endl;
  os << indent << "StepLength = " << m_StepLength << std::endl;
  os << indent << "CurrentIndex = " << m_CurrentIndex << std::endl;
  os << indent << "MaximumMetricValue = " << m_MaximumMetricValue << std::endl;
  os << indent << "MinimumMetricValue = " << m_MinimumMetricValue << std::endl;
  os << indent << "MinimumMetricValuePosition = " << m_MinimumMetricValuePosition << std::endl;
  os << indent << "MaximumMetricValuePosition = " << m_MaximumMetricValuePosition << std::endl;
}
} // end namespace itk

#endif
