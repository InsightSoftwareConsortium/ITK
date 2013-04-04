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

#include "itkMultiGradientOptimizerv4.h"

namespace itk
{

//-------------------------------------------------------------------
MultiGradientOptimizerv4
::MultiGradientOptimizerv4()
{
  this->m_NumberOfIterations = static_cast<SizeValueType>(0);
  this->m_CurrentIteration   = static_cast<SizeValueType>(0);
  this->m_StopCondition      = MAXIMUM_NUMBER_OF_ITERATIONS;
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";

  this->m_MaximumMetricValue=NumericTraits<MeasureType>::max();
  this->m_MinimumMetricValue = this->m_MaximumMetricValue;
}

//-------------------------------------------------------------------
MultiGradientOptimizerv4
::~MultiGradientOptimizerv4()
{
}

//-------------------------------------------------------------------
void
MultiGradientOptimizerv4
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number of iterations: " << this->m_NumberOfIterations  << std::endl;
  os << indent << "Current iteration: " << this->m_CurrentIteration << std::endl;
  os << indent << "Stop condition:"<< this->m_StopCondition << std::endl;
  os << indent << "Stop condition description: " << this->m_StopConditionDescription.str()  << std::endl;
}

//-------------------------------------------------------------------
MultiGradientOptimizerv4::OptimizersListType &
MultiGradientOptimizerv4
::GetOptimizersList()
{
  return this->m_OptimizersList;
}

/** Set the list of optimizers to use in the multiple gradient descent */
void
MultiGradientOptimizerv4
::SetOptimizersList(MultiGradientOptimizerv4::OptimizersListType & p)
{
  if( p != this->m_OptimizersList )
    {
    this->m_OptimizersList = p;
    this->Modified();
    }
}

/** Get the list of metric values that we produced after the multi-gradient optimization.  */
const MultiGradientOptimizerv4::MetricValuesListType &
MultiGradientOptimizerv4
::GetMetricValuesList() const
{
  return this->m_MetricValuesList;
}

//-------------------------------------------------------------------
const MultiGradientOptimizerv4::StopConditionReturnStringType
MultiGradientOptimizerv4
::GetStopConditionDescription() const
{
  return this->m_StopConditionDescription.str();
}

//-------------------------------------------------------------------
void
MultiGradientOptimizerv4
::StopOptimization(void)
{
  itkDebugMacro( "StopOptimization called with a description - "
    << this->GetStopConditionDescription() );
  this->m_Stop = true;

  // FIXME
  // this->m_Metric->SetParameters( this->m_OptimizersList[ this->m_BestParametersIndex ] );
  this->InvokeEvent( EndEvent() );
}

/**
 * Start and run the optimization
 */
void
MultiGradientOptimizerv4
::StartOptimization( bool doOnlyInitialization )
{
  itkDebugMacro("StartOptimization");
  SizeValueType maxOpt=this->m_OptimizersList.size();
  if ( maxOpt == NumericTraits<SizeValueType>::Zero )
    {
    itkExceptionMacro(" No optimizers are set.");
    }
  if ( ! this->m_Metric )
    {
    this->m_Metric = this->m_OptimizersList[0]->GetModifiableMetric();
    }
  this->m_MetricValuesList.clear();
  this->m_MinimumMetricValue = this->m_MaximumMetricValue;
  const ParametersType & testParamsAreTheSameObject = this->m_OptimizersList[0]->GetCurrentPosition();
  this->m_MetricValuesList.push_back( this->m_MaximumMetricValue );
  /* Initialize the optimizer, but don't run it. */
  this->m_OptimizersList[0]->StartOptimization( true /* doOnlyInitialization */ );

  for ( SizeValueType whichOptimizer = 1; whichOptimizer < maxOpt; whichOptimizer++ )
    {
    this->m_MetricValuesList.push_back(this->m_MaximumMetricValue);
    const ParametersType & compareParams = this->m_OptimizersList[whichOptimizer]->GetCurrentPosition();
    if ( &compareParams != &testParamsAreTheSameObject )
      {
      itkExceptionMacro(" Parameter objects are not identical across all optimizers/metrics.");
      }
    /* Initialize the optimizer, but don't run it. */
    this->m_OptimizersList[whichOptimizer]->StartOptimization( true /* doOnlyInitialization */ );
    }

  this->m_CurrentIteration = static_cast<SizeValueType>(0);

  /* Must call the superclass version for basic validation and setup,
   * and to start the optimization loop. */
  if ( this->m_NumberOfIterations > static_cast<SizeValueType>(0) )
    {
    Superclass::StartOptimization( doOnlyInitialization );
    }
}

/**
 * Resume optimization.
 */
void
MultiGradientOptimizerv4
::ResumeOptimization()
{
  this->m_StopConditionDescription.str("");
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";
  this->InvokeEvent( StartEvent() );
  itkDebugMacro(" start ");
  this->m_Stop = false;
  while( ! this->m_Stop )
    {
    /* Compute metric value/derivative. */

    SizeValueType maxOpt = this->m_OptimizersList.size();
    /** we rely on learning rate or parameter scale estimator to do the weighting */
    InternalComputationValueType combinefunction = NumericTraits<InternalComputationValueType>::OneValue() / static_cast<InternalComputationValueType>(maxOpt);
    itkDebugMacro(" nopt " << maxOpt);

    for (SizeValueType whichOptimizer = 0; whichOptimizer < maxOpt; whichOptimizer++ )
      {
      this->m_OptimizersList[whichOptimizer]->GetMetric()->GetValueAndDerivative(
        const_cast<MeasureType&>( this->m_OptimizersList[whichOptimizer]->GetCurrentMetricValue() ),
        const_cast<DerivativeType&>( this->m_OptimizersList[whichOptimizer]->GetGradient() ) );
      itkDebugMacro(" got-deriv " << whichOptimizer);
      if ( this->m_Gradient.Size() != this->m_OptimizersList[whichOptimizer]->GetGradient().Size() )
        {
        this->m_Gradient.SetSize( this->m_OptimizersList[whichOptimizer]->GetGradient().Size() );
        itkDebugMacro(" resized ");
        }

      /* Modify the gradient by scales, weights and learning rate */
      this->m_OptimizersList[whichOptimizer]->ModifyGradientByScales();
      this->m_OptimizersList[whichOptimizer]->EstimateLearningRate();
      this->m_OptimizersList[whichOptimizer]->ModifyGradientByLearningRate();

      itkDebugMacro(" mod-grad ");
      /** combine the gradients */
      if ( whichOptimizer == 0 )
        {
        this->m_Gradient.Fill(0);
        }
      this->m_Gradient = this->m_Gradient + this->m_OptimizersList[whichOptimizer]->GetGradient() * combinefunction;
      itkDebugMacro(" add-grad ");
      this->m_MetricValuesList[whichOptimizer] = this->m_OptimizersList[whichOptimizer]->GetCurrentMetricValue();
      }//endfor

    /* Check if optimization has been stopped externally.
    * (Presumably this could happen from a multi-threaded client app?) */
    if ( this->m_Stop )
      {
      this->m_StopConditionDescription << "StopOptimization() called";
      break;
      }
    try
      {
      /* Pass combined gradient to transforms and let them update */
      itkDebugMacro(" combine-grad ");
      this->m_OptimizersList[0]->GetModifiableMetric()->UpdateTransformParameters( this->m_Gradient );
      }
    catch ( ExceptionObject & )
      {
      this->m_StopCondition = UPDATE_PARAMETERS_ERROR;
      this->m_StopConditionDescription << "UpdateTransformParameters error";
      this->StopOptimization();
      // Pass exception to caller
      throw;
      }
    this->InvokeEvent( IterationEvent() );
    /* Update and check iteration count */
    this->m_CurrentIteration++;
    if ( this->m_CurrentIteration >= this->m_NumberOfIterations )
      {
      this->m_StopConditionDescription << "Maximum number of iterations (" << this->m_NumberOfIterations << ") exceeded.";
      this->m_StopCondition = MAXIMUM_NUMBER_OF_ITERATIONS;
      this->StopOptimization();
      break;
      }
    }  //while (!m_Stop)
}

} //namespace itk
