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
#ifndef itkGradientDescentOptimizerv4_hxx
#define itkGradientDescentOptimizerv4_hxx

#include "itkGradientDescentOptimizerv4.h"

namespace itk
{

template<typename TInternalComputationValueType>
GradientDescentOptimizerv4Template<TInternalComputationValueType>
::GradientDescentOptimizerv4Template() :
  m_LearningRate( NumericTraits<TInternalComputationValueType>::OneValue() ),
  m_MinimumConvergenceValue( 1e-8 ),
  m_ConvergenceValue( NumericTraits<TInternalComputationValueType>::max() ),
  m_CurrentBestValue( NumericTraits<MeasureType>::max() ),
  m_ReturnBestParametersAndValue( false )
{
  this->m_PreviousGradient.Fill( NumericTraits<TInternalComputationValueType>::ZeroValue() );
}

template<typename TInternalComputationValueType>
GradientDescentOptimizerv4Template<TInternalComputationValueType>
::~GradientDescentOptimizerv4Template()
{}

template<typename TInternalComputationValueType>
void
GradientDescentOptimizerv4Template<TInternalComputationValueType>
::StartOptimization( bool doOnlyInitialization )
{
  // Must call the superclass version for basic validation and setup.
  Superclass::StartOptimization( doOnlyInitialization );

  if( this->m_ReturnBestParametersAndValue )
    {
    this->m_BestParameters = this->GetCurrentPosition( );
    this->m_CurrentBestValue = NumericTraits< MeasureType >::max();
    }

  this->m_CurrentIteration = 0;
  this->m_ConvergenceValue = NumericTraits<TInternalComputationValueType>::max();

  if( ! doOnlyInitialization )
    {
    this->ResumeOptimization();
    }
}

template<typename TInternalComputationValueType>
void
GradientDescentOptimizerv4Template<TInternalComputationValueType>
::StopOptimization(void)
{
  if( this->m_ReturnBestParametersAndValue )
    {
    this->m_Metric->SetParameters( this->m_BestParameters );
    this->m_CurrentMetricValue = this->m_CurrentBestValue;
    }
  Superclass::StopOptimization();
}

template<typename TInternalComputationValueType>
void
GradientDescentOptimizerv4Template<TInternalComputationValueType>
::ResumeOptimization()
{
  this->m_StopConditionDescription.str("");
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";
  this->InvokeEvent( StartEvent() );

  this->m_Stop = false;
  while( ! this->m_Stop )
    {
    // Do not run the loop if the maximum number of iterations is reached or its value is zero.
    if ( this->m_CurrentIteration >= this->m_NumberOfIterations )
      {
      this->m_StopConditionDescription << "Maximum number of iterations (" << this->m_NumberOfIterations << ") exceeded.";
      this->m_StopCondition = Superclass::MAXIMUM_NUMBER_OF_ITERATIONS;
      this->StopOptimization();
      break;
      }

    // Save previous value with shallow swap that will be used by child optimizer.
    swap( this->m_PreviousGradient, this->m_Gradient );

    // Compute metric value/derivative.
    try
      {
      // m_Gradient will be sized as needed by metric. If it's already
      // proper size, no new allocation is done.
      this->m_Metric->GetValueAndDerivative( this->m_CurrentMetricValue, this->m_Gradient );
      }
    catch ( ExceptionObject & err )
      {
      this->m_StopCondition = Superclass::COSTFUNCTION_ERROR;
      this->m_StopConditionDescription << "Metric error during optimization";
      this->StopOptimization();

      // Pass exception to caller
      throw err;
      }

    // Check if optimization has been stopped externally.
    // (Presumably this could happen from a multi-threaded client app?)
    if ( this->m_Stop )
      {
      this->m_StopConditionDescription << "StopOptimization() called";
      break;
      }

    // Check the convergence by WindowConvergenceMonitoringFunction.
    if ( this->m_UseConvergenceMonitoring )
      {
      this->m_ConvergenceMonitoring->AddEnergyValue( this->m_CurrentMetricValue );
      try
        {
        this->m_ConvergenceValue = this->m_ConvergenceMonitoring->GetConvergenceValue();
        if (this->m_ConvergenceValue <= this->m_MinimumConvergenceValue)
          {
          this->m_StopCondition = Superclass::CONVERGENCE_CHECKER_PASSED;
          this->m_StopConditionDescription << "Convergence checker passed at iteration " << this->m_CurrentIteration << ".";
          this->StopOptimization();
          break;
          }
        }
      catch(std::exception & e)
        {
        itkWarningMacro(<< "GetConvergenceValue() failed with exception: " << e.what() << std::endl);
        }
      }

    // Advance one step along the gradient.
    // This will modify the gradient and update the transform.
    this->AdvanceOneStep();

    // Store best value and position
    if ( this->m_ReturnBestParametersAndValue && this->m_CurrentMetricValue < this->m_CurrentBestValue )
      {
      this->m_CurrentBestValue = this->m_CurrentMetricValue;
      this->m_BestParameters = this->GetCurrentPosition( );
      }

    // Update and check iteration count
    this->m_CurrentIteration++;

    } //while (!m_Stop)
}

template<typename TInternalComputationValueType>
void
GradientDescentOptimizerv4Template<TInternalComputationValueType>
::AdvanceOneStep()
{
  itkDebugMacro("AdvanceOneStep");

  // Begin threaded gradient modification.
  // Scale by gradient scales, then estimate the learning
  // rate if options are set to (using the scaled gradient),
  // then modify by learning rate. The m_Gradient variable
  // is modified in-place.
  this->ModifyGradientByScales();
  this->EstimateLearningRate();
  this->ModifyGradientByLearningRate();

  try
    {
    // Pass gradient to transform and let it do its own updating
    this->m_Metric->UpdateTransformParameters( this->m_Gradient );
    }
  catch ( ExceptionObject & err )
    {
    this->m_StopCondition = Superclass::UPDATE_PARAMETERS_ERROR;
    this->m_StopConditionDescription << "UpdateTransformParameters error";
    this->StopOptimization();

      // Pass exception to caller
    throw err;
    }

  this->InvokeEvent( IterationEvent() );
}

template<typename TInternalComputationValueType>
void
GradientDescentOptimizerv4Template<TInternalComputationValueType>
::ModifyGradientByScalesOverSubRange( const IndexRangeType& subrange )
{
  const ScalesType& scales = this->GetScales();
  const ScalesType& weights = this->GetWeights();

  ScalesType factor( scales.Size() );

  if( this->GetWeightsAreIdentity() )
    {
    for( SizeValueType i=0; i < factor.Size(); i++ )
      {
      factor[i] = NumericTraits<typename ScalesType::ValueType>::OneValue() / scales[i];
      }
    }
  else
    {
    for( SizeValueType i=0; i < factor.Size(); i++ )
      {
      factor[i] = weights[i] / scales[i];
      }
    }

  // Loop over the range. It is inclusive.
  for ( IndexValueType j = subrange[0]; j <= subrange[1]; j++ )
    {
      // scales is checked during StartOptmization for values <=
      // machine epsilon.
      // Take the modulo of the index to handle gradients from transforms
      // with local support. The gradient array stores the gradient of local
      // parameters at each local index with linear packing.
    IndexValueType index = j % scales.Size();
    this->m_Gradient[j] = this->m_Gradient[j] * factor[index];
    }
}

template<typename TInternalComputationValueType>
void
GradientDescentOptimizerv4Template<TInternalComputationValueType>
::ModifyGradientByLearningRateOverSubRange( const IndexRangeType& subrange )
{
  // Loop over the range. It is inclusive.
  for ( IndexValueType j = subrange[0]; j <= subrange[1]; j++ )
    {
    this->m_Gradient[j] = this->m_Gradient[j] * this->m_LearningRate;
    }
}

template<typename TInternalComputationValueType>
void
GradientDescentOptimizerv4Template<TInternalComputationValueType>
::EstimateLearningRate()
{
  if ( this->m_ScalesEstimator.IsNull() )
    {
    return;
    }
  if ( this->m_DoEstimateLearningRateAtEachIteration ||
      (this->m_DoEstimateLearningRateOnce && this->m_CurrentIteration == 0) )
    {
    TInternalComputationValueType stepScale =
      this->m_ScalesEstimator->EstimateStepScale(this->m_Gradient);

    if (stepScale <= NumericTraits<TInternalComputationValueType>::epsilon())
      {
      this->m_LearningRate = NumericTraits<TInternalComputationValueType>::OneValue();
      }
    else
      {
      this->m_LearningRate = this->m_MaximumStepSizeInPhysicalUnits / stepScale;
      }
    }
}

template<typename TInternalComputationValueType>
void
GradientDescentOptimizerv4Template<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "LearningRate: "
    << static_cast< typename NumericTraits< TInternalComputationValueType >::PrintType >( this->m_LearningRate )
    << std::endl;
  os << indent << "MinimumConvergenceValue: " << this->m_MinimumConvergenceValue << std::endl;
  os << indent << "ConvergenceValue: "
    << static_cast< typename NumericTraits< TInternalComputationValueType >::PrintType >( this->m_ConvergenceValue )
    << std::endl;
  os << indent << "CurrentBestValue: "
    << static_cast< typename NumericTraits< MeasureType >::PrintType >( this->m_CurrentBestValue )
    << std::endl;
  os << indent << "BestParameters: "
    << static_cast< typename NumericTraits< ParametersType >::PrintType >( this->m_BestParameters )
    << std::endl;
  os << indent << "ReturnBestParametersAndValue: "
    << ( this->m_ReturnBestParametersAndValue ? "On" : "Off" ) << std::endl;
  os << indent << "PreviousGradient: "
    << static_cast< typename NumericTraits< DerivativeType >::PrintType >( this->m_PreviousGradient )
    << std::endl;
}
}//namespace itk

#endif
