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
#ifndef __itkRegularStepGradientDescentOptimizerv4_hxx
#define __itkRegularStepGradientDescentOptimizerv4_hxx

#include "itkRegularStepGradientDescentOptimizerv4.h"

namespace itk
{

template<typename TInternalComputationValueType>
RegularStepGradientDescentOptimizerv4<TInternalComputationValueType>
::RegularStepGradientDescentOptimizerv4():
  m_LearningRate( NumericTraits<TInternalComputationValueType>::One ),
  m_RelaxationFactor( 0.5 ),
  m_MinimumStepLength( 1e-4 ), // Initialize parameter for the convergence checker
  m_GradientMagnitudeTolerance( 1e-4 ),
  m_CurrentStepLength( 0 ),
  m_ReturnBestParametersAndValue( false )
{
  this->m_PreviousGradient.Fill(0.0f);
}

template<typename TInternalComputationValueType>
RegularStepGradientDescentOptimizerv4<TInternalComputationValueType>
::~RegularStepGradientDescentOptimizerv4()
{}

template<typename TInternalComputationValueType>
void
RegularStepGradientDescentOptimizerv4<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Learning rate (step size):" << this->m_LearningRate << std::endl;
  os << indent << "Minimum step length: " << this->m_MinimumStepLength << std::endl;
  os << indent << "Relaxation factor: " << this->m_RelaxationFactor << std::endl;
}

template<typename TInternalComputationValueType>
void
RegularStepGradientDescentOptimizerv4<TInternalComputationValueType>
::StartOptimization( bool doOnlyInitialization )
{
  itkDebugMacro("StartOptimization");

  /* Estimate the parameter scales if requested. */
  if ( this->m_ScalesEstimator.IsNotNull() && this->m_DoEstimateScales )
    {
    this->m_ScalesEstimator->EstimateScales(this->m_Scales);
    itkDebugMacro( "Estimated scales = " << this->m_Scales );
    }

  /* Must call the superclass version for basic validation and setup */
  Superclass::StartOptimization( doOnlyInitialization );

  if( this->m_ReturnBestParametersAndValue )
    {
    this->m_BestParameters = this->GetCurrentPosition( );
    this->m_CurrentBestValue = NumericTraits< MeasureType >::max();
    }

  const SizeValueType spaceDimension = this->m_Metric->GetNumberOfParameters();

  this->m_Gradient = DerivativeType(spaceDimension);
  this->m_PreviousGradient = DerivativeType(spaceDimension);
  this->m_Gradient.Fill(0.0f);
  this->m_PreviousGradient.Fill(0.0f);

  // Reset the iterations and step length when the optimizer is started again.
  this->m_CurrentStepLength = this->m_LearningRate;
  this->m_CurrentIteration = 0;

  // validity check for the value of GradientMagnitudeTolerance
  if ( m_GradientMagnitudeTolerance < 0.0 )
    {
    itkExceptionMacro(<< "Gradient magnitude tolerance must be"
                         "greater or equal 0.0. Current value is " << m_GradientMagnitudeTolerance);
    }

  if( ! doOnlyInitialization )
    {
    this->ResumeOptimization();
    }
}

template<typename TInternalComputationValueType>
void
RegularStepGradientDescentOptimizerv4<TInternalComputationValueType>
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
RegularStepGradientDescentOptimizerv4<TInternalComputationValueType>
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
      this->m_StopCondition = Superclass::MAXIMUM_NUMBER_OF_ITERATIONS;
      this->m_StopConditionDescription << "Maximum number of iterations (" << this->m_NumberOfIterations << ") exceeded.";
      this->StopOptimization();
      break;
      }

    this->m_PreviousGradient = this->m_Gradient;
    /* Compute metric value/derivative. */
    try
      {
      /* m_Gradient will be sized as needed by metric. If it's already
       * proper size, no new allocation is done. */
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

    /* Check if optimization has been stopped externally.
     * (Presumably this could happen from a multi-threaded client app?) */
    if ( this->m_Stop )
      {
      this->m_StopConditionDescription << "StopOptimization() called";
      break;
      }

    /* Advance one step along the gradient.
     * This will modify the gradient and update the transform. */
    this->AdvanceOneStep();

    /* Store best value and position */
    if ( this->m_ReturnBestParametersAndValue && this->m_CurrentMetricValue < this->m_CurrentBestValue )
      {
      this->m_CurrentBestValue = this->m_CurrentMetricValue;
      this->m_BestParameters = this->GetCurrentPosition( );
      }

    /* Update and check iteration count */
    this->m_CurrentIteration++;

    } //while (!m_Stop)
}

template<typename TInternalComputationValueType>
void
RegularStepGradientDescentOptimizerv4<TInternalComputationValueType>
::AdvanceOneStep()
{
  itkDebugMacro("AdvanceOneStep");

  // Make sure the scales have been set properly
  if ( this->m_Scales.size() != this->m_Gradient.Size() )
    {
    itkExceptionMacro(<< "The size of Scales is "
                      << this->m_Scales.size()
                      << ", but the NumberOfParameters for the CostFunction is "
                      << this->m_Gradient.Size()
                      << ".");
    }

  if ( this->m_RelaxationFactor < 0.0 )
    {
    itkExceptionMacro(<< "Relaxation factor must be positive. Current value is " << this->m_RelaxationFactor);
    }

  if ( this->m_RelaxationFactor >= 1.0 )
    {
    itkExceptionMacro(<< "Relaxation factor must less than 1.0. Current value is " << this->m_RelaxationFactor);
    }

  /* Begin threaded gradient modification.
   * Scale gradient and previous gradient by scales.
   * The m_Gradient and m_PreviousGradient variables are modified in-place. */
  this->ModifyGradientByScales();

  CompensatedSummationType compensatedSummation;
  for( SizeValueType dim = 0; dim < this->m_Gradient.Size(); ++dim )
    {
    const double weighted = this->m_Gradient[dim];
    compensatedSummation += weighted * weighted;
    }
  const double gradientMagnitude = vcl_sqrt( compensatedSummation.GetSum() );

  if( gradientMagnitude < this->m_GradientMagnitudeTolerance )
    {
    this->m_StopCondition = Superclass::GRADIENT_MAGNITUDE_TOLEARANCE;
    this->m_StopConditionDescription << "Gradient magnitude tolerance met after "
                                      << this->m_CurrentIteration
                                      << " iterations. Gradient magnitude ("
                                      << gradientMagnitude
                                      << ") is less than gradient magnitude tolerance ("
                                      << this->m_GradientMagnitudeTolerance
                                      << ").";
    this->StopOptimization();
    return;
    }

  compensatedSummation.ResetToZero();
  for ( SizeValueType i = 0; i < this->m_Gradient.Size(); i++ )
    {
    const double weight1 = this->m_Gradient[i];
    const double weight2 = this->m_PreviousGradient[i];
    compensatedSummation += weight1 * weight2;
    }
  const double scalarProduct = compensatedSummation.GetSum();

    // If there is a direction change
  if ( scalarProduct < 0 )
    {
    this->m_CurrentStepLength *= this->m_RelaxationFactor;
    }

  if ( this->m_CurrentStepLength < this->m_MinimumStepLength )
    {
    this->m_StopCondition = Superclass::STEP_TOO_SMALL;
    this->m_StopConditionDescription << "Step too small after "
                                  << this->m_CurrentIteration
                                  << " iterations. Current step ("
                                  << this->m_CurrentStepLength
                                  << ") is less than minimum step ("
                                  << this->m_MinimumStepLength
                                  << ").";
    this->StopOptimization();
    return;
    }

  this->ModifyGradientByLearningRate();
  const double factor = NumericTraits<typename ScalesType::ValueType>::OneValue() / gradientMagnitude;

  try
    {
    /* Pass graident to transform and let it do its own updating */
    this->m_Metric->UpdateTransformParameters( this->m_Gradient, factor );
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
RegularStepGradientDescentOptimizerv4<TInternalComputationValueType>
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

  /* Loop over the range. It is inclusive. */
  for ( IndexValueType j = subrange[0]; j <= subrange[1]; j++ )
    {
      // scales is checked during StartOptmization for values <=
      // machine epsilon.
      // Take the modulo of the index to handle gradients from transforms
      // with local support. The gradient array stores the gradient of local
      // parameters at each local index with linear packing.
    IndexValueType index = j % scales.Size();
    this->m_Gradient[j] = this->m_Gradient[j] * factor[index];
    this->m_PreviousGradient[j] = this->m_PreviousGradient[j] * factor[index];
    }
}

template<typename TInternalComputationValueType>
void
RegularStepGradientDescentOptimizerv4<TInternalComputationValueType>
::ModifyGradientByLearningRateOverSubRange( const IndexRangeType& subrange )
{
  /* Loop over the range. It is inclusive. */
  for ( IndexValueType j = subrange[0]; j <= subrange[1]; j++ )
    {
    this->m_Gradient[j] = this->m_Gradient[j] * this->m_CurrentStepLength;
    }
}

}//namespace itk

#endif
