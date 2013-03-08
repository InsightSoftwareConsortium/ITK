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

#include "itkGradientDescentOptimizerv4.h"

namespace itk
{

/**
 * Default constructor
 */
GradientDescentOptimizerv4
::GradientDescentOptimizerv4()
{
  this->m_LearningRate = NumericTraits<InternalComputationValueType>::One;

  // m_MaximumStepSizeInPhysicalUnits is used for automatic learning
  // rate estimation. it may be initialized either by calling
  // SetMaximumStepSizeInPhysicalUnits manually or by using m_ScalesEstimator
  // automatically. and the former has higher priority than the latter.
  this->m_MaximumStepSizeInPhysicalUnits = NumericTraits<InternalComputationValueType>::Zero;

  // Initialize parameters for the convergence checker
  this->m_MinimumConvergenceValue = 1e-8;//NumericTraits<InternalComputationValueType>::epsilon();//1e-30;
  this->m_ConvergenceWindowSize = 50;

  this->m_DoEstimateScales = true;
  this->m_DoEstimateLearningRateAtEachIteration = false;
  this->m_DoEstimateLearningRateOnce = true;
  this->m_ReturnBestParametersAndValue = false;
}

/**
 * Destructor
 */
GradientDescentOptimizerv4
::~GradientDescentOptimizerv4()
{}


/**
 *PrintSelf
 */
void
GradientDescentOptimizerv4
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Learning rate:" << this->m_LearningRate << std::endl;
  os << indent << "MaximumStepSizeInPhysicalUnits: "
               << this->m_MaximumStepSizeInPhysicalUnits << std::endl;
  if( this->m_ScalesEstimator.IsNull() )
    {
    os << indent << "No ScalesEstimator set." << std::endl;
    }
  else
    {
    os << indent << "ScalesEstimator: " << std::endl;
    this->m_ScalesEstimator->Print( os, indent.GetNextIndent() );
    }
  os << indent << "DoEstimateScales: " << this->m_DoEstimateScales << std::endl;
  os << indent << "DoEstimateLearningRateAtEachIteration: "
               << this->m_DoEstimateLearningRateAtEachIteration << std::endl;
  os << indent << "DoEstimateLearningRateOnce: "
               << this->m_DoEstimateLearningRateOnce << std::endl;
}

/**
 * Start and run the optimization
 */
void
GradientDescentOptimizerv4
::StartOptimization( bool doOnlyInitialization )
{
  itkDebugMacro("StartOptimization");

  /* Validate some settings */
  if ( this->m_ScalesEstimator.IsNotNull() &&
       this->m_DoEstimateLearningRateOnce &&
       this->m_DoEstimateLearningRateAtEachIteration )
    {
    itkExceptionMacro("Both m_DoEstimateLearningRateOnce and m_DoEstimateLearningRateAtEachIteration are enabled. Not allowed. ");
    }

  /* Estimate the parameter scales if requested. */
  if ( this->m_DoEstimateScales && this->m_ScalesEstimator.IsNotNull() )
    {
    this->m_ScalesEstimator->EstimateScales(this->m_Scales);
    itkDebugMacro( "Estimated scales = " << this->m_Scales );

    /* If user hasn't set this, assign the default. */
    if ( this->m_MaximumStepSizeInPhysicalUnits <=
      NumericTraits<InternalComputationValueType>::epsilon())
      {
      this->m_MaximumStepSizeInPhysicalUnits = this->m_ScalesEstimator->EstimateMaximumStepSize();
      }
    }

  // Initialize the convergence checker
  this->m_ConvergenceMonitoring = ConvergenceMonitoringType::New();
  this->m_ConvergenceMonitoring->SetWindowSize( this->m_ConvergenceWindowSize );

  /* Must call the superclass version for basic validation and setup */
  Superclass::StartOptimization( doOnlyInitialization );

  if( this->m_ReturnBestParametersAndValue )
    {
    this->m_BestParameters = this->GetCurrentPosition( );
    this->m_CurrentBestValue = NumericTraits< MeasureType >::max();
    }

  this->m_CurrentIteration = 0;

  if( ! doOnlyInitialization )
    {
    this->ResumeOptimization();
    }
}

/**
 * StopOptimization
 */
void
GradientDescentOptimizerv4
::StopOptimization(void)
{
  if( this->m_ReturnBestParametersAndValue )
    {
    this->m_Metric->SetParameters( this->m_BestParameters );
    this->m_CurrentMetricValue = this->m_CurrentBestValue;
    }
  Superclass::StopOptimization();
}

/**
 * Resume optimization.
 */
void
GradientDescentOptimizerv4
::ResumeOptimization()
{
  this->m_StopConditionDescription.str("");
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";
  this->InvokeEvent( StartEvent() );

  this->m_Stop = false;
  while( ! this->m_Stop )
    {
    /* Compute metric value/derivative. */
    try
      {
      /* m_Gradient will be sized as needed by metric. If it's already
       * proper size, no new allocation is done. */
      this->m_Metric->GetValueAndDerivative( this->m_CurrentMetricValue, this->m_Gradient );
      }
    catch ( ExceptionObject & err )
      {
      this->m_StopCondition = COSTFUNCTION_ERROR;
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

    /* Check the convergence by WindowConvergenceMonitoringFunction.
     */
    this->m_ConvergenceMonitoring->AddEnergyValue( this->m_CurrentMetricValue );
    try
      {
      this->m_ConvergenceValue = this->m_ConvergenceMonitoring->GetConvergenceValue();
      if (this->m_ConvergenceValue <= this->m_MinimumConvergenceValue)
        {
        this->m_StopConditionDescription << "Convergence checker passed at iteration " << m_CurrentIteration << ".";
        this->m_StopCondition = CONVERGENCE_CHECKER_PASSED;
        this->StopOptimization();
        break;
        }
      }
    catch(std::exception & e)
      {
      std::cerr << "GetConvergenceValue() failed with exception: " << e.what() << std::endl;
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

    if ( this->m_CurrentIteration >= this->m_NumberOfIterations )
      {
      this->m_StopConditionDescription << "Maximum number of iterations (" << this->m_NumberOfIterations << ") exceeded.";
      this->m_StopCondition = MAXIMUM_NUMBER_OF_ITERATIONS;
      this->StopOptimization();
      break;
      }
    } //while (!m_Stop)
}

/**
 * Advance one Step following the gradient direction
 */
void
GradientDescentOptimizerv4
::AdvanceOneStep()
{
  itkDebugMacro("AdvanceOneStep");

  /* Begin threaded gradient modification.
   * Scale by gradient scales, then estimate the learning
   * rate if options are set to (using the scaled gradient),
   * then modify by learning rate. The m_Gradient variable
   * is modified in-place. */
  this->ModifyGradientByScales();
  this->EstimateLearningRate();
  this->ModifyGradientByLearningRate();

  try
    {
    /* Pass graident to transform and let it do its own updating */
    this->m_Metric->UpdateTransformParameters( this->m_Gradient );
    }
  catch ( ExceptionObject & err )
    {
    this->m_StopCondition = UPDATE_PARAMETERS_ERROR;
    this->m_StopConditionDescription << "UpdateTransformParameters error";
    this->StopOptimization();

    // Pass exception to caller
    throw err;
    }

  this->InvokeEvent( IterationEvent() );
}

/**
 * Modify the gradient by scales and weights over a given index range.
 */
void
GradientDescentOptimizerv4
::ModifyGradientByScalesOverSubRange( const IndexRangeType& subrange )
{
  const ScalesType& scales = this->GetScales();
  const ScalesType& weights = this->GetWeights();

  ScalesType factor( scales.Size() );

  if( this->GetWeightsAreIdentity() )
    {
    for( SizeValueType i=0; i < factor.Size(); i++ )
      {
      factor[i] = NumericTraits<ScalesType::ValueType>::OneValue() / scales[i];
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
    }
}

/**
 * Modify the gradient by learning rate over a given index range.
 */
void
GradientDescentOptimizerv4
::ModifyGradientByLearningRateOverSubRange( const IndexRangeType& subrange )
{
  /* Loop over the range. It is inclusive. */
  for ( IndexValueType j = subrange[0]; j <= subrange[1]; j++ )
    {
    this->m_Gradient[j] = this->m_Gradient[j] * this->m_LearningRate;
    }
}

/**
 * Estimate the learning rate.
 */
void
GradientDescentOptimizerv4
::EstimateLearningRate()
{
  if ( this->m_ScalesEstimator.IsNull() )
    {
    return;
    }
  if ( this->m_DoEstimateLearningRateAtEachIteration ||
      (this->m_DoEstimateLearningRateOnce && this->m_CurrentIteration == 0) )
    {
    InternalComputationValueType stepScale
      = this->m_ScalesEstimator->EstimateStepScale(this->m_Gradient);

    if (stepScale <= NumericTraits<InternalComputationValueType>::epsilon())
      {
      this->m_LearningRate = NumericTraits<InternalComputationValueType>::One;
      }
    else
      {
      this->m_LearningRate = this->m_MaximumStepSizeInPhysicalUnits / stepScale;
      }
    }
}

}//namespace itk
