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
#ifndef itkQuasiNewtonOptimizerv4_hxx
#define itkQuasiNewtonOptimizerv4_hxx

#include "itkQuasiNewtonOptimizerv4.h"
#include "itkQuasiNewtonOptimizerv4EstimateNewtonStepThreader.h"

namespace itk
{

template<typename TInternalComputationValueType>
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::QuasiNewtonOptimizerv4Template():
  m_MaximumIterationsWithoutProgress(30),
  m_PreviousValue(0.0),
  m_BestValue(0.0),
  m_BestPosition(0),
  m_BestIteration(0)
{
  this->m_LearningRate = NumericTraits<TInternalComputationValueType>::OneValue();

  // m_MaximumNewtonStepSizeInPhysicalUnits is used for automatic learning
  // rate estimation. it may be initialized either by calling
  // SetMaximumNewtonStepSizeInPhysicalUnits manually or by using m_ScalesEstimator
  // automatically. and the former has higher priority than the latter.
  this->m_MaximumNewtonStepSizeInPhysicalUnits = NumericTraits<TInternalComputationValueType>::ZeroValue();

  /** Threader for Quasi-Newton method */
  typedef QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate<TInternalComputationValueType>
    OptimizerType;
  typedef typename OptimizerType::Pointer
    OptimizerPointer;
  OptimizerPointer estimateNewtonStepThreader = OptimizerType::New();

  this->m_EstimateNewtonStepThreader = estimateNewtonStepThreader;
}

template<typename TInternalComputationValueType>
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::~QuasiNewtonOptimizerv4Template()
{
}

template<typename TInternalComputationValueType>
void
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template<typename TInternalComputationValueType>
void
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::StartOptimization( bool doOnlyInitialization )
{
  itkDebugMacro("StartOptimization");

  const SizeValueType numPara = this->m_Metric->GetNumberOfParameters();
  const SizeValueType numLocalPara = this->m_Metric->GetNumberOfLocalParameters();
  const SizeValueType numLocals = this->m_Metric->GetNumberOfParameters() / numLocalPara;

  /* Set up the data for Quasi-Newton method */
  this->m_NewtonStep.SetSize(numPara);

  this->m_NewtonStepValidFlags.resize(numLocals);

  this->m_HessianArray.resize(numLocals);
  for (SizeValueType loc=0; loc<numLocals; loc++)
    {
    this->m_HessianArray[loc].SetSize(numLocalPara, numLocalPara);
    }

  if ( this->m_ScalesEstimator.IsNotNull() )
    {
    if ( this->m_MaximumNewtonStepSizeInPhysicalUnits <=
        NumericTraits<TInternalComputationValueType>::epsilon())
      {
        // Newton step size might be bigger than one voxel spacing.
        // emperically, we set it to 1~5 voxel spacings.
      this->m_MaximumNewtonStepSizeInPhysicalUnits =
      3.0 * this->m_ScalesEstimator->EstimateMaximumStepSize();
      }
    }

  /* Must call the superclass version for basic validation, setup,
   * and to start the optimization loop. */
  Superclass::StartOptimization( doOnlyInitialization );
}

template<typename TInternalComputationValueType>
void
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::AdvanceOneStep(void)
{
  itkDebugMacro("AdvanceOneStep");
  const SizeValueType numPara = this->m_Metric->GetNumberOfParameters();
  this->m_CurrentPosition = this->m_Metric->GetParameters();

  if (this->GetCurrentIteration() == 0)
    {
      // initialize some information
    this->m_PreviousValue = this->GetCurrentMetricValue();
    this->m_PreviousPosition = this->GetCurrentPosition();
    this->m_PreviousGradient = this->GetGradient();

    this->m_BestValue = this->m_CurrentMetricValue;
    this->m_BestPosition = this->m_CurrentPosition;
    this->m_BestIteration = this->GetCurrentIteration();
    }
  else if (m_BestValue > this->m_CurrentMetricValue)
    {
      // store the best value and related information
    this->m_BestValue = this->m_CurrentMetricValue;
    this->m_BestPosition = this->m_CurrentPosition;
    this->m_BestIteration = this->GetCurrentIteration();
    }

  if ( this->GetCurrentIteration() - m_BestIteration
      > m_MaximumIterationsWithoutProgress )
    {
    ParametersType backStep;
    backStep = m_BestPosition - this->m_Metric->GetParameters();
    this->m_Metric->UpdateTransformParameters( backStep );

    this->m_CurrentPosition = this->m_BestPosition;
    this->m_CurrentMetricValue = this->m_BestValue;

    this->m_StopCondition = Superclass::STEP_TOO_SMALL;
    this->m_StopConditionDescription << "Optimization stops after "
    << this->GetCurrentIteration()
    << " iterations since"
    << " there is no progress in the last "
    << m_MaximumIterationsWithoutProgress
    << " steps." << std::endl
    << " The best value is from Iteration "
    << m_BestIteration << ".";
    this->StopOptimization();
    return;
    }

  if (this->GetCurrentIteration() > 0)
    {
    ParametersType lastStep(numPara);
    lastStep = this->m_CurrentPosition - this->m_PreviousPosition;
    if (lastStep.squared_magnitude() <
        NumericTraits<TInternalComputationValueType>::epsilon())
      {
      this->m_StopCondition = Superclass::STEP_TOO_SMALL;
      this->m_StopConditionDescription << "Optimization stops after "
      << this->GetCurrentIteration()
      << " iterations since"
      << " the last step is almost zero.";
      this->StopOptimization();
      return;
      }
    }

  this->EstimateNewtonStep();

  /** Save for the next iteration */
  this->m_PreviousValue = this->GetCurrentMetricValue();
  this->m_PreviousPosition = this->GetCurrentPosition();
  this->m_PreviousGradient = this->GetGradient();

  this->CombineGradientNewtonStep();
  this->ModifyCombinedNewtonStep();

  try
    {
    /* Pass gradient to transform and let it do its own updating */
    this->m_Metric->UpdateTransformParameters( this->m_NewtonStep );
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
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::CombineGradientNewtonStep()
{
  const SizeValueType numLocalPara = this->m_Metric->GetNumberOfLocalParameters();
  const SizeValueType numLocals = this->m_Metric->GetNumberOfParameters() / numLocalPara;

  bool validNewtonStepExists = false;
  for (SizeValueType loc=0; loc<numLocals; loc++)
    {
    if (this->m_NewtonStepValidFlags[loc])
      {
      validNewtonStepExists = true;
      break;
      }
    }

  TInternalComputationValueType ratio = NumericTraits<TInternalComputationValueType>::OneValue();
  if (validNewtonStepExists)
    {
    TInternalComputationValueType gradStepScale
    = this->m_ScalesEstimator->EstimateStepScale(this->m_Gradient);
    TInternalComputationValueType newtonStepScale
    = this->m_ScalesEstimator->EstimateStepScale(this->m_NewtonStep);
    if (gradStepScale > NumericTraits<TInternalComputationValueType>::epsilon())
      {
      ratio = newtonStepScale / gradStepScale;
      }
    }

  for (SizeValueType loc=0; loc<numLocals; loc++)
    {
    if (!this->m_NewtonStepValidFlags[loc])
      {
        // Using the Gradient step
      const SizeValueType offset = loc * numLocalPara;
      for (SizeValueType p = offset; p < (offset + numLocalPara); p++)
        {
        this->m_NewtonStep[p] = this->m_Gradient[p] * ratio;
        }
      }
    }
}

template<typename TInternalComputationValueType>
void
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::ModifyCombinedNewtonStep()
{
    // If m_ScalesEstimator is not set, we will not change the Newton step.
  if (this->m_ScalesEstimator.IsNull())
    {
    return;
    }

  TInternalComputationValueType stepScale
  = this->m_ScalesEstimator->EstimateStepScale(this->m_NewtonStep);

  if (stepScale <= NumericTraits<TInternalComputationValueType>::epsilon())
    {
    this->m_LearningRate = NumericTraits<TInternalComputationValueType>::OneValue();
    }
  else
    {
    this->m_LearningRate = this->m_MaximumNewtonStepSizeInPhysicalUnits / stepScale;
    if (this->m_LearningRate > NumericTraits<TInternalComputationValueType>::OneValue())
      {
        // learning rate is at most 1 for a newton step
      this->m_LearningRate = NumericTraits<TInternalComputationValueType>::OneValue();
      }
    }

  if (std::abs(this->m_LearningRate - NumericTraits<TInternalComputationValueType>::OneValue())
      > 0.01)
    {
    this->m_NewtonStep *= this->m_LearningRate;
    }
}

template<typename TInternalComputationValueType>
void
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::ResetNewtonStep(IndexValueType loc)
{
  const SizeValueType numLocalPara = this->m_Metric->GetNumberOfLocalParameters();

    // Initialize Hessian to identity matrix
  m_HessianArray[loc].Fill(NumericTraits<TInternalComputationValueType>::ZeroValue());

  for (unsigned int i=0; i<numLocalPara; i++)
    {
    m_HessianArray[loc][i][i] = NumericTraits<TInternalComputationValueType>::OneValue(); //identity matrix
    }

  IndexValueType offset = loc * numLocalPara;
  for (SizeValueType p=0; p<numLocalPara; p++)
    {
      // Set to zero for invalid Newton steps.
      // They must be defined since they will be used during step scale estimation.
    this->m_NewtonStep[offset+p] = NumericTraits<TInternalComputationValueType>::ZeroValue();
    }
}

template<typename TInternalComputationValueType>
void
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::EstimateNewtonStep()
{
  if ( this->m_Gradient.GetSize() == 0 )
    {
    return;
    }

  IndexRangeType fullrange;
  fullrange[0] = 0;
  fullrange[1] = this->m_Gradient.GetSize()-1; //range is inclusive
  /* Perform the modification either with or without threading */

  if( this->m_Gradient.GetSize() > 10000 )
    {
    /* This ends up calling EstimateNewtonStepOverSubRange from each thread */
    this->m_EstimateNewtonStepThreader->Execute( this, fullrange );
    }
  else
    {
    this->EstimateNewtonStepOverSubRange( fullrange );
    }
}

template<typename TInternalComputationValueType>
void
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::EstimateNewtonStepOverSubRange( const IndexRangeType& subrange )
{
  const SizeValueType numLocalPara = this->m_Metric->GetNumberOfLocalParameters();

  IndexValueType low = subrange[0] / numLocalPara;
  IndexValueType high = subrange[1] / numLocalPara;

    // let us denote the i-th thread's sub range by subrange_i
    // we assume subrange_i[1] + 1 = subrange_(i+1)[0] .
    // if the subrange_i doesn't start with the multiple of numLocalPara,
    // we assign this starting block of local parameters to thread_i
    // if the subrange_i doesn't end with the multiple of numLocalPara,
    // we assign this ending block of local parameters to thread_(i+1) .
  if( (subrange[1]+1) % numLocalPara != 0 )
    {
    high--;
    }

  for (IndexValueType loc = low; loc <= high; loc++)
    {
    if (this->GetCurrentIteration() == 0)
      {
      this->m_NewtonStepValidFlags[loc] = false;
      }
    else
      {
      this->m_NewtonStepValidFlags[loc] = this->ComputeHessianAndStepWithBFGS(loc);
      }

    if (!this->m_NewtonStepValidFlags[loc])
      {
      this->ResetNewtonStep(loc);
      }

    } // end for loc
}

template<typename TInternalComputationValueType>
bool
QuasiNewtonOptimizerv4Template<TInternalComputationValueType>
::ComputeHessianAndStepWithBFGS(IndexValueType loc)
{
  if (this->GetCurrentIteration() == 0)
    {
    return false;
    }

  const SizeValueType numLocalPara = this->m_Metric->GetNumberOfLocalParameters();
  IndexValueType offset = loc * numLocalPara;

  ParametersType dx(numLocalPara);  //delta of position x: x_k+1 - x_k
  DerivativeType dg(numLocalPara);  //delta of gradient: g_k+1 - g_k
  DerivativeType edg(numLocalPara); //estimated delta of gradient: hessian_k * dx

  for (SizeValueType p=0; p<numLocalPara; p++)
    {
    dx[p] = this->m_CurrentPosition[offset+p] - this->m_PreviousPosition[offset+p];
      // gradient is already negated
    dg[p] = this->m_PreviousGradient[offset+p] - this->m_Gradient[offset+p];
    }

  edg = this->m_HessianArray[loc] * dx;

  TInternalComputationValueType dot_dg_dx = inner_product(dg, dx);
  TInternalComputationValueType dot_edg_dx = inner_product(edg, dx);

  if (std::abs(dot_dg_dx) <= NumericTraits<TInternalComputationValueType>::epsilon()
      || std::abs(dot_edg_dx) <= NumericTraits<TInternalComputationValueType>::epsilon())
    {
    return false;
    }

  vnl_matrix<TInternalComputationValueType> plus  = outer_product(dg, dg) / dot_dg_dx;
  vnl_matrix<TInternalComputationValueType> minus = outer_product(edg, edg) / dot_edg_dx;
  vnl_matrix<TInternalComputationValueType> newHessian = this->m_HessianArray[loc] + plus - minus;

  this->m_HessianArray[loc] = newHessian;

  for (SizeValueType p=0; p<numLocalPara; p++)
    {
    if (newHessian[p][p] < 0)
      {
      return false;
      }
    }

  TInternalComputationValueType threshold
  = NumericTraits<TInternalComputationValueType>::epsilon();

  if ( std::abs(vnl_determinant(newHessian)) <= threshold )
    {
    return false;
    }
  else
    {
    vnl_matrix<TInternalComputationValueType> hessianInverse = vnl_matrix_inverse<TInternalComputationValueType>(newHessian);
    DerivativeType gradient(numLocalPara);
    DerivativeType newtonStep(numLocalPara);
    for (SizeValueType p=0; p<numLocalPara; p++)
      {
      gradient[p] = this->m_Gradient[offset+p];
      }
      // gradient is already negated
    newtonStep = hessianInverse * gradient;

    for (SizeValueType p=0; p<numLocalPara; p++)
      {
      this->m_NewtonStep[offset+p] = newtonStep[p];
      }

    }

  return true;
}

} // end namespace itk

#endif
