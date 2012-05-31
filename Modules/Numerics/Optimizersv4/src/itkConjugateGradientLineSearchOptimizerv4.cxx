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

#include "itkConjugateGradientLineSearchOptimizerv4.h"

namespace itk
{

/**
 * Default constructor
 */
ConjugateGradientLineSearchOptimizerv4
::ConjugateGradientLineSearchOptimizerv4()
{
  this->m_CurrentBestValue = NumericTraits< MeasureType >::max();
}

/**
 * Destructor
 */
ConjugateGradientLineSearchOptimizerv4
::~ConjugateGradientLineSearchOptimizerv4()
{}


/**
 *PrintSelf
 */
void
ConjugateGradientLineSearchOptimizerv4
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

//-------------------------------------------------------------------
void
ConjugateGradientLineSearchOptimizerv4
::StopOptimization(void)
{
  this->GetMetric()->SetParameters( this->m_BestParameters );
  Superclass::StopOptimization();
}


/**
 * Advance one Step following the gradient direction
 */
void
ConjugateGradientLineSearchOptimizerv4
::AdvanceOneStep()
{
  itkDebugMacro("AdvanceOneStep");

  /* This will estimate the learning rate (m_LearningRate)
   * if the options are set to do so. We only ever want to
   * estimate at the first step for this class. */
  if ( this->m_CurrentIteration == 0 )
    {
    this->m_BestParameters = this->GetCurrentPosition( );
    this->m_CurrentBestValue = NumericTraits< MeasureType >::max();
    DerivativeType baseGradient( this->m_Gradient );
    this->ModifyGradientByScales();
    this->EstimateLearningRate();
    this->m_Gradient = baseGradient;
    this->m_ConjugateGradient.SetSize( this->m_Gradient.GetSize( ) );
    this->m_ConjugateGradient.Fill( itk::NumericTraits< InternalComputationValueType >::Zero );
    }

  InternalComputationValueType gamma = itk::NumericTraits< InternalComputationValueType >::Zero;
  InternalComputationValueType gammaDenom = inner_product( this->m_LastGradient , this->m_LastGradient );
  if ( gammaDenom > itk::NumericTraits< InternalComputationValueType >::epsilon() )
    {
    gamma = inner_product( this->m_Gradient , this->m_Gradient ) / gammaDenom;
    }
  this->m_LastGradient = this->m_Gradient;
  this->m_ConjugateGradient = this->m_Gradient + this->m_ConjugateGradient * gamma;
  if ( this->m_ConjugateGradient.two_norm() > ( this->m_Gradient.two_norm( ) * 5 ) )
    {
    this->m_ConjugateGradient = this->m_Gradient;
    }
  else
    {
    this->m_Gradient = this->m_ConjugateGradient;
    }

  /* Cache the learning rate so we can optionally restore it later */
  InternalComputationValueType baseLearningRate = this->m_LearningRate;
  this->ModifyGradientByScales();

  /* Estimate a learning rate for this step */
  this->m_LearningRate = this->GoldenSectionSearch( this->m_LearningRate * this->m_LowerLimit ,
    this->m_LearningRate , this->m_LearningRate * this->m_UpperLimit  );

  /* Begin threaded gradient modification of m_Gradient variable. */
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

  /** reset to base learning rate if set to do so */
  if( this->m_SearchMethod == SearchNearBaselineLearningRate )
    {
    this->m_LearningRate = baseLearningRate;
    }
  if ( this->m_Value < this->m_CurrentBestValue )
    {
    this->m_CurrentBestValue = this->m_Value;
    this->m_BestParameters = this->GetCurrentPosition( );
    }
  else if ( ( this->m_Value - this->m_CurrentBestValue ) > itk::NumericTraits< InternalComputationValueType >::epsilon() )
    {
    this->m_LastGradient.Fill( itk::NumericTraits< InternalComputationValueType >::Zero );
    }
  this->InvokeEvent( IterationEvent() );
}


}//namespace itk
