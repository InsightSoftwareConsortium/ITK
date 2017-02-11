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
#ifndef itkConjugateGradientLineSearchOptimizerv4_hxx
#define itkConjugateGradientLineSearchOptimizerv4_hxx

#include "itkConjugateGradientLineSearchOptimizerv4.h"

namespace itk
{

/**
 * Default constructor
 */
template<typename TInternalComputationValueType>
ConjugateGradientLineSearchOptimizerv4Template<TInternalComputationValueType>
::ConjugateGradientLineSearchOptimizerv4Template()
{
}

/**
 * Destructor
 */
template<typename TInternalComputationValueType>
ConjugateGradientLineSearchOptimizerv4Template<TInternalComputationValueType>
::~ConjugateGradientLineSearchOptimizerv4Template()
{}


/**
 *PrintSelf
 */
template<typename TInternalComputationValueType>
void
ConjugateGradientLineSearchOptimizerv4Template<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template<typename TInternalComputationValueType>
void
ConjugateGradientLineSearchOptimizerv4Template<TInternalComputationValueType>
::StartOptimization( bool doOnlyInitialization)
{
  this->m_ConjugateGradient.SetSize( this->m_Metric->GetNumberOfParameters() );
  this->m_ConjugateGradient.Fill( itk::NumericTraits< TInternalComputationValueType >::ZeroValue() );
  this->m_LastGradient.SetSize( this->m_Metric->GetNumberOfParameters() );
  this->m_LastGradient.Fill( itk::NumericTraits< TInternalComputationValueType >::ZeroValue() );
  Superclass::StartOptimization( doOnlyInitialization );
}

/**
* Advance one Step following the gradient direction
*/
template<typename TInternalComputationValueType>
void
ConjugateGradientLineSearchOptimizerv4Template<TInternalComputationValueType>
::AdvanceOneStep()
{
  itkDebugMacro("AdvanceOneStep");

  this->ModifyGradientByScales();
  if ( this->m_CurrentIteration == 0 )
    {
    this->EstimateLearningRate();
    }

  TInternalComputationValueType gamma = itk::NumericTraits< TInternalComputationValueType >::ZeroValue();
  TInternalComputationValueType gammaDenom = inner_product( this->m_LastGradient , this->m_LastGradient );
  if ( gammaDenom > itk::NumericTraits< TInternalComputationValueType >::epsilon() )
    {
    gamma = inner_product( this->m_Gradient - this->m_LastGradient , this->m_Gradient ) / gammaDenom;
    }

  /** Modified Polak-Ribiere restart conditions */
  if ( gamma < 0 || gamma > 5 )
    {
    gamma = 0;
    }
  this->m_LastGradient = this->m_Gradient;
  this->m_ConjugateGradient = this->m_Gradient + this->m_ConjugateGradient * gamma;
  this->m_Gradient = this->m_ConjugateGradient;

  /* Estimate a learning rate for this step */
  this->m_LineSearchIterations = 0;
  this->m_LearningRate = this->GoldenSectionSearch( this->m_LearningRate * this->m_LowerLimit ,
                                                   this->m_LearningRate , this->m_LearningRate * this->m_UpperLimit  );

  /* Begin threaded gradient modification of m_Gradient variable. */
  this->ModifyGradientByLearningRate();

  try
    {
    /* Pass gradient to transform and let it do its own updating. */
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

}//namespace itk

#endif
