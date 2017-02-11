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
#ifndef itkGradientDescentLineSearchOptimizerv4_hxx
#define itkGradientDescentLineSearchOptimizerv4_hxx

#include "itkGradientDescentLineSearchOptimizerv4.h"

namespace itk
{

/**
 * Default constructor
 */
template<typename TInternalComputationValueType>
GradientDescentLineSearchOptimizerv4Template<TInternalComputationValueType>
::GradientDescentLineSearchOptimizerv4Template()
{
  this->m_MaximumLineSearchIterations = 20;
  this->m_LineSearchIterations = NumericTraits<unsigned int>::ZeroValue();
  this->m_LowerLimit = itk::NumericTraits< TInternalComputationValueType >::ZeroValue();
  this->m_UpperLimit = 5.0;
  this->m_Phi = 1.618034;
  this->m_Resphi = 2 - this->m_Phi;
  this->m_Epsilon = 0.01;
  this->m_ReturnBestParametersAndValue = true;
}

/**
* Destructor
*/
template<typename TInternalComputationValueType>
GradientDescentLineSearchOptimizerv4Template<TInternalComputationValueType>
::~GradientDescentLineSearchOptimizerv4Template()
{}


/**
*PrintSelf
*/
template<typename TInternalComputationValueType>
void
GradientDescentLineSearchOptimizerv4Template<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  }

/**
* Advance one Step following the gradient direction
*/
template<typename TInternalComputationValueType>
void
GradientDescentLineSearchOptimizerv4Template<TInternalComputationValueType>
::AdvanceOneStep()
{
  itkDebugMacro("AdvanceOneStep");

  /* Modify the gradient by scales once at the begin */
  this->ModifyGradientByScales();

  /* This will estimate the learning rate (m_LearningRate)
   * if the options are set to do so. We only ever want to
   * estimate at the first step for this class. */
  if ( this->m_CurrentIteration == 0 )
    {
    this->EstimateLearningRate();
    }

  this->m_LineSearchIterations = 0;
  this->m_LearningRate = this->GoldenSectionSearch( this->m_LearningRate * this->m_LowerLimit ,
                                                   this->m_LearningRate , this->m_LearningRate * this->m_UpperLimit );

  /* Begin threaded gradient modification of m_Gradient variable. */
  this->ModifyGradientByLearningRate();

  try
    {
    /* Pass gradient to transform and let it do its own updating */
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


// a and c are the current bounds; the minimum is between them.
// b is a center point
// f(x) is some mathematical function elsewhere defined
// a corresponds to x1; b corresponds to x2; c corresponds to x3
// x corresponds to x4
template<typename TInternalComputationValueType>
TInternalComputationValueType
GradientDescentLineSearchOptimizerv4Template<TInternalComputationValueType>
::GoldenSectionSearch( TInternalComputationValueType a, TInternalComputationValueType b, TInternalComputationValueType c )
{
  if ( this->m_LineSearchIterations > this->m_MaximumLineSearchIterations )
    {
    return ( c + a ) / 2;
    }
  this->m_LineSearchIterations++;

  TInternalComputationValueType x;
  if ( c - b > b - a )
    {
    x = b + this->m_Resphi * ( c - b );
    }
  else
    {
    x = b - this->m_Resphi * ( b - a );
    }
  if ( std::abs( c - a ) < this->m_Epsilon * ( std::abs( b ) + std::abs( x ) ) )
    {
    return ( c + a ) / 2;
    }

  TInternalComputationValueType metricx, metricb;

    {
      // Cache the learning rate , parameters , gradient
      // Contain this in a block so these variables go out of
      // scope before we call recursively below. With dense transforms
      // we would otherwise eat up a lot of memory unnecessarily.
    TInternalComputationValueType baseLearningRate = this->m_LearningRate;
    DerivativeType baseGradient( this->m_Gradient );
    ParametersType baseParameters( this->GetCurrentPosition() );

    this->m_LearningRate = x;
    this->ModifyGradientByLearningRate();
    this->m_Metric->UpdateTransformParameters( this->m_Gradient );
    metricx = this->GetMetric()->GetValue( );

    /** reset position of transform and gradient */
    this->m_Metric->SetParameters( baseParameters );
    this->m_Gradient = baseGradient;

    this->m_LearningRate = b;
    this->ModifyGradientByLearningRate();

    this->m_Metric->UpdateTransformParameters( this->m_Gradient );
    metricb = this->GetMetric()->GetValue( );

    /** reset position of transform and learning rate */
    this->m_Metric->SetParameters( baseParameters );
    this->m_Gradient = baseGradient;
    this->m_LearningRate = baseLearningRate;
    }

  /** golden section */
  if (  metricx < metricb )
    {
    if (c - b > b - a)
      {
      return this->GoldenSectionSearch( b, x, c );
      }
    else
      {
      return this->GoldenSectionSearch( a, x, b );
      }
    }
  else
    {
    if ( c - b > b - a )
      {
      return this->GoldenSectionSearch( a, b, x );
      }
    else
      {
      return this->GoldenSectionSearch( x, b, c  );
      }
    }
  }

}//namespace itk

#endif
