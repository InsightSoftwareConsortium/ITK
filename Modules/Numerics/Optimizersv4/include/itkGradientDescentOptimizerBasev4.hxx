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
#ifndef __itkGradientDescentOptimizerBasev4_hxx
#define __itkGradientDescentOptimizerBasev4_hxx

#include "itkGradientDescentOptimizerBasev4.h"
#include "itkGradientDescentOptimizerBasev4ModifyGradientByScalesThreader.h"
#include "itkGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader.h"

namespace itk
{

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::GradientDescentOptimizerBasev4Template():
  m_Stop(false)
{
  /** Threader for apply scales to gradient */
  typename GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate<TInternalComputationValueType>::Pointer modifyGradientByScalesThreader =
    GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate<TInternalComputationValueType>::New();
  this->m_ModifyGradientByScalesThreader = modifyGradientByScalesThreader;

  /** Threader for apply the learning rate to gradient */
  typename GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate<TInternalComputationValueType>::Pointer modifyGradientByLearningRateThreader =
    GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate<TInternalComputationValueType>::New();
  this->m_ModifyGradientByLearningRateThreader = modifyGradientByLearningRateThreader;

  this->m_NumberOfIterations = 100;
  this->m_StopCondition      = MAXIMUM_NUMBER_OF_ITERATIONS;
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";
}

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::~GradientDescentOptimizerBasev4Template()
{}

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
void
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number of iterations: " << this->m_NumberOfIterations  << std::endl;
  os << indent << "Stop condition:"<< this->m_StopCondition << std::endl;
  os << indent << "Stop condition description: " << this->m_StopConditionDescription.str()  << std::endl;
}


//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
const typename GradientDescentOptimizerBasev4Template<TInternalComputationValueType>::StopConditionReturnStringType
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::GetStopConditionDescription() const
{
  return this->m_StopConditionDescription.str();
}

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
void
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::StopOptimization(void)
{
  itkDebugMacro( "StopOptimization called with a description - "
    << this->GetStopConditionDescription() );
  this->m_Stop = true;
  this->InvokeEvent( EndEvent() );
}

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
void
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::ModifyGradientByScales()
{
  if ( this->GetScalesAreIdentity() && this->GetWeightsAreIdentity() )
    {
    return;
    }

  IndexRangeType fullrange;
  fullrange[0] = 0;
  fullrange[1] = this->m_Gradient.GetSize()-1; //range is inclusive
  /* Perform the modification either with or without threading */

  if( this->m_Metric->HasLocalSupport() )
    {
    // Inheriting classes should instantiate and assign m_ModifyGradientByScalesThreader
    // in their constructor.
    itkAssertInDebugAndIgnoreInReleaseMacro( !m_ModifyGradientByScalesThreader.IsNull() );

    this->m_ModifyGradientByScalesThreader->Execute( this, fullrange );
    }
  else
    {
    /* Global transforms are small, so update without threading. */
    this->ModifyGradientByScalesOverSubRange( fullrange );
    }
}

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
void
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::ModifyGradientByLearningRate()
{
  IndexRangeType fullrange;
  fullrange[0] = 0;
  fullrange[1] = this->m_Gradient.GetSize()-1; //range is inclusive
  /* Perform the modification either with or without threading */
  if( this->m_Metric->HasLocalSupport() )
    {
    // Inheriting classes should instantiate and assign m_ModifyGradientByLearningRateThreader
    // in their constructor.
    itkAssertInDebugAndIgnoreInReleaseMacro( !m_ModifyGradientByLearningRateThreader.IsNull() );
    /* Add a check for m_LearningRateIsIdentity?
       But m_LearningRate is not assessible here.
       Should we declare it in a base class as m_Scales ? */

    this->m_ModifyGradientByLearningRateThreader->Execute( this, fullrange );
    }
  else
    {
    /* Global transforms are small, so update without threading. */
    this->ModifyGradientByLearningRateOverSubRange( fullrange );
    }
}

} //namespace itk

#endif
