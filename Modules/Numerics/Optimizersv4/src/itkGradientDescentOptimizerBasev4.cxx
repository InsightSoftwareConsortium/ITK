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

#include "itkGradientDescentOptimizerBasev4.h"

namespace itk
{

//-------------------------------------------------------------------
GradientDescentOptimizerBasev4
::GradientDescentOptimizerBasev4()
{
  /** Threader for apply scales to gradient */
  this->m_ModifyGradientByScalesThreader = GradientDescentOptimizerBasev4ModifyGradientByScalesThreader::New();

  /** Threader for apply the learning rate to gradient */
  this->m_ModifyGradientByLearningRateThreader = GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader::New();

  this->m_NumberOfIterations = 100;
  this->m_CurrentIteration   = 0;
  this->m_StopCondition      = MAXIMUM_NUMBER_OF_ITERATIONS;
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";
}

//-------------------------------------------------------------------
GradientDescentOptimizerBasev4
::~GradientDescentOptimizerBasev4()
{}

//-------------------------------------------------------------------
void
GradientDescentOptimizerBasev4
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number of iterations: " << this->m_NumberOfIterations  << std::endl;
  os << indent << "Current iteration: " << this->m_CurrentIteration << std::endl;
  os << indent << "Stop condition:"<< this->m_StopCondition << std::endl;
  os << indent << "Stop condition description: " << this->m_StopConditionDescription.str()  << std::endl;
}


//-------------------------------------------------------------------
const GradientDescentOptimizerBasev4::StopConditionReturnStringType
GradientDescentOptimizerBasev4
::GetStopConditionDescription() const
{
  return this->m_StopConditionDescription.str();
}

//-------------------------------------------------------------------
void
GradientDescentOptimizerBasev4
::StopOptimization(void)
{
  itkDebugMacro( "StopOptimization called with a description - "
    << this->GetStopConditionDescription() );
  this->m_Stop = true;
  this->InvokeEvent( EndEvent() );
}

//-------------------------------------------------------------------
void
GradientDescentOptimizerBasev4
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
void
GradientDescentOptimizerBasev4
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
