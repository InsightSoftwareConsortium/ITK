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
#include "itkGradientDescentObjectOptimizerBase.h"

namespace itk
{

//-------------------------------------------------------------------
GradientDescentObjectOptimizerBase
::GradientDescentObjectOptimizerBase()
{
  this->m_ModifyGradientThreader = ModifyGradientThreaderType::New();
  this->m_ModifyGradientThreader->SetThreadedGenerateData(
                                                Self::ModifyGradientThreaded );
  this->m_ModifyGradientThreader->SetHolder( this );

  this->m_NumberOfIterations = 100;
  this->m_CurrentIteration = 0;
  this->m_StopCondition = MAXIMUM_NUMBER_OF_ITERATIONS;
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";
}

//-------------------------------------------------------------------
GradientDescentObjectOptimizerBase
::~GradientDescentObjectOptimizerBase()
{}

//-------------------------------------------------------------------
void
GradientDescentObjectOptimizerBase
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Modify Gradient Threader: " << this->m_ModifyGradientThreader << std::endl;
  os << indent << "Number of iterations: " << this->m_NumberOfIterations  << std::endl;
  os << indent << "Current iteration: " << this->m_CurrentIteration << std::endl;
  os << indent << "Stop condition:"<< this->m_StopCondition << std::endl;
  os << indent << "Stop condition description: " << this->m_StopConditionDescription.str()  << std::endl;
}


//-------------------------------------------------------------------
const GradientDescentObjectOptimizerBase::StopConditionReturnStringType
GradientDescentObjectOptimizerBase
::GetStopConditionDescription() const
{
  return this->m_StopConditionDescription.str();
}

//-------------------------------------------------------------------
void
GradientDescentObjectOptimizerBase
::StopOptimization(void)
{
  itkDebugMacro("StopOptimization");
  this->m_Stop = true;
  this->InvokeEvent( EndEvent() );
}

//-------------------------------------------------------------------
void
GradientDescentObjectOptimizerBase
::ModifyGradient()
{
  IndexRangeType fullrange;
  fullrange[0] = 0;
  fullrange[1] = this->m_Gradient.GetSize()-1; //range is inclusive
  /* Perform the modification either with or without threading */
  if( this->m_Metric->HasLocalSupport() )
    {
    this->m_ModifyGradientThreader->SetOverallIndexRange( fullrange );
    /* This ends up calling ModifyGradientThreaded from each thread */
    this->m_ModifyGradientThreader->StartThreadedExecution();
    }
  else
    {
    /* Global transforms are small, so update without threading. */
    this->ModifyGradientOverSubRange( fullrange );
    }
}

//-------------------------------------------------------------------
void
GradientDescentObjectOptimizerBase
::ModifyGradientThreaded( const IndexRangeType& rangeForThread,
                          ThreadIdType,
                          Self *holder )
{
  holder->ModifyGradientOverSubRange( rangeForThread );
}

}//namespace itk
