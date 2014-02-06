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
#ifndef __itkVersorTransformOptimizerv4_hxx
#define __itkVersorTransformOptimizerv4_hxx

#include "itkVersorTransformOptimizerv4.h"

namespace itk
{
template<typename TInternalComputationValueType>
void
VersorTransformOptimizerv4Template<TInternalComputationValueType>
::AdvanceOneStep()
{
  itkDebugMacro("AdvanceOneStep");

  /* Modify the gradient by scales once at the begin */
  this->ModifyGradientByScales();

  /* This will estimate the learning rate (m_LearningRate)
   * if the options are set to do so. */
  this->EstimateLearningRate();

  try
    {
    /* Pass graident and learning rate to transform and let it do its own updating */
    this->m_Metric->UpdateTransformParameters( this->m_Gradient, this->m_LearningRate );
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
VersorTransformOptimizerv4Template<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}//namespace itk

#endif
