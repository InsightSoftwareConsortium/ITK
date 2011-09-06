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

#include "itkGradientDescentObjectOptimizer.h"

namespace itk
{

/**
 * Default constructor
 */
GradientDescentObjectOptimizer
::GradientDescentObjectOptimizer()
{
  this->m_LearningRate = 1.0;
}

/**
 * Destructor
 */
GradientDescentObjectOptimizer
::~GradientDescentObjectOptimizer()
{}


/**
 *PrintSelf
 */
void
GradientDescentObjectOptimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
 Superclass::PrintSelf(os, indent);
 os << indent << "Learning rate:" << this->m_LearningRate << std::endl;
}




/**
 * Start and run the optimization
 */
void
GradientDescentObjectOptimizer
::StartOptimization()
{
  itkDebugMacro("StartOptimization");

  /* Must call the superclass version for basic validation and setup */
  Superclass::StartOptimization();

  this->m_CurrentIteration = 0;

  this->ResumeOptimization();
}

/**
 * Resume optimization.
 */
void
GradientDescentObjectOptimizer
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
      this->m_Metric->GetValueAndDerivative( this->m_Value, this->m_Gradient );
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

    /* Advance one step along the gradient.
     * This will modify the gradient and update the transform. */
    this->AdvanceOneStep();

    /* Update and check iteration count */
    this->m_CurrentIteration++;
    if ( this->m_CurrentIteration >= this->m_NumberOfIterations )
      {
      this->m_StopConditionDescription << "Maximum number of iterations ("
                                 << this->m_NumberOfIterations
                                 << ") exceeded.";
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
GradientDescentObjectOptimizer
::AdvanceOneStep()
{
  itkDebugMacro("AdvanceOneStep");

  /* Begin threaded gradient modification. Work is done in
   * ModifyGradientOverSubRange */
  this->ModifyGradient();

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
 * Modify the gradient over a given index range.
 */
void
GradientDescentObjectOptimizer
::ModifyGradientOverSubRange( const IndexRangeType& subrange )
{
  const ScalesType& scales = this->GetScales();

  /* Loop over the range. It is inclusive. */
  for ( IndexValueType j = subrange[0]; j <= subrange[1]; j++ )
    {
    if( scales.GetSize() == 0 )
      {
      this->m_Gradient[j] = this->m_Gradient[j] * this->m_LearningRate;
      }
    else
      {
      // scales is checked during StartOptmization for values <=
      // machine epsilon.
      this->m_Gradient[j] =
                this->m_Gradient[j] / scales[j] * this->m_LearningRate;
      }
    }
}

}//namespace itk
