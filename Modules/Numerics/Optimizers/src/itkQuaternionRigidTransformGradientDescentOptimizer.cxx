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
#ifndef _itkQuaternionRigidTransformGradientDescentOptimizer_hxx
#define _itkQuaternionRigidTransformGradientDescentOptimizer_hxx

#include "itkQuaternionRigidTransformGradientDescentOptimizer.h"
#include "vnl/vnl_quaternion.h"

namespace itk
{
/**
 * Advance one Step following the gradient direction
 */
void
QuaternionRigidTransformGradientDescentOptimizer
::AdvanceOneStep(void)
{
  const double direction = ( m_Maximize ) ? 1.0 : -1.0;
  const ScalesType & scales = this->GetScales();

  const unsigned int spaceDimension =  m_CostFunction->GetNumberOfParameters();

  // Make sure the scales have been set
  if ( scales.size() != spaceDimension )
    {
    itkExceptionMacro(<< "The size of Scales is "
                      << scales.size()
                      << ", but the NumberOfParameters is "
                      << spaceDimension
                      << ".");
    }

  DerivativeType transformedGradient(spaceDimension);
  for ( unsigned int i = 0; i < spaceDimension; i++ )
    {
    transformedGradient[i] = m_Gradient[i] / scales[i];
    }

  ParametersType currentPosition = this->GetCurrentPosition();

  // compute new quaternion value
  vnl_quaternion< double > newQuaternion;
  for ( unsigned int j = 0; j < 4; j++ )
    {
    newQuaternion[j] = currentPosition[j] + direction * m_LearningRate
                       * transformedGradient[j];
    }

  newQuaternion.normalize();

  ParametersType newPosition(spaceDimension);
  // update quaternion component of currentPosition
  for ( unsigned int j = 0; j < 4; j++ )
    {
    newPosition[j] = newQuaternion[j];
    }

  // update the translation component
  for ( unsigned int j = 4; j < spaceDimension; j++ )
    {
    newPosition[j] = currentPosition[j]
                     + direction * m_LearningRate * transformedGradient[j];
    }

  // First invoke the event, so the current position
  // still corresponds to the metric values.
  this->InvokeEvent( IterationEvent() );

  this->SetCurrentPosition(newPosition);
}
} // end namespace itk

#endif
