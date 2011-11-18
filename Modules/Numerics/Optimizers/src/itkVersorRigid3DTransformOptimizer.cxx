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
#ifndef _itkVersorRigid3DTransformOptimizer_hxx
#define _itkVersorRigid3DTransformOptimizer_hxx

#include "itkVersorRigid3DTransformOptimizer.h"

namespace itk
{
/**
 * Advance one Step following the gradient direction
 * This method will be overrided in non-vector spaces
 */
void
VersorRigid3DTransformOptimizer
::StepAlongGradient(double factor,
                    const DerivativeType & transformedGradient)
{
  const ParametersType & currentPosition = this->GetCurrentPosition();

  // The parameters are assumed to be the right part of the versor and the
  // components of the translation vector.
  //
  VectorType rightPart;

  for ( unsigned int i = 0; i < 3; i++ )
    {
    rightPart[i] = currentPosition[i];
    }

  VersorType currentRotation;
  currentRotation.Set(rightPart);

  // The gradient indicate the contribution of each one
  // of the axis to the direction of highest change in
  // the function
  VectorType axis;
  axis[0] = transformedGradient[0];
  axis[1] = transformedGradient[1];
  axis[2] = transformedGradient[2];

  // gradientRotation is a rotation along the
  // versor direction which maximize the
  // variation of the cost function in question.
  // An additional Exponentiation produce a jump
  // of a particular length along the versor gradient
  // direction.

  VersorType gradientRotation;
  gradientRotation.Set( axis, factor * axis.GetNorm() );

  //
  // Composing the currentRotation with the gradientRotation
  // produces the new Rotation versor
  //
  VersorType newRotation = currentRotation * gradientRotation;

  ParametersType newParameters(SpaceDimension);

  newParameters[0] = newRotation.GetX();
  newParameters[1] = newRotation.GetY();
  newParameters[2] = newRotation.GetZ();

  // Now do the typical update for a Vector space.
  for ( unsigned int k = 3; k < 6; k++ )
    {
    newParameters[k] = currentPosition[k] + transformedGradient[k] * factor;
    }

  this->SetCurrentPosition(newParameters);
}
} // end namespace itk

#endif
