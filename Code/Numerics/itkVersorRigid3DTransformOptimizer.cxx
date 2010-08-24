/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorRigid3DTransformOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVersorRigid3DTransformOptimizer_txx
#define _itkVersorRigid3DTransformOptimizer_txx

#include "itkVersorRigid3DTransformOptimizer.h"
#include "vnl/vnl_quaternion.h"
#include "itkEventObject.h"

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
