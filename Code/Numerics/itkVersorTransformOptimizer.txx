/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTransformOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVersorTransformOptimizer_txx
#define _itkVersorTransformOptimizer_txx

#include "itkVersorTransformOptimizer.h"
#include "vnl/vnl_quaternion.h"
#include "itkEventObject.h"

namespace itk
{




/**
 * Advance one Step following the gradient direction
 * This method will be overrided in non-vector spaces
 */
template <class TCostFunction>
void
VersorTransformOptimizer<TCostFunction>
::StepAlongGradient( double factor, 
                     const DerivativeType & transformedGradient )
{ 


  const ParametersType & currentRotation = GetCurrentPosition();

  typename ParametersType::VectorType   axis;

  // The gradient indicate the contribution of each one 
  // of the axis to the direction of highest change in
  // the function
  axis[0] = transformedGradient[0];
  axis[1] = transformedGradient[1];
  axis[2] = transformedGradient[2];

  
  // gradientRotation is a rotation along the 
  // versor direction which maximize the 
  // variation of the cost function in question.
  // An additional Exponentiation produce a jump 
  // of a particular length along the versor gradient 
  // direction.

  ParametersType gradientRotation;
  gradientRotation.Set( axis, factor * axis.GetNorm() );

  //
  // Composing the currentRotation with the gradientRotation 
  // produces the new Rotation versor 
  //
  ParametersType newRotation = currentRotation * gradientRotation;

  this->SetCurrentPosition( newRotation );

}




} // end namespace itk

#endif
