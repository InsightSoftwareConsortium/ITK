/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTransformOptimizer.cxx
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
void
VersorTransformOptimizer
::StepAlongGradient( double factor, 
                     const DerivativeType & transformedGradient )
{ 


  const ParametersType & currentPosition = GetCurrentPosition();

  // The parameters are assumed to be the right part of the versor
  // 
  VectorType rightPart;
  for(unsigned int i=0; i<SpaceDimension; i++)
    {
    rightPart[i] = currentPosition[i];
    }

  VersorType currentRotation;
  currentRotation.Set( rightPart );

  std::cout << "Current rotation = " << currentRotation << std::endl;

  // The gradient indicate the contribution of each one 
  // of the axis to the direction of highest change in
  // the function
  VectorType   axis;
  axis[0] = transformedGradient[0];
  axis[1] = transformedGradient[1];
  axis[2] = transformedGradient[2];

  
  // gradientRotation is a rotation along the 
  // versor direction which maximize the 
  // variation of the cost function in question.
  // An additional Exponentiation produce a jump 
  // of a particular length along the versor gradient 
  // direction.
  std::cout << "factor =  " << factor << std::endl;
  std::cout << "axis.Norm =  " << axis.GetNorm() << std::endl;

  VersorType gradientRotation;
  gradientRotation.Set( axis, factor * axis.GetNorm() );

  std::cout << "Gradient rotaion = " << gradientRotation << std::endl;

  //
  // Composing the currentRotation with the gradientRotation 
  // produces the new Rotation versor 
  //
  VersorType newRotation = currentRotation * gradientRotation;

  ParametersType newParameters(SpaceDimension);

  std::cout << "New rotaion = " << newRotation << std::endl;

  newParameters[0] = newRotation.GetX();
  newParameters[1] = newRotation.GetY();
  newParameters[2] = newRotation.GetZ();

  this->SetCurrentPosition( newParameters );

}




} // end namespace itk

#endif
