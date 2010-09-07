/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuaternionRigidTransformGradientDescentOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkQuaternionRigidTransformGradientDescentOptimizer_txx
#define _itkQuaternionRigidTransformGradientDescentOptimizer_txx

#include "itkQuaternionRigidTransformGradientDescentOptimizer.h"
#include "vnl/vnl_quaternion.h"
#include "itkEventObject.h"

namespace itk
{
/**
 * Advance one Step following the gradient direction
 */
void
QuaternionRigidTransformGradientDescentOptimizer
::AdvanceOneStep(void)
{
  double direction;

  if ( m_Maximize )
    {
    direction = 1.0;
    }
  else
    {
    direction = -1.0;
    }

  ScalesType scales = this->GetScales();

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
