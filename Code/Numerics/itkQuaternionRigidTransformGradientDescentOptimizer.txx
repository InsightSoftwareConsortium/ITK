/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuaternionRigidTransformGradientDescentOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
 * Constructor
 */
template <class TCostFunction>
QuaternionRigidTransformGradientDescentOptimizer<TCostFunction>
::QuaternionRigidTransformGradientDescentOptimizer()
{
}


template <class TCostFunction>
void
QuaternionRigidTransformGradientDescentOptimizer<TCostFunction>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


/**
 * Advance one Step following the gradient direction
 */
template <class TCostFunction>
void
QuaternionRigidTransformGradientDescentOptimizer<TCostFunction>
::AdvanceOneStep( void )
{ 

  double direction = 1.0;
  if( m_Maximize ) 
  {
    direction = 1.0;
  }
  else 
  {
    direction = -1.0;
  }

  ParametersType newPosition;
  const ParametersType & currentPosition = GetCurrentPosition();
  DerivativeType transformedGradient = 
            GetTransform()->TransformCovariantVector( m_Gradient );

  // compute new quaternion value
  vnl_quaternion<double> newQuaternion;
  for( unsigned int j=0; j < 4; j++ )
    {
    newQuaternion[j] = currentPosition[j] + direction * m_LearningRate *
      transformedGradient[j];
    }

  newQuaternion.normalize();

  // update quaternion component of currentPosition
  for( unsigned int j=0; j < 4; j++ )
    {
    newPosition[j] = newQuaternion[j];
    }
  
  // update the translation component
  for(unsigned int j=4; j<SpaceDimension; j++)
  {
    newPosition[j] = currentPosition[j] + 
      direction * m_LearningRate * transformedGradient[j];
  }

  SetCurrentPosition( newPosition );

  InvokeEvent( IterationEvent() );

}



} // end namespace itk

#endif
