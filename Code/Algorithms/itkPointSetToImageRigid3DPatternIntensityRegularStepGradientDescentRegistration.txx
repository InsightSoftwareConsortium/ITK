/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration_txx
#define _itkPointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration_txx

#include "itkPointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration()
{ 
}


/**
 * Destructor
 */
template <class TReference, class TTarget>
PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration<TReference,  TTarget>
::~PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration()
{
}

/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
void
PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{ 
  /* Initialize the Offset */ 
  for (unsigned int k=0; k<ParametersDimension; k++)
  { 
    m_Parameters[ k ] = 0;
  }

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  optimizer->SetCostFunction( this->GetMetric() );
  optimizer->SetInitialPosition( m_Parameters );
  
  optimizer->StartOptimization();


}



} // end namespace itk


#endif
