/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkPointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration_txx
#define _itkPointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration_txx

#include "itkPointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration()
{ 
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration( const Self & other )
:Superclass( other )
{
  m_Parameters       = other.m_Parameters;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
PointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration<TReference,  TTarget>
::~PointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const PointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration< TReference, TTarget> &
PointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration< TReference, TTarget>
::operator=( const Self & other )
{
  Superclass::operator=( other );
  m_Parameters       = other.m_Parameters;
  return *this;
}



/**
 * Prepare the Registration Process
 */
template <class TReference, class TTarget>
void
PointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::PrepareRegistration( void )
{ 
  /* Initialize the Offset */ 
  for (unsigned int k=0; k<ParametersDimension; k++)
  { 
    m_Parameters[ k ] = 0;
  }

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  typename OptimizerType::TransformType::ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );

  optimizer->SetCostFunction( this->GetMetric() );
  optimizer->SetMinimize();
  optimizer->GetTransform()->SetScale( parametersScale );
  optimizer->SetGradientMagnitudeTolerance( 1e-6 );
  optimizer->SetMaximumStepLength( 30.0 );
  optimizer->SetMinimumStepLength( 1e-6 );
  optimizer->SetNumberOfIterations( 900 );

  optimizer->SetInitialPosition( m_Parameters );
  

}


/**
 * Prepare the Registration Process
 */
template <class TReference, class TTarget>
void
PointSetToImageRigid3DPerspectivePatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{ 

  optimizer->StartOptimization();


}


} // end namespace itk


#endif
