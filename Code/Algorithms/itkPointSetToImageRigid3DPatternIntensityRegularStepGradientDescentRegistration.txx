/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration( const Self & other )
:Superclass( other )
{
  m_Parameters       = other.m_Parameters;
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
 * Assignment Operator
 */
template <class TReference, class TTarget>
const PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration< TReference, TTarget> &
PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration< TReference, TTarget>
::operator=( const Self & other )
{
  Superclass::operator=( other );
  m_Parameters       = other.m_Parameters;
  return *this;
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

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransform();

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );

  optimizer->SetCostFunction( this->GetMetric() );
  optimizer->SetMinimize();
  optimizer->SetScale( parametersScale );
  optimizer->SetGradientMagnitudeTolerance( 1e-6 );
  optimizer->SetMaximumStepLength( 30.0 );
  optimizer->SetMinimumStepLength( 1e-6 );
  optimizer->SetNumberOfIterations( 900 );

  optimizer->SetInitialPosition( m_Parameters );
  optimizer->StartOptimization();

  std::cout << "The Solution is : " ;
  m_Parameters = optimizer->GetCurrentPosition();
  std::cout << m_Parameters << std::endl;
  std::cout << std::endl;


}



} // end namespace itk


#endif
