/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkPointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration_txx
#define _itkPointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration_txx

#include "itkPointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration()
{ 
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration( const Self & other )
:Superclass( other )
{
  m_Parameters       = other.m_Parameters;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration<TReference,  TTarget>
::~PointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const PointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration< TReference, TTarget> &
PointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration< TReference, TTarget>
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
int
PointSetToImageTranslationMeanSquaresRegularStepGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{ 
  /* Initialize the Offset */ 
  for (unsigned int k=0; k<ParametersDimension; k++)
  { 
    m_Parameters[ k ] = 0;
  }

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransformation();

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
  optimizer->SetMaximumNumberOfIterations( 900 );

  optimizer->SetInitialPosition( m_Parameters );
  optimizer->StartOptimization();

  std::cout << "The Solution is : " ;
  m_Parameters = optimizer->GetCurrentPosition();
  std::cout << m_Parameters << std::endl;
  std::cout << std::endl;

return 0;
}



} // end namespace itk


#endif
