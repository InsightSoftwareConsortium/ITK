/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkPointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration_txx
#define _itkPointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration_txx

#include "itkPointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration()
:Superclass( other )
{ 
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration( const Self & other )
:Superclass( other )
{
  m_Parameters       = other.m_Parameters;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration<TReference,  TTarget>
::~PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration< TReference, TTarget> &
PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration< TReference, TTarget>
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
PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{ 
  /* Initialize the Offset */ 
  for (unsigned int k=0; k<ParametersDimension; k++)
  { 
    m_Parameters[ k ] = 0;
  }

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransformation();


  ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );
  m_Optimizer->SetMinimize();
  m_Optimizer->SetScale( parametersScale );
  m_Optimizer->SetGradientMagnitudeTolerance( 1e-6 );
  m_Optimizer->SetMaximumStepLength( 30.0 );
  m_Optimizer->SetMinimumStepLength( 1e-6 );
  m_Optimizer->SetMaximumNumberOfIterations( 900 );

  m_Optimizer->SetInitialPosition( m_Parameters );
  m_Optimizer->StartOptimization();

  std::cout << "The Solution is : " ;
  m_Parameters = m_Optimizer->GetCurrentPosition();
  std::cout << m_Parameters << std::endl;
  std::cout << std::endl;

return 0;
}



} // end namespace itk


#endif
