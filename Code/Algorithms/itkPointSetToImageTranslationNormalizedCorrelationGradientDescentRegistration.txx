/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkPointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration_txx
#define _itkPointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration_txx

#include "itkPointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration()
:Superclass( other )
{ 
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration( const Self & other )
:Superclass( other )
{
  m_Parameters       = other.m_Parameters;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration<TReference,  TTarget>
::~PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration< TReference, TTarget> &
PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration< TReference, TTarget>
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
PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration<TReference, TTarget>
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
