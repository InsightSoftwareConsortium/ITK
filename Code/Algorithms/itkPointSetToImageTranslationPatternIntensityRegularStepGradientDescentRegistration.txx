/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration_txx
#define _itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration_txx

#include "itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration()
{ 
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration( const Self & other )
:Superclass( other )
{
  m_Parameters       = other.m_Parameters;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration<TReference,  TTarget>
::~PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration< TReference, TTarget> &
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration< TReference, TTarget>
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
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
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
