/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationPatternIntensityGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkPointSetToImageTranslationPatternIntensityGradientDescentRegistration_txx
#define _itkPointSetToImageTranslationPatternIntensityGradientDescentRegistration_txx

#include "itkPointSetToImageTranslationPatternIntensityGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationPatternIntensityGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationPatternIntensityGradientDescentRegistration()
{ 
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationPatternIntensityGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationPatternIntensityGradientDescentRegistration( const Self & other )
:Superclass( other )
{
  m_Parameters       = other.m_Parameters;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationPatternIntensityGradientDescentRegistration<TReference,  TTarget>
::~PointSetToImageTranslationPatternIntensityGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const PointSetToImageTranslationPatternIntensityGradientDescentRegistration< TReference, TTarget> &
PointSetToImageTranslationPatternIntensityGradientDescentRegistration< TReference, TTarget>
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
PointSetToImageTranslationPatternIntensityGradientDescentRegistration<TReference, TTarget>
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
