/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationNormalizedCorrelationRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkPointSetToImageTranslationNormalizedCorrelationRegistration_txx
#define _itkPointSetToImageTranslationNormalizedCorrelationRegistration_txx

#include "itkPointSetToImageTranslationNormalizedCorrelationRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationRegistration<TReference, TTarget>
::PointSetToImageTranslationNormalizedCorrelationRegistration()
{ 
  m_Metric = MetricType::New();
  m_Mapper = MapperType::New(); 
  m_Optimizer = OptimizerType::New();
  m_Transformation = TransformationType::New();
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationRegistration<TReference, TTarget>
::PointSetToImageTranslationNormalizedCorrelationRegistration( const Self & other )
{
  m_Reference       =   other.m_Reference;
  m_Target          =   other.m_Target;
  m_Transformation  =   other.m_Transformation;
  m_Metric          =   other.m_Metric;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationRegistration<TReference,  TTarget>
::~PointSetToImageTranslationNormalizedCorrelationRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const PointSetToImageTranslationNormalizedCorrelationRegistration< TReference, TTarget> &
PointSetToImageTranslationNormalizedCorrelationRegistration< TReference, TTarget>
::operator=( const Self & other )
{
  m_Reference       =   other.m_Reference;
  m_Target          =   other.m_Target;
  m_Transformation  =   other.m_Transformation;
  m_Metric          =   other.m_Metric;
  return *this;
}


/**
 * Set Reference 
 */


template <class TReference, class TTarget>
void
PointSetToImageTranslationNormalizedCorrelationRegistration<TReference, TTarget>
::SetReference( ReferenceType * reference )
{
  m_Reference       =   reference;
  m_Mapper->SetDomain( m_Reference );
}


/**
 * Set Target 
 */
template <class TReference, class TTarget>
void
PointSetToImageTranslationNormalizedCorrelationRegistration< TReference, TTarget>
::SetTarget( TargetType * target )
{
  m_Target       =   target;
  m_Metric->SetTarget( m_Target );
}


/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
int
PointSetToImageTranslationNormalizedCorrelationRegistration<TReference, TTarget>
::StartRegistration( void )
{ 
  /* Initialize the Offset */ 
  for (unsigned int k=0; k<ParametersDimension; k++)
  { 
    m_Parameters[ k ] = 0;
  }

  m_Mapper->SetTransformation(m_Transformation);
  m_Metric->SetMapper(m_Mapper);
  m_Optimizer->SetCostFunction( m_Metric );


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
