/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include "itkRegistrationTransform.h"

namespace itk
{

/**
 * Constructor
 */
template <class TMetric, class TOptimizationMethod>
RegistrationTransform<TMetric, TOptimizationMethod>
::RegistrationTransform()
{
  m_Metric = MetricType::New();
}


/**
 * Constructor
 */
template <class TMetric, class TOptimizationMethod>
RegistrationTransform<TMetric, TOptimizationMethod>
::RegistrationTransform( const Self & other )
{
  m_Reference       =   other.m_Reference;
  m_Target          =   other.m_Target;
  m_Transformation  =   other.m_Transformation;
  m_Metric          =   other.m_Metric;
}



/**
 * Destructor
 */
template <class TMetric, class TOptimizationMethod>
RegistrationTransform<TMetric, TOptimizationMethod>
::~RegistrationTransform()
{
}



/**
 * Assignment Operator
 */
template <class TMetric, class TOptimizationMethod>
const RegistrationTransform<TMetric, TOptimizationMethod> &
RegistrationTransform<TMetric, TOptimizationMethod>
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
template <class TMetric, class TOptimizationMethod>
void
RegistrationTransform<TMetric, TOptimizationMethod>
::SetReference( ReferenceType * reference )
{
  m_Reference       =   reference;
  m_Metric->SetReference( m_Reference );
}


/**
 * Set Target 
 */
template <class TMetric, class TOptimizationMethod>
void
RegistrationTransform<TMetric, TOptimizationMethod>
::SetTarget( TargetType * target )
{
  m_Target       =   target;
  m_Metric->SetTarget( m_Target );
}


/**
 * Set Transformation 
 */
template <class TMetric, class TOptimizationMethod>
void
RegistrationTransform<TMetric, TOptimizationMethod>
::SetTransformation( TransformationType * transformation )
{
  m_Transformation  =   transformation;
  m_Mapper->SetTransformation( m_Transformation );
}



/**
 * Starts the Registration Process
 */
template <class TMetric, class TOptimizationMethod>
void
RegistrationTransform<TMetric, TOptimizationMethod>
::StartRegistration( void )
{
  m_Optimizer->SetMetric( m_Metric );
  m_Optimizer->StartOptimization();
}





} // end namespace itk
