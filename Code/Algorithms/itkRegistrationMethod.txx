/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMethod.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRegistrationMethod_txx
#define _itkRegistrationMethod_txx

#include "itkRegistrationMethod.h"


namespace itk
{

/**
 * Constructor
 */
template <class TTraits>
RegistrationMethod<TTraits>
::RegistrationMethod()
{ 
  m_Metric    = MetricType::New();
  m_Optimizer = OptimizerType::New();
}


/**
 * Constructor
 */
template <class TTraits>
RegistrationMethod<TTraits>
::RegistrationMethod( const Self & other )
{
  m_Metric          =   other.m_Metric;
  m_Optimizer       =   other.m_Optimizer;
}



/**
 * Destructor
 */
template <class TTraits>
RegistrationMethod<TTraits>
::~RegistrationMethod()
{
}



/**
 * Assignment Operator
 */
template <class TTraits>
const RegistrationMethod< TTraits > &
RegistrationMethod< TTraits >
::operator=( const Self & other )
{
  m_Metric          =   other.m_Metric;
  m_Optimizer       =   other.m_Optimizer;
  return *this;
}


/**
 * Get Reference 
 */


template <class TTraits>
typename RegistrationMethod<TTraits>::ReferencePointer
RegistrationMethod<TTraits>
::GetReference( void )
{
  return m_Metric->GetReference();
}


/**
 * Get Target 
 */
template <class TTraits>
typename RegistrationMethod<TTraits>::TargetPointer
RegistrationMethod< TTraits >
::GetTarget( void )
{
  return  m_Metric->GetTarget();
}



template <class TTraits>
void
RegistrationMethod<TTraits>
::SetReference( ReferenceType * reference )
{
  m_Metric->SetReference( reference );
}


/**
 * Set Target 
 */
template <class TTraits>
void
RegistrationMethod< TTraits >
::SetTarget( TargetType * target )
{
  m_Metric->SetTarget( target );
}






/**
 * Starts the Registration Process
 */
template <class TTraits>
int
RegistrationMethod<TTraits>
::StartRegistration( void )
{ 
  std::cerr << "RegistrationMethod::StartRegistration::" << std::endl;
  std::cerr << "This method should have been overloaded" << std::endl;
  return 0;
}



} // end namespace itk


#endif
