/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMethod.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * Destructor
 */
template <class TTraits>
RegistrationMethod<TTraits>
::~RegistrationMethod()
{
}


/**
 * Get Reference 
 */


template <class TTraits>
typename RegistrationMethod<TTraits>::ReferenceConstPointer
RegistrationMethod<TTraits>
::GetReference( void )
{
  return m_Metric->GetReference();
}


/**
 * Get Target 
 */
template <class TTraits>
typename RegistrationMethod<TTraits>::TargetConstPointer
RegistrationMethod< TTraits >
::GetTarget( void )
{
  return  m_Metric->GetTarget();
}



template <class TTraits>
void
RegistrationMethod<TTraits>
::SetReference( const ReferenceType * reference )
{
  m_Metric->SetReference( reference );
}


/**
 * Set Target 
 */
template <class TTraits>
void
RegistrationMethod< TTraits >
::SetTarget( const TargetType * target )
{
  m_Metric->SetTarget( target );
}






/**
 * Starts the Registration Process
 */
template <class TTraits>
void
RegistrationMethod<TTraits>
::StartRegistration( void )
{ 
  itkErrorMacro(<< "RegistrationMethod::StartRegistration::" );
  itkErrorMacro(<< "This method should have been overloaded" );

}


/**
 * PrintSelf
 */
template <class TTraits>
void
RegistrationMethod<TTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Metric: " << m_Metric.GetPointer() << std::endl;
  os << indent << "Optimizer: ";
  os << m_Optimizer.GetPointer() << std::endl;
}




} // end namespace itk


#endif
