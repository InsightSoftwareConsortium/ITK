/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRegistrationTransform_txx
#define _itkRegistrationTransform_txx


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
 * Destructor
 */
template <class TMetric, class TOptimizationMethod>
RegistrationTransform<TMetric, TOptimizationMethod>
::~RegistrationTransform()
{
}


/**
 * Set Reference 
 */
template <class TMetric, class TOptimizationMethod>
void
RegistrationTransform<TMetric, TOptimizationMethod>
::SetReference( const ReferenceType * reference )
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
::SetTarget( const TargetType * target )
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

template <class TMetric, class TOptimizationMethod>
void
RegistrationTransform<TMetric, TOptimizationMethod>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  if (m_Target)
    {
    os << "Target: " << m_Target << std::endl;
    }
  if (m_Reference)
    {
    os << "Reference: " << m_Reference << std::endl;
    }
  if (m_Transformation)
    {
    os << "Transformation: " << m_Transformation << std::endl;
    }
  if (m_Mapper)
    {
    os << "Mapper: " << m_Mapper << std::endl;
    }
  if (m_Metric)
    {
    os << "Metric: " << m_Metric << std::endl;
    }
  if (m_Optimizer)
    {
    os << "Optimizer: " << m_Optimizer << std::endl;
    }
}

} // end namespace itk

#endif
