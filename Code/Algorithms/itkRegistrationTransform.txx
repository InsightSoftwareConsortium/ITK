/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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





} // end namespace itk

#endif
