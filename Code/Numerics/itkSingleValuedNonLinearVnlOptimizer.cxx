/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedNonLinearVnlOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSingleValuedNonLinearVnlOptimizer_txx
#define _itkSingleValuedNonLinearVnlOptimizer_txx

#include "itkSingleValuedNonLinearVnlOptimizer.h"

namespace itk
{

/** Constructor */
SingleValuedNonLinearVnlOptimizer
::SingleValuedNonLinearVnlOptimizer()
{
  m_CostFunctionAdaptor = 0;
  m_Maximize = false;
  m_Command = CommandType::New();
  m_Command->SetCallbackFunction( this, 
      &SingleValuedNonLinearVnlOptimizer::IterationReport );
  m_CachedValue = 0;
  m_CachedCurrentPosition.Fill(0);
  m_CachedDerivative.Fill(0);
}

/** Destructor */
SingleValuedNonLinearVnlOptimizer
::~SingleValuedNonLinearVnlOptimizer()
{
  if( m_CostFunctionAdaptor )
    {
    delete m_CostFunctionAdaptor;
    m_CostFunctionAdaptor = 0;
    }
}




void 
SingleValuedNonLinearVnlOptimizer
::SetCostFunctionAdaptor( CostFunctionAdaptorType * adaptor )
{

  if( m_CostFunctionAdaptor == adaptor ) 
    {
    return;
    }

  if( m_CostFunctionAdaptor )
    {
    delete m_CostFunctionAdaptor;
    }

  m_CostFunctionAdaptor = adaptor; 

  m_CostFunctionAdaptor->AddObserver( IterationEvent(), m_Command );
}



const SingleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType * 
SingleValuedNonLinearVnlOptimizer
::GetCostFunctionAdaptor( void ) const
{
  return m_CostFunctionAdaptor;
}


SingleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType * 
SingleValuedNonLinearVnlOptimizer
::GetCostFunctionAdaptor( void )
{
  return m_CostFunctionAdaptor;
}


/** The purpose of this method is to get around the lack of
 *  const-correctness in VNL cost-functions and optimizers */
SingleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType * 
SingleValuedNonLinearVnlOptimizer
::GetNonConstCostFunctionAdaptor( void ) const
{
  return m_CostFunctionAdaptor;
}


/** The purpose of this method is to get around the lack of iteration reporting
 * in VNL optimizers. By interfacing directly with the ITK cost function
 * adaptor we are generating here Iteration Events. Note the iteration events
 * here are produce PER EVALUATION of the metric, not per real iteration of the
 * vnl optimizer. Optimizers that evaluate the metric multiple times at each
 * iteration will generate a lot more of Iteration events here. */
void
SingleValuedNonLinearVnlOptimizer
::IterationReport( const EventObject & event ) 
{
  const CostFunctionAdaptorType * adaptor = this->GetCostFunctionAdaptor();
  m_CachedValue = adaptor->GetCachedValue();
  m_CachedDerivative = adaptor->GetCachedDerivative();
  m_CachedCurrentPosition = adaptor->GetCachedCurrentParameters();
  this->InvokeEvent( event );
}



/**
 * PrintSelf
 */
void
SingleValuedNonLinearVnlOptimizer
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Maximize flag: "
     << (m_Maximize ? "On" : "Off") << std::endl;
  os << indent << "Cached Value: " << m_CachedValue << std::endl;
  os << indent << "Cached Derivative: " << m_CachedDerivative << std::endl;
  os << indent << "Cached current positiion: "
     << m_CachedCurrentPosition << std::endl;
  os << "Command observer " << m_Command.GetPointer() << std::endl;
  os << "Cost Function adaptor" << m_CostFunctionAdaptor << std::endl;
}


} // end namespace itk

#endif
