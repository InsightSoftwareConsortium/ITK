/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedNonLinearVnlOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMultipleValuedNonLinearVnlOptimizer_txx
#define _itkMultipleValuedNonLinearVnlOptimizer_txx

#include "itkMultipleValuedNonLinearVnlOptimizer.h"

namespace itk
{

/**
 * Constructor
 */
MultipleValuedNonLinearVnlOptimizer
::MultipleValuedNonLinearVnlOptimizer()
{
  m_CostFunctionAdaptor = 0;
  m_UseGradient = true;
}


/**
 * Destructor
 */
MultipleValuedNonLinearVnlOptimizer
::~MultipleValuedNonLinearVnlOptimizer()
{
  if( m_CostFunctionAdaptor )
    {
    delete m_CostFunctionAdaptor;
    m_CostFunctionAdaptor = 0;
    }
}




void 
MultipleValuedNonLinearVnlOptimizer
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

  this->SetUseCostFunctionGradient(m_UseGradient);

}



const MultipleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType * 
MultipleValuedNonLinearVnlOptimizer
::GetCostFunctionAdaptor( void ) const
{
  return m_CostFunctionAdaptor;
}


MultipleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType * 
MultipleValuedNonLinearVnlOptimizer
::GetCostFunctionAdaptor( void )
{
  return m_CostFunctionAdaptor;
}

/** The purpose of this method is to get around the lack of const 
 * correctness in vnl cost_functions and optimizers */
MultipleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType * 
MultipleValuedNonLinearVnlOptimizer
::GetNonConstCostFunctionAdaptor( void ) const
{
  return m_CostFunctionAdaptor;
}


void
MultipleValuedNonLinearVnlOptimizer
::SetUseCostFunctionGradient( bool useGradient ) 
{
  if( m_CostFunctionAdaptor )
    {
    m_CostFunctionAdaptor->SetUseGradient( useGradient );
    }
  else
    {
    m_UseGradient = useGradient;
    }
}





bool
MultipleValuedNonLinearVnlOptimizer
::GetUseCostFunctionGradient() const
{
  if( m_CostFunctionAdaptor )
    {
    return m_CostFunctionAdaptor->GetUseGradient();
    }
  else
    {
    return m_UseGradient;
    }
}





} // end namespace itk

#endif
