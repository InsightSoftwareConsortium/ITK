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

/**
 * Constructor
 */
SingleValuedNonLinearVnlOptimizer
::SingleValuedNonLinearVnlOptimizer()
{
  m_CostFunctionAdaptor = 0;
  m_Maximize = false;
}


/**
 * Destructor
 */
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
}


} // end namespace itk

#endif
