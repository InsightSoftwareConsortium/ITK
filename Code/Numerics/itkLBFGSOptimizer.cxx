/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLBFGSOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLBFGSOptimizer_txx
#define _itkLBFGSOptimizer_txx

#include "itkLBFGSOptimizer.h"

namespace itk
{

/**
 * Constructor
 */
LBFGSOptimizer
::LBFGSOptimizer()
{
  m_OptimizerInitialized    = false;
  m_VnlOptimizer            = 0;
}


/**
 * Destructor
 */
LBFGSOptimizer
::~LBFGSOptimizer()
{
  delete m_VnlOptimizer;
}



/**
 * Connect a Cost Function
 */
void
LBFGSOptimizer
::SetCostFunction( SingleValuedCostFunction * costFunction )
{


  const unsigned int numberOfParameters = 
                        costFunction->GetNumberOfParameters();

  CostFunctionAdaptorType * adaptor = 
              new CostFunctionAdaptorType( numberOfParameters );
       
  adaptor->SetCostFunction( costFunction );

  if( m_OptimizerInitialized )
    { 
    delete m_VnlOptimizer;
    }
    
  this->SetCostFunctionAdaptor( adaptor );

  m_VnlOptimizer = new vnl_lbfgs( *adaptor );

  ScalesType scales( numberOfParameters );
  scales.Fill( 1.0f );
  SetScales( scales );

  m_OptimizerInitialized = true;

}



/**
 * Start the optimization
 */
void
LBFGSOptimizer
::StartOptimization( void )
{
  
  ParametersType initialPosition = GetInitialPosition();

  InternalParametersType parameters( initialPosition.Size() );

  CostFunctionAdaptorType::ConvertExternalToInternalParameters( 
                                            GetInitialPosition(), 
                                            parameters     );
  
  // vnl optimizers return the solution by reference 
  // in the variable provided as initial position
  m_VnlOptimizer->minimize( parameters );
  
  ParametersType solution;

  CostFunctionAdaptorType::ConvertInternalToExternalParameters( 
                                            parameters,
                                            solution     );
  this->SetCurrentPosition( solution );
         

}




/**
 * Get the Optimizer
 */
vnl_lbfgs * 
LBFGSOptimizer
::GetOptimizer()
{
  return m_VnlOptimizer;
}




} // end namespace itk

#endif
