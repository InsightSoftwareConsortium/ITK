/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkAmoebaOptimizer_txx
#define _itkAmoebaOptimizer_txx

#include "itkAmoebaOptimizer.h"

namespace itk
{

/**
 * Constructor
 */
AmoebaOptimizer
::AmoebaOptimizer()
{
  m_OptimizerInitialized    = false;
  m_VnlOptimizer            = 0;
}


/**
 * Destructor
 */
AmoebaOptimizer
::~AmoebaOptimizer()
{
  delete m_VnlOptimizer;
}



/**
 * Connect a Cost Function
 */
void
AmoebaOptimizer
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

  m_VnlOptimizer = new vnl_amoeba( *adaptor );

  ScalesType scales( numberOfParameters );
  scales.Fill( 1.0f );
  SetScales( scales );

  m_OptimizerInitialized = true;

}



/**
 * Start the optimization
 */
void
AmoebaOptimizer
::StartOptimization( void )
{
  
  ParametersType initialPosition = GetInitialPosition();

  InternalParametersType parameters( initialPosition.Size() );

  CostFunctionAdaptorType::ConvertExternalToInternalParameters( 
                                            GetInitialPosition(), 
                                            parameters     );

  m_VnlOptimizer->minimize( parameters );

  ParametersType solution =
   this->GetCostFunctionAdaptor()->GetCostFunction()->GetParameters() ;

  this->SetCurrentPosition( solution );
      

}




/**
 * Get the Optimizer
 */
vnl_amoeba * 
AmoebaOptimizer
::GetOptimizer()
{
  return m_VnlOptimizer;
}




} // end namespace itk

#endif
