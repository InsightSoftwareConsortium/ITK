/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConjugateGradientOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConjugateGradientOptimizer_txx
#define _itkConjugateGradientOptimizer_txx

#include "itkConjugateGradientOptimizer.h"

namespace itk
{

/**
 * Constructor
 */
ConjugateGradientOptimizer
::ConjugateGradientOptimizer()
{
  m_OptimizerInitialized    = false;
  m_VnlOptimizer            = 0;
}


/**
 * Destructor
 */
ConjugateGradientOptimizer
::~ConjugateGradientOptimizer()
{
  delete m_VnlOptimizer;
}


/**
 * Get the Optimizer
 */
vnl_conjugate_gradient * 
ConjugateGradientOptimizer
::GetOptimizer( void ) 
{
  return m_VnlOptimizer;
}



/**
 * Connect a Cost Function
 */
void
ConjugateGradientOptimizer
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

  m_VnlOptimizer = new vnl_conjugate_gradient( *adaptor );

  ScalesType scales( numberOfParameters );
  scales.Fill( 1.0f );
  SetScales( scales );

  m_OptimizerInitialized = true;

}


/**
 * Start the optimization
 */
void
ConjugateGradientOptimizer
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


} // end namespace itk

#endif
