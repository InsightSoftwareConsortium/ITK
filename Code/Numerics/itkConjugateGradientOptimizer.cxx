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
 * Get the maximum number of evaluations of the function.
 * In vnl this is used instead of a maximum number of iterations
 * given that an iteration could imply several evaluations.
 */
unsigned long 
ConjugateGradientOptimizer
::GetNumberOfIterations( void ) const
{
  return m_VnlOptimizer->get_max_function_evals();
}


/**
 * Get the number of iterations in the last optimization.
 */
unsigned long 
ConjugateGradientOptimizer
::GetCurrentIteration( void ) const
{
  return m_VnlOptimizer->get_num_iterations();
}





} // end namespace itk

#endif
