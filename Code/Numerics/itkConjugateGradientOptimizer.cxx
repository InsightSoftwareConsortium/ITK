/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConjugateGradientOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  m_OptimizerInitialized = true;

}

/** Return Current Value */
ConjugateGradientOptimizer::MeasureType
ConjugateGradientOptimizer
::GetValue()
{
  return this->GetNonConstCostFunctionAdaptor()->f(this->GetCurrentPosition());
}

/**
 * Start the optimization
 */
void
ConjugateGradientOptimizer
::StartOptimization( void )
{
  this->InvokeEvent( StartEvent() );

  if( this->GetMaximize() )
    {
    this->GetNonConstCostFunctionAdaptor()->NegateCostFunctionOn();
    }

  ParametersType initialPosition = this->GetInitialPosition();

  ParametersType parameters(initialPosition);

  // If the user provides the scales then we set otherwise we don't
  // for computation speed.
  // We also scale the initial parameters up if scales are defined.
  // This compensates for later scaling them down in the cost function adaptor
  // and at the end of this function.  
  if(m_ScalesInitialized)
    {
    ScalesType scales = this->GetScales();
    this->GetNonConstCostFunctionAdaptor()->SetScales(scales);
    for(unsigned int i=0;i<parameters.size();i++)
      {
      parameters[i] *= scales[i]; 
      }
    }
  

  // vnl optimizers return the solution by reference 
  // in the variable provided as initial position
  m_VnlOptimizer->minimize( parameters );
  
   // we scale the parameters down if scales are defined
  if(m_ScalesInitialized)
    {
    ScalesType scales = this->GetScales();
    for(unsigned int i=0;i<parameters.size();i++)
      {
      parameters[i] /= scales[i]; 
      }
    }

  this->SetCurrentPosition( parameters );
    
  this->InvokeEvent( EndEvent() );
    
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
