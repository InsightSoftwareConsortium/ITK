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
  m_Trace                              = false;
  m_MaximumNumberOfFunctionEvaluations = 2000;
  m_GradientConvergenceTolerance       = 1e-5;
  m_LineSearchAccuracy                 = 0.9;
  m_DefaultStepLength                  = 1.0;
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
 * PrintSelf
 */
void
LBFGSOptimizer
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Trace: ";
  if ( m_Trace ) { os << "On"; } else { os << "Off"; }
  os << std::endl;
  os << indent << "MaximumNumberOfFunctionEvaluations: ";
  os << m_MaximumNumberOfFunctionEvaluations << std::endl;
  os << indent << "GradientConvergenceTolerance: ";
  os << m_GradientConvergenceTolerance << std::endl;
  os << indent << "LineSearchAccuracy: ";
  os << m_LineSearchAccuracy << std::endl;
  os << indent << "DefaultStepLength: ";
  os << m_DefaultStepLength << std::endl;

}

/**
 * Set the optimizer trace flag
 */
void
LBFGSOptimizer
::SetTrace( bool flag )
{
  if ( flag == m_Trace )
    {
    return;
    }

  m_Trace = flag;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_trace( m_Trace );
    }

  this->Modified();
}

/**
 * Set the maximum number of function evalutions
 */
void
LBFGSOptimizer
::SetMaximumNumberOfFunctionEvaluations( unsigned int n )
{
  if ( n == m_MaximumNumberOfFunctionEvaluations )
    {
    return;
    }

  m_MaximumNumberOfFunctionEvaluations = n;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_max_function_evals(
      static_cast<int>( m_MaximumNumberOfFunctionEvaluations ) );
    }

  this->Modified();
}

/**
 * Set the gradient convergence tolerance
 */
void
LBFGSOptimizer
::SetGradientConvergenceTolerance( double f )
{
  if ( f == m_GradientConvergenceTolerance )
    {
    return;
    }

  m_GradientConvergenceTolerance = f;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_g_tolerance( m_GradientConvergenceTolerance );
    }

  this->Modified();
}

/**
 * Set the line search accuracy
 */
void
LBFGSOptimizer
::SetLineSearchAccuracy( double f )
{
  if ( f == m_LineSearchAccuracy )
    {
    return;
    }

  m_LineSearchAccuracy = f;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->line_search_accuracy = m_LineSearchAccuracy;
    }

  this->Modified();
}

/**
 * Set the default step length
 */
void
LBFGSOptimizer
::SetDefaultStepLength( double f )
{
  if ( f == m_DefaultStepLength )
    {
    return;
    }

  m_DefaultStepLength = f;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->default_step_length = m_DefaultStepLength;
    }

  this->Modified();
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

  // set the optimizer parameters
  m_VnlOptimizer->set_trace( m_Trace );
  m_VnlOptimizer->set_max_function_evals(
    static_cast<int>( m_MaximumNumberOfFunctionEvaluations ) );
  m_VnlOptimizer->set_g_tolerance( m_GradientConvergenceTolerance );
  m_VnlOptimizer->line_search_accuracy = m_LineSearchAccuracy;
  m_VnlOptimizer->default_step_length  = m_DefaultStepLength;


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
