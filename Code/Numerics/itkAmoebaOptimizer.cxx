/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizer.cxx
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
  m_OptimizerInitialized           = false;
  m_VnlOptimizer                   = 0;
  m_MaximumNumberOfIterations      = 500;
  m_ParametersConvergenceTolerance = 1e-8;
  m_FunctionConvergenceTolerance   = 1e-4;
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
 * PrintSelf
 */
void
AmoebaOptimizer
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "MaximumNumberOfIterations: " 
     << m_MaximumNumberOfIterations << std::endl;
  os << indent << "ParametersConvergenceTolerance: "
     << m_ParametersConvergenceTolerance << std::endl;
  os << indent << "FunctionConvergenceTolerance: "
     << m_FunctionConvergenceTolerance << std::endl;
}


/**
 * Set the maximum number of iterations
 */
void
AmoebaOptimizer
::SetMaximumNumberOfIterations( unsigned int n )
{
  if ( n == m_MaximumNumberOfIterations )
    {
    return;
    }

  m_MaximumNumberOfIterations = n;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_max_iterations( static_cast<int>( n ) );
    }

  this->Modified();
}

/**
 * Set the parameters convergence tolerance
 */
void
AmoebaOptimizer
::SetParametersConvergenceTolerance( double tol )
{
  if ( tol == m_ParametersConvergenceTolerance )
    {
    return;
    }

  m_ParametersConvergenceTolerance = tol;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_x_tolerance( tol );
    }

  this->Modified();
}


/**
 * Set the function convergence tolerance
 */
void
AmoebaOptimizer
::SetFunctionConvergenceTolerance( double tol )
{
  if ( tol == m_FunctionConvergenceTolerance )
    {
    return;
    }

  m_FunctionConvergenceTolerance = tol;
  if ( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_f_tolerance( tol );
    }

  this->Modified();
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

  // set up optimizer parameters
  m_VnlOptimizer->set_max_iterations( static_cast<int>( m_MaximumNumberOfIterations ) );
  m_VnlOptimizer->set_x_tolerance( m_ParametersConvergenceTolerance );
  m_VnlOptimizer->set_f_tolerance( m_FunctionConvergenceTolerance );

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
vnl_amoeba * 
AmoebaOptimizer
::GetOptimizer()
{
  return m_VnlOptimizer;
}




} // end namespace itk

#endif
