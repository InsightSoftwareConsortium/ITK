/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  : m_InitialSimplexDelta(1)  // initial size
{
  m_OptimizerInitialized           = false;
  m_VnlOptimizer                   = 0;
  m_MaximumNumberOfIterations      = 500;
  m_ParametersConvergenceTolerance = 1e-8;
  m_FunctionConvergenceTolerance   = 1e-4;
  m_AutomaticInitialSimplex        = true;
  m_InitialSimplexDelta.Fill(NumericTraits<ParametersType::ValueType>::One);
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
  os << indent << "AutomaticInitialSimplex: "
     << (m_AutomaticInitialSimplex ? "On" : "Off") << std::endl;
  os << indent << "InitialSimplexDelta: "
     << m_InitialSimplexDelta << std::endl;
  
}
  
/** Return Current Value */
AmoebaOptimizer::MeasureType
AmoebaOptimizer
::GetValue() const
{
  const ParametersType & parameters = this->GetCurrentPosition();
  return this->GetCostFunctionAdaptor()->f( parameters );
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

  m_OptimizerInitialized = true;

}



/**
 * Start the optimization
 */
void
AmoebaOptimizer
::StartOptimization( void )
{
    
  this->InvokeEvent( StartEvent() );

  ParametersType initialPosition = GetInitialPosition();

  ParametersType parameters( initialPosition );

  // If the user provides the scales then we set otherwise we don't
  // for computation speed.
  // We also scale the initial parameters up if scales are defined.
  // This compensates for later scaling them down in the cost function adaptor
  // and at the end of this function.  
  if(m_ScalesInitialized)
    {
    ScalesType scales = this->GetScales();
    this->GetCostFunctionAdaptor()->SetScales(scales);
    for(unsigned int i=0;i<parameters.size();i++)
      {
      parameters[i] *= scales[i]; 
      }
    }
  
  
  // vnl optimizers return the solution by reference 
  // in the variable provided as initial position
  if (m_AutomaticInitialSimplex)
    {
    m_VnlOptimizer->minimize( parameters );
    }
  else
    {
    InternalParametersType delta( m_InitialSimplexDelta );
    // m_VnlOptimizer->verbose = 1;
    m_VnlOptimizer->minimize( parameters, delta );
    }
  
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
