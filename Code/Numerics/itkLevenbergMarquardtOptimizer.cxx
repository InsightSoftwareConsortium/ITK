/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevenbergMarquardtOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLevenbergMarquardtOptimizer_txx
#define _itkLevenbergMarquardtOptimizer_txx

#include "itkLevenbergMarquardtOptimizer.h"

namespace itk
{

/**
 * Constructor
 */
LevenbergMarquardtOptimizer
::LevenbergMarquardtOptimizer()
{
  m_OptimizerInitialized    = false;
  m_VnlOptimizer            = 0;
  m_NumberOfIterations      = 2000;
  m_ValueTolerance          = 1e-8;
  m_GradientTolerance       = 1e-5;
  m_EpsilonFunction         = 1e-11;
}


/**
 * Destructor
 */
LevenbergMarquardtOptimizer
::~LevenbergMarquardtOptimizer()
{
  delete m_VnlOptimizer;
}



/**
 * Connect a Cost Function
 */
void
LevenbergMarquardtOptimizer
::SetCostFunction( MultipleValuedCostFunction * costFunction )
{
  const unsigned int numberOfParameters = 
    costFunction->GetNumberOfParameters();

  const unsigned int numberOfValues = 
    costFunction->GetNumberOfValues();

  CostFunctionAdaptorType * adaptor = 
    new CostFunctionAdaptorType( numberOfParameters, numberOfValues );
       
  adaptor->SetCostFunction( costFunction );

  if( m_OptimizerInitialized )
    { 
    delete m_VnlOptimizer;
    }
    
  this->SetCostFunctionAdaptor( adaptor );

  m_VnlOptimizer = new vnl_levenberg_marquardt( *adaptor );
 
  this->SetNumberOfIterations(m_NumberOfIterations);
  this->SetValueTolerance(m_ValueTolerance);
  this->SetGradientTolerance(m_GradientTolerance);
  this->SetEpsilonFunction(m_EpsilonFunction);

  m_OptimizerInitialized = true;

}

/** Return Current Value */
LevenbergMarquardtOptimizer::MeasureType
LevenbergMarquardtOptimizer
::GetValue()
{
  MeasureType  measures;
  this->GetNonConstCostFunctionAdaptor()->f(this->GetCurrentPosition(),measures);
  return measures;
}


/**
 * Start the optimization
 */
void
LevenbergMarquardtOptimizer
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
    this->GetNonConstCostFunctionAdaptor()->SetScales(scales);
    for(unsigned int i=0;i<parameters.size();i++)
      {
      parameters[i] *= scales[i]; 
      }
    }
  
  if( this->GetCostFunctionAdaptor()->GetUseGradient() )
    {
    m_VnlOptimizer->minimize_using_gradient( parameters );
    }
  else
    {
    m_VnlOptimizer->minimize_without_gradient( parameters );
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

  this->SetCurrentPosition(parameters);
      
  this->InvokeEvent( EndEvent() );
  
}

/** Set the maximum number of iterations */
void 
LevenbergMarquardtOptimizer
::SetNumberOfIterations(unsigned int iterations)
{
  if(m_VnlOptimizer)
    { 
    m_VnlOptimizer->set_max_function_evals(iterations);
    }

  m_NumberOfIterations = iterations;
}


/** Set the maximum number of iterations */
void 
LevenbergMarquardtOptimizer
::SetValueTolerance(double tol)
{
  if(m_VnlOptimizer)
    { 
    m_VnlOptimizer->set_x_tolerance(tol);
    }

  m_ValueTolerance = tol;
}


/** Set Gradient Tolerance */
void 
LevenbergMarquardtOptimizer
::SetGradientTolerance(double tol)
{
 if(m_VnlOptimizer)
    { 
    m_VnlOptimizer->set_g_tolerance(tol);
    }

  m_GradientTolerance = tol;

}


/** Set Epsilon function */
void 
LevenbergMarquardtOptimizer
::SetEpsilonFunction(double epsilon)
{
  if(m_VnlOptimizer)
    { 
    m_VnlOptimizer->set_epsilon_function(epsilon);
    }

  m_EpsilonFunction = epsilon;

}


/** Get the Optimizer */
vnl_levenberg_marquardt * 
LevenbergMarquardtOptimizer
::GetOptimizer()
{
  return m_VnlOptimizer;
}




} // end namespace itk

#endif
