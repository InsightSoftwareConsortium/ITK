/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevenbergMarquardtOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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

  ScalesType scales( numberOfParameters );
  scales.Fill( 1.0f );
  SetScales( scales );

  m_OptimizerInitialized = true;

}



/**
 * Start the optimization
 */
void
LevenbergMarquardtOptimizer
::StartOptimization( void )
{
  
  ParametersType initialPosition = GetInitialPosition();

  InternalParametersType parameters( initialPosition.Size() );

  CostFunctionAdaptorType::ConvertExternalToInternalParameters( 
                                            GetInitialPosition(), 
                                            parameters     );

  if( this->GetCostFunctionAdaptor()->GetUseGradient() )
    {
    m_VnlOptimizer->minimize_using_gradient( parameters );
    }
  else
    {
    m_VnlOptimizer->minimize_without_gradient( parameters );
    }

  // InternalParametersType is different than ParametersType....
  ParametersType p(parameters.size());
  for(unsigned int i=0; i < parameters.size(); ++i)
    {
    p[i] = parameters[i];
    }
  this->SetCurrentPosition( p );
      

}




/**
 * Get the Optimizer
 */
vnl_levenberg_marquardt * 
LevenbergMarquardtOptimizer
::GetOptimizer()
{
  return m_VnlOptimizer;
}




} // end namespace itk

#endif
