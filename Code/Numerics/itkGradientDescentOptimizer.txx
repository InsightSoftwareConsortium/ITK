/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientDescentOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkGradientDescentOptimizer_txx
#define _itkGradientDescentOptimizer_txx

#include "itkGradientDescentOptimizer.h"

namespace itk
{

/**
 * Constructor
 */
template <class TCostFunction>
GradientDescentOptimizer<TCostFunction>
::GradientDescentOptimizer()
{
   m_LearningRate = 1.0;
   m_NumberOfIterations = 100;
}



/**
 * Start the optimization
 */
template <class TCostFunction>
void
GradientDescentOptimizer<TCostFunction>
::StartOptimization( void )
{

  m_CurrentNumberOfIterations   = 0;

  this->SetCurrentPosition( GetInitialPosition() );
  this->ResumeOptimization();

}



/**
 * Resume the optimization
 */
template <class TCostFunction>
void
GradientDescentOptimizer<TCostFunction>
::ResumeOptimization( void )
{
  
  m_Stop = false;

  while( !m_Stop ) 
  {

    m_CostFunction->GetValueAndDerivative( GetCurrentPosition(), m_Value, m_Gradient );
    if( m_Stop )
    {
      break;
    }
  
    AdvanceOneStep();

    m_CurrentNumberOfIterations++;

    if( m_CurrentNumberOfIterations >= m_NumberOfIterations )
    {
       m_StopCondition = MaximumNumberOfIterations;
       StopOptimization();
       break;
    }
    
  }
    

}


/**
 * Stop optimization
 */
template <class TCostFunction>
void
GradientDescentOptimizer<TCostFunction>
::StopOptimization( void )
{
  m_Stop = true;
}


/**
 * Advance one Step following the gradient direction
 */
template <class TCostFunction>
void
GradientDescentOptimizer<TCostFunction>
::AdvanceOneStep( void )
{ 

  double direction = 1.0;
  if( this->m_Maximize ) 
  {
    direction = 1.0;
  }
  else 
  {
    direction = -1.0;
  }

  ParametersType newPosition;
  const ParametersType & currentPosition = GetCurrentPosition();

  for(unsigned int j=0; j<SpaceDimension; j++)
  {
    newPosition[j] = currentPosition[j] + 
      direction * m_LearningRate * m_Scale[j] * m_Gradient[j];
  }

  SetCurrentPosition( newPosition );

}



} // end namespace itk

#endif
