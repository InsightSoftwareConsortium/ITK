/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRegularStepGradientDescentOptimizer_txx
#define _itkRegularStepGradientDescentOptimizer_txx

#include "itkRegularStepGradientDescentOptimizer.h"

namespace itk
{

/**
 * Constructor
 */
template <class TCostFunction>
RegularStepGradientDescentOptimizer<TCostFunction>
::RegularStepGradientDescentOptimizer()
{
  for(unsigned int i=0; i<SpaceDimension; i++)
  {
    m_Gradient[i] = 0;
    m_PreviousGradient[i] = 0;
  }
  
}



/**
 * Start the optimization
 */
template <class TCostFunction>
void
RegularStepGradientDescentOptimizer<TCostFunction>
::StartOptimization( void )
{

  m_CurrentStepLength         = m_MaximumStepLength;
  m_CurrentNumberOfIterations   = 0;

  this->SetCurrentPosition( GetInitialPosition() );
  this->ResumeOptimization();

}





/**
 * Resume the optimization
 */
template <class TCostFunction>
void
RegularStepGradientDescentOptimizer<TCostFunction>
::ResumeOptimization( void )
{
  
  m_Stop = false;

  while( !m_Stop ) 
  {

    m_Value = m_CostFunction->GetValue( GetCurrentPosition() );

    if( m_Stop )
    {
      break;
    }

    m_PreviousGradient = m_Gradient;
  
    typename CostFunctionType::DerivativeType derivative =
            m_CostFunction->GetDerivative( GetCurrentPosition() );

    for( unsigned int i=0; i<CostFunctionType::SpaceDimension; i++)
    {
      m_Gradient[i] = derivative[i];
    }

    if( m_Stop )
    {
      break;
    }

    AdvanceOneStep();

    m_CurrentNumberOfIterations++;

    if( m_CurrentNumberOfIterations == m_MaximumNumberOfIterations )
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
RegularStepGradientDescentOptimizer<TCostFunction>
::StopOptimization( void )
{
  m_Stop = true;
}


/**
 * Advance one Step following the gradient direction
 */
template <class TCostFunction>
void
RegularStepGradientDescentOptimizer<TCostFunction>
::AdvanceOneStep( void )
{ 

  double magnitudeSquare = 0;
  for(unsigned int dim=0; dim<SpaceDimension; dim++)
  {
    const double weighted = m_Gradient[dim] * m_Scale[dim];
    magnitudeSquare += weighted * weighted;
  }
    
  const double gradientMagnitude = sqrt( magnitudeSquare );

  if( gradientMagnitude < m_GradientMagnitudeTolerance ) 
  {
    m_StopCondition = RegularStepGradientMagnitudeTolerance;
    StopOptimization();
    return;
  }
    
  double scalarProduct = 0;

  for(unsigned int i=0; i<SpaceDimension; i++)
  {
    const double weight1 = m_Gradient[i]         * m_Scale[i]; 
    const double weight2 = m_PreviousGradient[i] * m_Scale[i]; 
    scalarProduct += weight1 * weight2;
  }
   
  // If there is a direction change 
  if( scalarProduct < 0 ) 
  {
    m_CurrentStepLength /= 2.0;
  }

  if( m_CurrentStepLength < m_MinimumStepLength )
  {
    m_StopCondition = StepTooSmall;
    StopOptimization();
    return;
  }

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
  const double factor = 
    (direction * m_CurrentStepLength / gradientMagnitude);

  for(unsigned int j=0; j<SpaceDimension; j++)
  {
    newPosition[j] = currentPosition[j] + m_Gradient[j] * factor;
  }

  SetCurrentPosition( newPosition );

}



} // end namespace itk

#endif
