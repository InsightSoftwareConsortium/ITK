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
}



/**
 * Start the optimization
 */
template <class TCostFunction>
void
GradientDescentOptimizer<TCostFunction>
::StartOptimization(ParametersPointer &  initialValue )
{
  m_CurrentStepLength         = m_MaximumStepLength;
  m_CurrentNumberIterations   = 0;
  ResumeOptimization();
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
    m_Value = m_CostFunction->GetValue();

    if( m_Stop )
    {
      break;
    }

	typename ParametersType::Iterator grad  = m_Gradient->Begin();
	typename ParametersType::Iterator pgrad = m_PreviousGradient->Begin();
    while( grad != m_Gradient->End() )
    {
	   pgrad.Value() = grad.Value();
	   ++grad;
	   ++pgrad;
	}

    m_Gradient = m_CostFunction->GetDerivative();

    if( m_Stop )
    {
      break;
    }

    AdvanceOneStep();

    m_CurrentNumberIterations++;

    if( m_CurrentNumberIterations == m_MaximumNumberOfIterations )
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

    double magnitudeSquare = 0;

	typename ParametersType::Iterator grad  = m_Gradient->Begin();
	typename ParametersType::Iterator step  = m_StepSize->Begin();
	typename ParametersType::Iterator pgrad = m_PreviousGradient->Begin();

    while( grad != m_Gradient->End() )
    {
      const double weighted = grad.Value() * step.Value();
      magnitudeSquare += weighted * weighted;
	  ++grad;
	  ++step;
    }
    
    const double gradientMagnitude = sqrt( magnitudeSquare );

    if( gradientMagnitude < m_GradientMagnitudeTolerance ) 
    {
       m_StopCondition = GradientMagnitudeTolerance;
       StopOptimization();
       return;
    }
    
    double scalarProduct = 0;

	grad  = m_Gradient->Begin();
	pgrad = m_Gradient->Begin();
	step  = m_StepSize->Begin();

    while( grad != m_Gradient->End() )    
    {
      const double weight1 =  grad.Value()  * step.Value();
      const double weight2 = pgrad.Value()  * step.Value();
      scalarProduct += weight1 * weight2;
	  ++grad;
	  ++step;
	  ++pgrad;
    }

   
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

    ParametersPointer positionChange = ParametersType::New();
	positionChange->Reserve( m_Gradient->Size() );
	typename ParametersType::Iterator post  = positionChange->Begin();

	grad  = m_Gradient->Begin();
	post  = positionChange->Begin();

    while( grad != m_Gradient->End() )    
    {
      post.Value() = grad.Value() * (direction * m_CurrentStepLength / gradientMagnitude);
	  ++grad;
	  ++post;
    }





}



} // end namespace itk

#endif
