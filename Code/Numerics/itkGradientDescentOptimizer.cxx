/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientDescentOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGradientDescentOptimizer_txx
#define _itkGradientDescentOptimizer_txx

#include "itkGradientDescentOptimizer.h"
#include "itkCommand.h"
#include "itkEventObject.h"
#include "itkExceptionObject.h"

namespace itk
{

/**
 * Constructor
 */
GradientDescentOptimizer
::GradientDescentOptimizer()
{
  itkDebugMacro("Constructor");

  m_LearningRate = 1.0;
  m_NumberOfIterations = 100;
  m_CurrentIteration = 0;
  m_Maximize = false;
  m_Value = 0.0;
  m_StopCondition = MaximumNumberOfIterations;
}



void
GradientDescentOptimizer
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "LearningRate: "
     << m_LearningRate << std::endl;
  os << indent << "NunberOfIterations: "
     << m_NumberOfIterations << std::endl;
  os << indent << "Maximize: "
     << m_Maximize << std::endl;
  os << indent << "CurrentIteration: "
     << m_CurrentIteration;
  os << indent << "Value: "
     << m_Value;
  if (m_CostFunction)
    {
    os << indent << "CostFunction: "
       << m_CostFunction;
    }
  os << indent << "StopCondition: "
     << m_StopCondition;
  os << std::endl;

}


/**
 * Start the optimization
 */
void
GradientDescentOptimizer
::StartOptimization( void )
{

  itkDebugMacro("StartOptimization");
   
  m_CurrentIteration   = 0;

  this->SetCurrentPosition( this->GetInitialPosition() );
  this->ResumeOptimization();

}



/**
 * Resume the optimization
 */
void
GradientDescentOptimizer
::ResumeOptimization( void )
{
  
  itkDebugMacro("ResumeOptimization");

  m_Stop = false;

  InvokeEvent( StartEvent() );
  while( !m_Stop ) 
    {

    try
      {
      m_CostFunction->GetValueAndDerivative( 
        this->GetCurrentPosition(), m_Value, m_Gradient );
      }
    catch( ExceptionObject& err )
      {
      // An exception has occurred. 
      // Terminate immediately.
      m_StopCondition = MetricError;
      StopOptimization();

      // Pass exception to caller
      throw err;
      }


    if( m_Stop )
      {
      break;
      }
  
    AdvanceOneStep();

    m_CurrentIteration++;

    if( m_CurrentIteration >= m_NumberOfIterations )
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
void
GradientDescentOptimizer
::StopOptimization( void )
{

  itkDebugMacro("StopOptimization");

  m_Stop = true;
  InvokeEvent( EndEvent() );
}





/**
 * Advance one Step following the gradient direction
 */
void
GradientDescentOptimizer
::AdvanceOneStep( void )
{ 

  itkDebugMacro("AdvanceOneStep");

  double direction;
  if( this->m_Maximize ) 
    {
    direction = 1.0;
    }
  else 
    {
    direction = -1.0;
    }

  const unsigned int spaceDimension = 
    m_CostFunction->GetNumberOfParameters();

  const ParametersType & currentPosition = this->GetCurrentPosition();

  ScalesType scales = this->GetScales();

  DerivativeType transformedGradient( spaceDimension ); 

  for(unsigned int j = 0; j < spaceDimension; j++)
    {
    transformedGradient[j] = m_Gradient[j] / scales[j];
    }

  ParametersType newPosition( spaceDimension );
  for(unsigned int j = 0; j < spaceDimension; j++)
    {
    newPosition[j] = currentPosition[j] + 
      direction * m_LearningRate * transformedGradient[j];
    }

  this->SetCurrentPosition( newPosition );

  this->InvokeEvent( IterationEvent() );

}



} // end namespace itk

#endif
