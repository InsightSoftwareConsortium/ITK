/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientDescentOptimizer.txx
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
template <class TCostFunction>
GradientDescentOptimizer<TCostFunction>
::GradientDescentOptimizer()
{
   m_LearningRate = 1.0;
   m_NumberOfIterations = 100;
   m_CurrentIteration = 0;
}



template <class TCostFunction>
void
GradientDescentOptimizer<TCostFunction>
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
template <class TCostFunction>
void
GradientDescentOptimizer<TCostFunction>
::StartOptimization( void )
{

  m_CurrentIteration   = 0;

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

  InvokeEvent( StartEvent() );
  while( !m_Stop ) 
  {

    try
      {
      m_CostFunction->GetValueAndDerivative( 
        GetCurrentPosition(), m_Value, m_Gradient );
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
template <class TCostFunction>
void
GradientDescentOptimizer<TCostFunction>
::StopOptimization( void )
{
  m_Stop = true;
  InvokeEvent( EndEvent() );
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

  ParametersType newPosition(SpaceDimension);
  const ParametersType & currentPosition = GetCurrentPosition();

  CovariantVector<double,SpaceDimension> gradient; 

  for(unsigned int j=0; j<SpaceDimension; j++)
    {
    gradient[j] = m_Gradient[j];
    }

  CovariantVector<double,SpaceDimension> transformedGradient = 
                GetTransform()->TransformCovariantVector( gradient );

  for(unsigned int j=0; j<SpaceDimension; j++)
    {
    newPosition[j] = currentPosition[j] + 
      direction * m_LearningRate * transformedGradient[j];
    }

  SetCurrentPosition( newPosition );

  InvokeEvent( IterationEvent() );

}



} // end namespace itk

#endif
