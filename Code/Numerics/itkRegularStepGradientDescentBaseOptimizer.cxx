/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentBaseOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRegularStepGradientDescentBaseOptimizer_txx
#define _itkRegularStepGradientDescentBaseOptimizer_txx

#include "itkRegularStepGradientDescentBaseOptimizer.h"
#include "itkCommand.h"
#include "itkEventObject.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 * Constructor
 */
RegularStepGradientDescentBaseOptimizer
::RegularStepGradientDescentBaseOptimizer()
{

  itkDebugMacro("Constructor");
      
  m_MaximumStepLength = 1.0;
  m_MinimumStepLength = 1e-3;
  m_GradientMagnitudeTolerance = 1e-4;
  m_NumberOfIterations = 100;
  m_CurrentIteration   =   0;
  m_Maximize = false;

  m_Gradient.Fill( 0.0f );
  m_PreviousGradient.Fill( 0.0f );
  
}



/**
 * Start the optimization
 */
void
RegularStepGradientDescentBaseOptimizer
::StartOptimization( void )
{

  itkDebugMacro("StartOptimization");

  m_CurrentStepLength         = m_MaximumStepLength;
  m_CurrentIteration          = 0;

  const unsigned int spaceDimension = 
              m_CostFunction->GetNumberOfParameters();

  m_Gradient = DerivativeType( spaceDimension );
  m_PreviousGradient = DerivativeType( spaceDimension );
  m_Gradient.Fill( 0.0f );
  m_PreviousGradient.Fill( 0.0f );

  this->SetCurrentPosition( GetInitialPosition() );
  this->ResumeOptimization();

}





/**
 * Resume the optimization
 */
void
RegularStepGradientDescentBaseOptimizer
::ResumeOptimization( void )
{
  
  itkDebugMacro("ResumeOptimization");

  m_Stop = false;

  this->InvokeEvent( StartEvent() );

  while( !m_Stop ) 
  {

    ParametersType currentPosition = this->GetCurrentPosition();
    m_Value = m_CostFunction->GetValue( currentPosition );

    if( m_Stop )
    {
      break;
    }

    m_PreviousGradient = m_Gradient;
    m_CostFunction->GetDerivative( currentPosition, m_Gradient );

    if( m_Stop )
    {
      break;
    }

    this->AdvanceOneStep();

    m_CurrentIteration++;

    if( m_CurrentIteration == m_NumberOfIterations )
    {
       m_StopCondition = MaximumNumberOfIterations;
       this->StopOptimization();
       break;
    }
    
  }
    

}





/**
 * Stop optimization
 */
void
RegularStepGradientDescentBaseOptimizer
::StopOptimization( void )
{

  itkDebugMacro("StopOptimization");
  
  m_Stop = true;
  this->InvokeEvent( EndEvent() );
}




/**
 * Advance one Step following the gradient direction
 */
void
RegularStepGradientDescentBaseOptimizer
::AdvanceOneStep( void )
{ 

  itkDebugMacro("AdvanceOneStep");

  const unsigned int  spaceDimension =
                m_CostFunction->GetNumberOfParameters();

  DerivativeType transformedGradient( spaceDimension );
  DerivativeType previousTransformedGradient( spaceDimension );
  ScalesType     scales = this->GetScales();

  for(unsigned int i = 0;  i < spaceDimension; i++)
    {
    transformedGradient[i]  = m_Gradient[i] / scales[i];    
    previousTransformedGradient[i] = 
                      m_PreviousGradient[i] / scales[i];    
    }

  double magnitudeSquare = 0;
  for(unsigned int dim=0; dim<spaceDimension; dim++)
  {
    const double weighted = transformedGradient[dim];
    magnitudeSquare += weighted * weighted;
  }
    
  const double gradientMagnitude = vcl_sqrt( magnitudeSquare );

  if( gradientMagnitude < m_GradientMagnitudeTolerance ) 
  {
    m_StopCondition = GradientMagnitudeTolerance;
    StopOptimization();
    return;
  }
    
  double scalarProduct = 0;

  for(unsigned int i=0; i<spaceDimension; i++)
  {
    const double weight1 = transformedGradient[i];
    const double weight2 = previousTransformedGradient[i];
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

  double direction;
  if( this->m_Maximize ) 
  {
    direction = 1.0;
  }
  else 
  {
    direction = -1.0;
  }

  const double factor = 
                direction * m_CurrentStepLength / gradientMagnitude;

  // This method StepAlongGradient() will 
  // be overloaded in non-vector spaces
  this->StepAlongGradient( factor, transformedGradient );



  this->InvokeEvent( IterationEvent() );

}

void
RegularStepGradientDescentBaseOptimizer
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "MaximumStepLength: "
     << m_MaximumStepLength << std::endl;
  os << indent << "MinimumStepLength: "
     << m_MinimumStepLength << std::endl;
  os << indent << "GradientMagnitudeTolerance: "
     << m_GradientMagnitudeTolerance << std::endl;
  os << indent << "NumberOfIterations: "
     << m_NumberOfIterations << std::endl;
  os << indent << "CurrentIteration: "
     << m_CurrentIteration   << std::endl;
  os << indent << "Value: "
     << m_Value << std::endl;
  os << indent << "Maximize: "
     << m_Maximize << std::endl;
  os << indent << "CostFunction: "
     << m_CostFunction << std::endl;
  os << indent << "CurrentStepLength: "
     << m_CurrentStepLength << std::endl;
  os << indent << "StopCondition: "
     << m_StopCondition << std::endl;
}
} // end namespace itk

#endif
