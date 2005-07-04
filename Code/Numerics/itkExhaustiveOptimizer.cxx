/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExhaustiveOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkExhaustiveOptimizer.h"
#include "itkCommand.h"
#include "itkEventObject.h"

namespace itk
{

/**
 * Constructor
 */
ExhaustiveOptimizer
::ExhaustiveOptimizer()
{

  itkDebugMacro("Constructor");
      
  m_StepLength = 1.0;
  m_CurrentIteration   =   0;
  m_CurrentValue = 0;
  m_CurrentIndex.Fill(0);
  m_Stop = false;
  m_NumberOfSteps.Fill(0);
}

/**
 * Start walking
 **/

void ExhaustiveOptimizer::StartOptimization( void )
{
  this->StartWalking();
}


void
ExhaustiveOptimizer
::StartWalking( void )
{

  itkDebugMacro("StartWalking");
  this->InvokeEvent( StartEvent() );

  m_MinimumMetricValuePosition = this->GetInitialPosition();
  m_MaximumMetricValuePosition = this->GetInitialPosition();

  m_MaximumMetricValue = this->GetValue( m_MaximumMetricValuePosition );
  m_MinimumMetricValue = this->GetValue( m_MinimumMetricValuePosition );
  
  m_CurrentIteration          = 0;
  m_MaximumNumberOfIterations = 1;
  
  const unsigned int spaceDimension = this->GetInitialPosition().GetSize();

  for (unsigned int i=0; i< spaceDimension; i++)
    {
    m_MaximumNumberOfIterations *= (2 * m_NumberOfSteps[i] + 1);
    }
    
  m_CurrentIndex.SetSize(spaceDimension);
  m_CurrentIndex.Fill(0);
  
  this->SetCurrentPosition( this->GetInitialPosition() );

  
  itkDebugMacro("Calling ResumeWalking");
  
  this->ResumeWalking();

}





/**
 * Resume the optimization
 */
void
ExhaustiveOptimizer
::ResumeWalking( void )
{
  itkDebugMacro("ResumeWalk");
  m_Stop = false;
 
  while( !m_Stop ) 
    {
    ParametersType currentPosition = this->GetCurrentPosition();
    
    if( m_Stop )
      {
      StopWalking();
      break;
      }

    m_CurrentValue = this->GetValue( currentPosition );
    
    if (m_CurrentValue > m_MaximumMetricValue) 
      {
      m_MaximumMetricValue = m_CurrentValue;
      m_MaximumMetricValuePosition = currentPosition;
      }
    if (m_CurrentValue < m_MinimumMetricValue) 
      {
      m_MinimumMetricValue = m_CurrentValue;
      m_MinimumMetricValuePosition = currentPosition;
      }
     

    
    if( m_Stop )
      {
      this->StopWalking();
      break;
      }

    this->AdvanceOneStep();

    m_CurrentIteration++;

    }
}


void
ExhaustiveOptimizer
::StopWalking( void )
{

  itkDebugMacro("StopWalking");
  
  m_Stop = true;
  this->InvokeEvent( EndEvent() );
}



void
ExhaustiveOptimizer
::AdvanceOneStep( void )
{ 

  itkDebugMacro("AdvanceOneStep");

  const unsigned int  spaceDimension =
    m_CostFunction->GetNumberOfParameters();

  ScalesType     scales = this->GetScales();


  // Make sure the scales have been set properly
  if (scales.size() != spaceDimension)
    {
    itkExceptionMacro(<< "The size of Scales is "
                      << scales.size()
                      << ", but the NumberOfParameters is "
                      << spaceDimension
                      << ".");
    }


  ParametersType newPosition( spaceDimension );
  ParametersType currentPosition = this->GetCurrentPosition();

  IncrementIndex( newPosition );

  
  itkDebugMacro(<<"new position = " << newPosition );

  
  this->InvokeEvent( IterationEvent() );
  this->SetCurrentPosition( newPosition );

}

void
ExhaustiveOptimizer
::IncrementIndex( ParametersType &newPosition ) 
{

  unsigned int idx = 0;

  const unsigned int  spaceDimension =
    m_CostFunction->GetNumberOfParameters();

  while( idx < spaceDimension )
    {
    m_CurrentIndex[idx]++;

    if( m_CurrentIndex[idx] > (2*m_NumberOfSteps[idx]))
      {
      m_CurrentIndex[idx]=0;
      idx++;
      }
    else
      {
      break;
      }
    }
      
  if( idx==spaceDimension )
    {
    m_Stop = true;
    }

  for(unsigned int i=0; i<spaceDimension; i++)
    {
    newPosition[i] = (m_CurrentIndex[i]-m_NumberOfSteps[i]) *
                      m_StepLength * this->GetScales()[i] + 
                      this->GetInitialPosition()[i];
    }
}



void
ExhaustiveOptimizer
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "CurrentValue = " << m_CurrentValue << std::endl;
  os << indent << "NumberOfSteps = " << m_NumberOfSteps << std::endl;
  os << indent << "CurrentIteration = " << m_CurrentIteration << std::endl;
  os << indent << "Stop = " << m_Stop << std::endl;
  os << indent << "CurrentParameter = " << m_CurrentParameter << std::endl;
  os << indent << "StepLength = " << m_StepLength << std::endl; 
  os << indent << "CurrentIndex = " << m_CurrentIndex << std::endl;
  os << indent << "MaximumNumberOfIterations = " << m_MaximumNumberOfIterations << std::endl;
  os << indent << "MaximumMetricValue = " << m_MaximumMetricValue << std::endl;
  os << indent << "MinimumMetricValue = " << m_MinimumMetricValue << std::endl;
  os << indent << "MinimumMetricValuePosition = " << m_MinimumMetricValuePosition << std::endl;
  os << indent << "MaximumMetricValuePosition = " << m_MaximumMetricValuePosition << std::endl;
}

 
} // end namespace itk


