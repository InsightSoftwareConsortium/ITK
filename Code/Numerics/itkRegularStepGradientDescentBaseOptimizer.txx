/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentBaseOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkRegularStepGradientDescentBaseOptimizer_txx
#define _itkRegularStepGradientDescentBaseOptimizer_txx

#include "itkRegularStepGradientDescentBaseOptimizer.h"
#include "itkCommand.h"
#include "itkEventObject.h"

namespace itk
{

/**
 * Constructor
 */
template <class TCostFunction>
RegularStepGradientDescentBaseOptimizer<TCostFunction>
::RegularStepGradientDescentBaseOptimizer()
{

  m_MaximumStepLength = 1.0;
  m_MinimumStepLength = 1e-3;
  m_GradientMagnitudeTolerance = 1e-4;
  m_NumberOfIterations = 100;
  m_CurrentIteration   =   0;

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
RegularStepGradientDescentBaseOptimizer<TCostFunction>
::StartOptimization( void )
{

  m_CurrentStepLength         = m_MaximumStepLength;
  m_CurrentIteration          = 0;

  this->SetCurrentPosition( GetInitialPosition() );
  this->ResumeOptimization();

}





/**
 * Resume the optimization
 */
template <class TCostFunction>
void
RegularStepGradientDescentBaseOptimizer<TCostFunction>
::ResumeOptimization( void )
{
  
  m_Stop = false;

  InvokeEvent( StartEvent() );

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

    this->AdvanceOneStep();

    m_CurrentIteration++;

    if( m_CurrentIteration == m_NumberOfIterations )
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
RegularStepGradientDescentBaseOptimizer<TCostFunction>
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
RegularStepGradientDescentBaseOptimizer<TCostFunction>
::AdvanceOneStep( void )
{ 

  DerivativeType transformedGradient =
    GetTransform()->TransformCovariantVector( m_Gradient );

  DerivativeType previousTransformedGradient =
    GetTransform()->TransformCovariantVector( m_PreviousGradient );

  double magnitudeSquare = 0;
  for(unsigned int dim=0; dim<SpaceDimension; dim++)
  {
    const double weighted = transformedGradient[dim];
    magnitudeSquare += weighted * weighted;
  }
    
  const double gradientMagnitude = sqrt( magnitudeSquare );

  if( gradientMagnitude < m_GradientMagnitudeTolerance ) 
  {
    m_StopCondition = GradientMagnitudeTolerance;
    StopOptimization();
    return;
  }
    
  double scalarProduct = 0;

  for(unsigned int i=0; i<SpaceDimension; i++)
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

  double direction = 1.0;
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

template <class TCostFunction>
void
RegularStepGradientDescentBaseOptimizer<TCostFunction>
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
