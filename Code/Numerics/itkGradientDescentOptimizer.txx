/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientDescentOptimizer.txx
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
#ifndef _itkGradientDescentOptimizer_txx
#define _itkGradientDescentOptimizer_txx

#include "itkGradientDescentOptimizer.h"
#include "itkCommand.h"

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

  InvokeEvent( Command::StartEvent );
  while( !m_Stop ) 
  {

    try
      {
      m_CostFunction->GetValueAndDerivative( 
        GetCurrentPosition(), m_Value, m_Gradient );
      }
    catch(...)
      {
       // An exception has occurred. 
       // Terminate immediately.
       m_StopCondition = MetricError;
       StopOptimization();

       // Pass exception to caller
       throw;
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
  InvokeEvent( Command::EndEvent );
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
    DerivativeType transformedGradient = 
            GetTransform()->TransformCovariantVector( m_Gradient );
    newPosition[j] = currentPosition[j] + 
      direction * m_LearningRate * transformedGradient[j];
  }

  SetCurrentPosition( newPosition );

  InvokeEvent( Command::IterationEvent );

}



} // end namespace itk

#endif
