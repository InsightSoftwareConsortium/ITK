/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration.txx
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
#ifndef _itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration_txx
#define _itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration_txx

#include "itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration()
{ 
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration( const Self & other )
:Superclass( other )
{
  m_Parameters       = other.m_Parameters;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration<TReference,  TTarget>
::~PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration< TReference, TTarget> &
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration< TReference, TTarget>
::operator=( const Self & other )
{
  Superclass::operator=( other );
  m_Parameters       = other.m_Parameters;
  return *this;
}



/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
void
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{ 
  /* Initialize the Offset */ 
  for (unsigned int k=0; k<ParametersDimension; k++)
  { 
    m_Parameters[ k ] = 0;
  }

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransform();

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );

  optimizer->SetCostFunction( this->GetMetric() );
  optimizer->SetMinimize();
  optimizer->SetScale( parametersScale );
  optimizer->SetGradientMagnitudeTolerance( 1e-6 );
  optimizer->SetMaximumStepLength( 30.0 );
  optimizer->SetMinimumStepLength( 1e-6 );
  optimizer->SetNumberOfIterations( 900 );

  optimizer->SetInitialPosition( m_Parameters );
  optimizer->StartOptimization();

  std::cout << "The Solution is : " ;
  m_Parameters = optimizer->GetCurrentPosition();
  std::cout << m_Parameters << std::endl;
  std::cout << std::endl;


}



} // end namespace itk


#endif
