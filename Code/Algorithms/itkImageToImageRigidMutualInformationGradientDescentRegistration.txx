/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageRigidMutualInformationGradientDescentRegistration.txx
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
#ifndef _itkImageToImageRigidMutualInformationGradientDescentRegistration_txx
#define _itkImageToImageRigidMutualInformationGradientDescentRegistration_txx

#include "itkImageToImageRigidMutualInformationGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageRigidMutualInformationGradientDescentRegistration<TReference, TTarget>
::ImageToImageRigidMutualInformationGradientDescentRegistration()
{

  // initialize the parameter to be the identity transform
  m_Parameters.Fill( 0.0 );
  m_Parameters[3] = 1.0;

  // set default parameters
  m_NumberOfIterations = 1000;
  m_LearningRate = 1.0;
  m_TranslationScale = 1.0;

}


/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageRigidMutualInformationGradientDescentRegistration<TReference, TTarget>
::ImageToImageRigidMutualInformationGradientDescentRegistration( const Self & other )
:Superclass( other )
{
  m_Parameters = other.m_Parameters;
  m_TranslationScale = other.m_TranslationScale;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageRigidMutualInformationGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageRigidMutualInformationGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const ImageToImageRigidMutualInformationGradientDescentRegistration< TReference, TTarget> &
ImageToImageRigidMutualInformationGradientDescentRegistration< TReference, TTarget>
::operator=( const Self & other )
{
  Superclass::operator=( other );
  m_Parameters = other.m_Parameters;
  m_TranslationScale = other.m_TranslationScale;
  return *this;
}


/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
int
ImageToImageRigidMutualInformationGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{

  this->GetMetric()->GetMapper()->GetTransformation()->
   SetTranslationScale( m_TranslationScale );

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  optimizer->SetCostFunction( this->GetMetric() );

  // setup the optimizer
  optimizer->SetMaximize();
  optimizer->SetLearningRate( m_LearningRate );
  optimizer->SetNumberOfIterations( m_NumberOfIterations );
  optimizer->SetInitialPosition( m_Parameters );

  // do the optimization
  optimizer->StartOptimization();

  // get the results
  m_Parameters = optimizer->GetCurrentPosition();

  return 0;

}


} // end namespace itk


#endif
