/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration_txx
#define _itkImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration_txx

#include "itkImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration<TReference, TTarget>
::ImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration()
{ 
  m_TranslationScale = 100.0; 

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  optimizer->SetGradientMagnitudeTolerance( 1e-9 );
  optimizer->SetMaximumStepLength( 1e-3  );
  optimizer->SetMinimumStepLength( 1e-6 );
  optimizer->SetNumberOfIterations( 100 );


}


/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration()
{
}


/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
void
ImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{ 

  /* Initialize the Offset */ 
  for (unsigned int k=0; k<ParametersDimension; k++)
  { 
    m_Parameters[ k ] = 0;
  }

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransform();


  ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  optimizer->SetCostFunction( this->GetMetric() );
  optimizer->MinimizeOn();
  optimizer->GetTransform()->SetScale( parametersScale );
  optimizer->SetInitialPosition( m_Parameters );
  optimizer->StartOptimization();


}



} // end namespace itk


#endif
