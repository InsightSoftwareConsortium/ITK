/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageTranslationMeanSquaresRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToImageTranslationMeanSquaresRegularStepGradientDescentRegistration_txx
#define _itkImageToImageTranslationMeanSquaresRegularStepGradientDescentRegistration_txx

#include "itkImageToImageTranslationMeanSquaresRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageTranslationMeanSquaresRegularStepGradientDescentRegistration<TReference, TTarget>
::ImageToImageTranslationMeanSquaresRegularStepGradientDescentRegistration()
{ 
  m_TranslationScale = 100.0; 
}


/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageTranslationMeanSquaresRegularStepGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageTranslationMeanSquaresRegularStepGradientDescentRegistration()
{
}


/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
void
ImageToImageTranslationMeanSquaresRegularStepGradientDescentRegistration<TReference, TTarget>
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
