/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration<TReference, TTarget>
::ImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration( const Self & other )
  :Superclass( other )
{
  m_Parameters       = other.m_Parameters;
  m_TranslationScale = other.m_TranslationScale;
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
 * Assignment Operator
 */
template <class TReference, class TTarget>
const ImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration< TReference, TTarget> &
ImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration< TReference, TTarget>
::operator=( const Self & other )
{
  Superclass::operator=( other );
  m_Parameters       = other.m_Parameters;
  m_TranslationScale = other.m_TranslationScale;
  return *this;
}




/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
int
ImageToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{ 

  /* Initialize the Offset */ 
  for (unsigned int k=0; k<ParametersDimension; k++)
  { 
    m_Parameters[ k ] = 0;
  }

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransformation();


  ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  optimizer->SetCostFunction( this->GetMetric() );
  optimizer->SetMinimize();
  optimizer->SetScale( parametersScale );
  optimizer->SetGradientMagnitudeTolerance( 1e-9 );
  optimizer->SetMaximumStepLength( 1e-3  );
  optimizer->SetMinimumStepLength( 1e-6 );
  optimizer->SetMaximumNumberOfIterations( 100 );

  optimizer->SetInitialPosition( m_Parameters );
  optimizer->StartOptimization();

  std::cout << "The Solution is : " ;
  m_Parameters = optimizer->GetCurrentPosition();
  const unsigned int offsetStart = ImageDimension * ImageDimension;
  for(unsigned int k=0; k<ImageDimension; k++)
  {
    m_Parameters[ offsetStart + k ] *= m_TranslationScale;
  }
  std::cout << m_Parameters << std::endl;
  std::cout << std::endl;

return 0;
}



} // end namespace itk


#endif
