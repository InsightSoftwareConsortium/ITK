/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineNormalizedCorrelationRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToImageAffineNormalizedCorrelationRegularStepGradientDescentRegistration_txx
#define _itkImageToImageAffineNormalizedCorrelationRegularStepGradientDescentRegistration_txx

#include "itkImageToImageAffineNormalizedCorrelationRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineNormalizedCorrelationRegularStepGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffineNormalizedCorrelationRegularStepGradientDescentRegistration()
{ 
  m_TranslationScale = 100.0; 
}


/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageAffineNormalizedCorrelationRegularStepGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageAffineNormalizedCorrelationRegularStepGradientDescentRegistration()
{
}


/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
void
ImageToImageAffineNormalizedCorrelationRegularStepGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{ 
  
  /* Initialize the rotation / shear */
  unsigned int k = 0;
  for (unsigned int col=0; col<ImageDimension; col++)
  {
    for (unsigned int row=0; row<ImageDimension; row++)
    { 
      if( col == row ) 
      {
        m_Parameters[ k++ ] = 1.0;
      }
      else 
      {
        m_Parameters[ k++ ] = 0.0;
      }
    }
  }

  /* Initialize the Offset */ 
  for (unsigned int coff=0; coff<ImageDimension; coff++)
  {
    m_Parameters[ k++ ] = 0.0;
  }


  typename OptimizerType::TransformType::ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );
  for(unsigned int trans=ImageDimension*ImageDimension; 
      trans<ImageDimension*(ImageDimension+1); trans++)
  {
    parametersScale[trans] = m_TranslationScale;
  }


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
