/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration_txx
#define _itkImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration_txx

#include "itkImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration()
{ 
  m_TranslationScale = 100.0; 
}

/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration()
{
}

/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
void
ImageToImageAffinePatternIntensityRegularStepGradientDescentRegistration<TReference, TTarget>
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
