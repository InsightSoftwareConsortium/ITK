/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration_txx
#define _itkPointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration_txx

#include "itkPointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration.h"


namespace itk
{

/*
 * Constructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration<TReference, TTarget>
::PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration()
{ 
}


/*
 * Destructor
 */
template <class TReference, class TTarget>
PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration<TReference,  TTarget>
::~PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration()
{
}


/*
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
void
PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{ 
  /* Initialize the Offset */ 
  for (unsigned int k=0; k<ParametersDimension; k++)
  { 
    m_Parameters[ k ] = 0;
  }


  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  typename OptimizerType::TransformType::ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );

  optimizer->SetCostFunction( this->GetMetric() );
  optimizer->MinimizeOn();
  optimizer->GetTransform()->SetScale( parametersScale );

  optimizer->SetInitialPosition( m_Parameters );
  optimizer->StartOptimization();



}



} // end namespace itk


#endif
