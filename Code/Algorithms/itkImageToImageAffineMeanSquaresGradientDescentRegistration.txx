/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineMeanSquaresGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageToImageAffineMeanSquaresGradientDescentRegistration_txx
#define _itkImageToImageAffineMeanSquaresGradientDescentRegistration_txx

#include "itkImageToImageAffineMeanSquaresGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMeanSquaresGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffineMeanSquaresGradientDescentRegistration()
{ 
  m_TranslationScale = 100.0; 
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMeanSquaresGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffineMeanSquaresGradientDescentRegistration( const Self & other )
  :Superclass( other )
{
  m_Parameters       = other.m_Parameters;
  m_TranslationScale = other.m_TranslationScale;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMeanSquaresGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageAffineMeanSquaresGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const ImageToImageAffineMeanSquaresGradientDescentRegistration< TReference, TTarget> &
ImageToImageAffineMeanSquaresGradientDescentRegistration< TReference, TTarget>
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
ImageToImageAffineMeanSquaresGradientDescentRegistration<TReference, TTarget>
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

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransformation();

  transformation->SetTranslationScale( m_TranslationScale );

  ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  optimizer->SetCostFunction( this->GetMetric() );
  optimizer->SetMinimize();
  optimizer->SetScale( parametersScale );

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
