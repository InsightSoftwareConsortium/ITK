/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkGradientRecursiveGaussianImageFilter_txx
#define _itkGradientRecursiveGaussianImageFilter_txx

#include "itkGradientRecursiveGaussianImageFilter.h"

namespace itk
{


/**
 * Constructor
 */
template <class TInputImage, class TOutputImage, class TComputation>
GradientRecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation>
::GradientRecursiveGaussianImageFilter()
{

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ] = SmoothingFilterType::New();
  }
  m_DerivativeFilter = DerivativeFilterType::New();
  

  m_SmoothingFilters[0]->SetInput( this->GetInput() );
  for( unsigned int i = 1; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ]->SetInput( 
                              m_SmoothingFilters[i-1]->GetOutput() );
  }
  m_DerivativeFilter->SetInput( 
                       m_SmoothingFilters[ImageDimension-2]->GetOutput() );
  

  m_OutputAdaptor = OutputAdaptorType::New();

  m_OutputAdaptor->SetImage( GetOutput() );

  m_DerivativeFilter->SetOutput( m_OutputAdaptor );

  this->SetSigma( 1.0 );

}



/**
 * Set value of Sigma
 */
template <class TInputImage, class TOutputImage, class TComputation>
void 
GradientRecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation>
::SetSigma( TComputation sigma )
{

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ]->SetSigma( sigma );
  }
  m_DerivativeFilter->SetSigma( sigma );

  this->Modified();

}




/**
 * Compute filter for Gaussian kernel
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
GradientRecursiveGaussianImageFilter<TInputImage,TOutputImage, TComputation>
::GenerateData(void)
{
  std::cout << "Hi, GradientRecursiveGaussianImageFilter generating data ";
  std::cout << std::endl;

  const typename TInputImage::Pointer   inputImage(    GetInput()   );

  m_OutputAdaptor->SetLargestPossibleRegion( 
      inputImage->GetLargestPossibleRegion() );

  m_OutputAdaptor->SetBufferedRegion( 
      inputImage->GetBufferedRegion() );

  m_OutputAdaptor->SetRequestedRegion( 
      inputImage->GetRequestedRegion() );

  m_OutputAdaptor->Allocate();

  for( unsigned int dim=0; dim < ImageDimension; dim++ )
  {
	unsigned int i=0; 
	unsigned int j=0;
    while(  i< ImageDimension)
    {
      if( i == dim ) 
      {
        j++;
      }
      m_SmoothingFilters[ i ]->SetDirection( j );
	  i++;
	  j++;
    }
    m_DerivativeFilter->SetDirection( dim );
    m_OutputAdaptor->SelectNthElement( dim );
    m_DerivativeFilter->Update();
  }
  

}


} // end namespace itk

#endif
