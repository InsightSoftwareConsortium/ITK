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
    m_SmoothingFilters[ i ]->SetInput( m_SmoothingFilters[i-1]->GetOutput() );
  }
  m_DerivativeFilter->SetInput( m_SmoothingFilters[ImageDimension-2]->GetOutput() );
  
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
        typename TOutputImage::Pointer  outputImage(   GetOutput()  );

  outputImage->SetLargestPossibleRegion( 
      inputImage->GetLargestPossibleRegion() );

  outputImage->SetBufferedRegion( 
      inputImage->GetBufferedRegion() );

  outputImage->SetRequestedRegion( 
      inputImage->GetRequestedRegion() );

  outputImage->Allocate();


}


} // end namespace itk

#endif
