/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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

  itkDebugMacro(<< "Hi, GradientRecursiveGaussianImageFilter generating data ");

  const typename TInputImage::Pointer   inputImage( GetInput() );

  OutputImagePointer    outputImage   = TOutputImage::New();
  OutputAdaptorPointer  outputAdaptor = m_DerivativeFilter->GetOutput();

  outputImage->ReleaseData();  // Free any data from previous execution

  outputAdaptor->SetImage( outputImage );
  


  outputAdaptor->SetLargestPossibleRegion( 
      inputImage->GetLargestPossibleRegion() );

  outputAdaptor->SetBufferedRegion( 
      inputImage->GetBufferedRegion() );

  outputAdaptor->SetRequestedRegion( 
      inputImage->GetRequestedRegion() );

  outputAdaptor->Allocate();

  m_SmoothingFilters[0]->SetInput( inputImage );

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
    outputAdaptor->SelectNthElement( dim );
    m_DerivativeFilter->Update();
    
  }
  
  // Reconnect the image to the output
  this->GraftOutput( outputImage );

}


} // end namespace itk

#endif
