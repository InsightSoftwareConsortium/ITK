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
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{


/**
 * Constructor
 */
template <typename TInputImage, typename TOutputImage >
GradientRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::GradientRecursiveGaussianImageFilter()
{

  m_NormalizeAcrossScale = false;

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ] = GaussianFilterType::New();
    m_SmoothingFilters[ i ]->SetOrder( GaussianFilterType::ZeroOrder );
    m_SmoothingFilters[ i ]->SetNormalizeAcrossScale( m_NormalizeAcrossScale );
  }

  m_DerivativeFilter = GaussianFilterType::New();
  m_DerivativeFilter->SetOrder( GaussianFilterType::FirstOrder );
  m_DerivativeFilter->SetNormalizeAcrossScale( m_NormalizeAcrossScale );
  

  m_SmoothingFilters[0]->SetInput( this->GetInput() );
  for( unsigned int i = 1; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ]->SetInput( 
                              m_SmoothingFilters[i-1]->GetOutput() );
  }
  m_DerivativeFilter->SetInput( 
                       m_SmoothingFilters[ImageDimension-2]->GetOutput() );
  
  m_ImageAdaptor = OutputImageAdaptorType::New();

  this->SetSigma( 1.0 );

}



/**
 * Set value of Sigma
 */
template <typename TInputImage, typename TOutputImage>
void 
GradientRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::SetSigma( RealType sigma )
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
template <typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter<TInputImage,TOutputImage >
::GenerateData(void)
{

  itkDebugMacro(<< "GradientRecursiveGaussianImageFilter generating data ");

  const typename TInputImage::ConstPointer   inputImage( this->GetInput() );

  m_ImageAdaptor->SetImage( this->GetOutput() );

  m_ImageAdaptor->SetLargestPossibleRegion( 
                    inputImage->GetLargestPossibleRegion() );

  m_ImageAdaptor->SetBufferedRegion( 
                    inputImage->GetBufferedRegion() );

  m_ImageAdaptor->SetRequestedRegion( 
                    inputImage->GetRequestedRegion() );

  m_ImageAdaptor->Allocate();

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
    m_DerivativeFilter->Update();
    

    // Copy the results to the corresponding component
    // on the output image of vectors
    m_ImageAdaptor->SelectNthElement( dim );

    typename TInputImage::Pointer derivativeImage = 
                                    m_DerivativeFilter->GetOutput(); 

    ImageRegionIteratorWithIndex< TInputImage > it( 
                                      derivativeImage, 
                                      derivativeImage->GetRequestedRegion() );

    ImageRegionIteratorWithIndex< OutputImageAdaptorType > ot( 
                                      m_ImageAdaptor, 
                                      m_ImageAdaptor->GetRequestedRegion() );
  
    const RealType spacing = inputImage->GetSpacing()[ dim ];

    it.GoToBegin();
    ot.GoToBegin();
    while( !it.IsAtEnd() )
      {
      ot.Set( it.Get() / spacing );
      ++it;
      ++ot;
      }

  }
  

}


template <typename TInputImage, typename TOutputImage>
void
GradientRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
}


} // end namespace itk

#endif
