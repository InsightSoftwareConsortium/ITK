/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientMagnitudeRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGradientMagnitudeRecursiveGaussianImageFilter_txx
#define _itkGradientMagnitudeRecursiveGaussianImageFilter_txx

#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{


/**
 * Constructor
 */
template <typename TInputImage, typename TOutputImage >
GradientMagnitudeRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::GradientMagnitudeRecursiveGaussianImageFilter()
{

  m_NormalizeAcrossScale = false;

  m_ProgressCommand = CommandType::New();
  m_ProgressCommand->SetCallbackFunction( this, & Self::ReportProgress );
  m_Progress  = 0.0f;

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ] = GaussianFilterType::New();
    m_SmoothingFilters[ i ]->SetOrder( GaussianFilterType::ZeroOrder );
    m_SmoothingFilters[ i ]->SetNormalizeAcrossScale( m_NormalizeAcrossScale );
    m_SmoothingFilters[ i ]->AddObserver( ProgressEvent(), m_ProgressCommand );
  }

  m_DerivativeFilter = DerivativeFilterType::New();
  m_DerivativeFilter->SetOrder( DerivativeFilterType::FirstOrder );
  m_DerivativeFilter->SetNormalizeAcrossScale( m_NormalizeAcrossScale );
  m_DerivativeFilter->AddObserver( ProgressEvent(), m_ProgressCommand );

  m_DerivativeFilter->SetInput( this->GetInput() );

  m_SmoothingFilters[0]->SetInput( m_DerivativeFilter->GetOutput() );

  for( unsigned int i = 1; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ]->SetInput( 
                              m_SmoothingFilters[i-1]->GetOutput() );
  }
  
  m_CumulativeImage = CumulativeImageType::New();

  this->SetSigma( 1.0 );

}



/**
 *  Report progress by weigthing contributions of internal filters
 */
template <typename TInputImage, typename TOutputImage>
void 
GradientMagnitudeRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::ReportProgress(const Object * object, const EventObject & event )
{
   const ProcessObject * internalFilter = 
            dynamic_cast<const ProcessObject *>( object );

   if( typeid( event ) == typeid( ProgressEvent() ) )
     {
     const float filterProgress    = internalFilter->GetProgress();
     const float weightedProgress  = filterProgress / ImageDimension;
     m_Progress += weightedProgress;
     this->UpdateProgress( m_Progress );
     }
}



/**
 * Set value of Sigma
 */
template <typename TInputImage, typename TOutputImage>
void 
GradientMagnitudeRecursiveGaussianImageFilter<TInputImage,TOutputImage>
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
 * Set Normalize Across Scale Space
 */
template <typename TInputImage, typename TOutputImage>
void 
GradientMagnitudeRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::SetNormalizeAcrossScale( bool normalize )
{

  m_NormalizeAcrossScale = normalize;

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
  {
    m_SmoothingFilters[ i ]->SetNormalizeAcrossScale( normalize );
  }
  m_DerivativeFilter->SetNormalizeAcrossScale( normalize );

  this->Modified();

}


/**
 * Compute filter for Gaussian kernel
 */
template <typename TInputImage, typename TOutputImage >
void
GradientMagnitudeRecursiveGaussianImageFilter<TInputImage,TOutputImage >
::GenerateData(void)
{

  itkDebugMacro(<< "GradientMagnitudeRecursiveGaussianImageFilter generating data ");

  m_Progress = 0.0f;

  const typename TInputImage::ConstPointer   inputImage( this->GetInput() );

  typename TOutputImage::Pointer outputImage( this->GetOutput() );

  outputImage = this->GetOutput();

  outputImage->SetRegions( inputImage->GetBufferedRegion() );

  outputImage->Allocate();

  m_CumulativeImage->SetRegions( inputImage->GetBufferedRegion() );
  m_CumulativeImage->Allocate();
  m_CumulativeImage->FillBuffer( NumericTraits< InternalRealType >::Zero );

  m_DerivativeFilter->SetInput( inputImage );

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

    GaussianFilterPointer lastFilter = m_SmoothingFilters[ImageDimension-2];

    lastFilter->Update();
    

    // Cummulate the results on the output image

    typename RealImageType::Pointer derivativeImage = lastFilter->GetOutput(); 

    ImageRegionIteratorWithIndex< RealImageType > it( 
                                      derivativeImage, 
                                      derivativeImage->GetRequestedRegion() );

    ImageRegionIteratorWithIndex< CumulativeImageType > ot( 
                                      m_CumulativeImage, 
                                      m_CumulativeImage->GetRequestedRegion() );
  
    const RealType spacing = inputImage->GetSpacing()[ dim ];

    it.GoToBegin();
    ot.GoToBegin();
    while( !it.IsAtEnd() )
      {
      const RealType value = it.Get() / spacing;
      const RealType cumulated = ot.Get() + value * value;
      ot.Set( cumulated );
      ++it;
      ++ot;
      }

  }
  

  // Finally convert the cumulated image to the output by 
  // taking the square root of the pixels.
  ImageRegionIteratorWithIndex< OutputImageType > ot( 
                                    outputImage, 
                                    outputImage->GetRequestedRegion() );

  ImageRegionIteratorWithIndex< CumulativeImageType > it( 
                                    m_CumulativeImage, 
                                    m_CumulativeImage->GetRequestedRegion() );

  it.GoToBegin();
  ot.GoToBegin();
  while( !it.IsAtEnd() )
    {
    ot.Set( static_cast<OutputPixelType>( vcl_sqrt( it.Get() ) ) );
    ++it;
    ++ot;
    }



}


template <typename TInputImage, typename TOutputImage>
void
GradientMagnitudeRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
}


} // end namespace itk

#endif
