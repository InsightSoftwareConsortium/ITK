/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientMagnitudeRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGradientMagnitudeRecursiveGaussianImageFilter_txx
#define _itkGradientMagnitudeRecursiveGaussianImageFilter_txx

#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressAccumulator.h"

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

  m_DerivativeFilter = DerivativeFilterType::New();
  m_DerivativeFilter->SetOrder( DerivativeFilterType::FirstOrder );
  m_DerivativeFilter->SetNormalizeAcrossScale( m_NormalizeAcrossScale );

  m_DerivativeFilter->ReleaseDataFlagOn();

  for( unsigned int i = 0; i<ImageDimension-1; i++ )
    {
    m_SmoothingFilters[ i ] = GaussianFilterType::New();
    m_SmoothingFilters[ i ]->SetOrder( GaussianFilterType::ZeroOrder );
    m_SmoothingFilters[ i ]->SetNormalizeAcrossScale( m_NormalizeAcrossScale );
    m_SmoothingFilters[ i ]->ReleaseDataFlagOn();
    }

  m_SmoothingFilters[0]->SetInput( m_DerivativeFilter->GetOutput() );
  for( unsigned int i = 1; i<ImageDimension-1; i++ )
    {
    m_SmoothingFilters[ i ]->SetInput( m_SmoothingFilters[i-1]->GetOutput() );
    }
  
  this->SetSigma( 1.0 );

  this->InPlaceOff();
}


template <typename TInputImage, typename TOutputImage>
void
GradientMagnitudeRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
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


//
//
//
template <typename TInputImage, typename TOutputImage>
void
GradientMagnitudeRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  typename GradientMagnitudeRecursiveGaussianImageFilter<TInputImage,TOutputImage>::InputImagePointer image = const_cast<InputImageType *>( this->GetInput() );
  if( image )
    {
    image->SetRequestedRegion( this->GetInput()->GetLargestPossibleRegion() );
    }
}


//
//
//
template <typename TInputImage, typename TOutputImage>
void
GradientMagnitudeRecursiveGaussianImageFilter<TInputImage,TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  TOutputImage *out = dynamic_cast<TOutputImage*>(output);

  if (out)
    {
    out->SetRequestedRegion( out->GetLargestPossibleRegion() );
    }
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

  const typename TInputImage::ConstPointer   inputImage( this->GetInput() );

  typename TOutputImage::Pointer outputImage( this->GetOutput() );

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  typename CumulativeImageType::Pointer cumulativeImage = CumulativeImageType::New();
  cumulativeImage->SetRegions( inputImage->GetBufferedRegion() );
  cumulativeImage->Allocate();
  cumulativeImage->FillBuffer( NumericTraits< InternalRealType >::Zero );

  m_DerivativeFilter->SetInput( inputImage );

  const unsigned int numberOfFilterRuns = ImageDimension * ImageDimension;
  progress->RegisterInternalFilter(m_DerivativeFilter, 1.0f / numberOfFilterRuns );

  for( unsigned int k=0; k < ImageDimension-1; k++ )
    {
    progress->RegisterInternalFilter( m_SmoothingFilters[k], 1.0f / numberOfFilterRuns );
    }

  for( unsigned int dim=0; dim < ImageDimension; dim++ )
    {
    unsigned int i=0; 
    unsigned int j=0;
    while(  i < ImageDimension-1 )
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
    
    progress->ResetFilterProgressAndKeepAccumulatedProgress();

    // Cummulate the results on the output image

    typename RealImageType::Pointer derivativeImage = lastFilter->GetOutput(); 

    ImageRegionIterator< RealImageType > it( 
      derivativeImage, 
      derivativeImage->GetRequestedRegion() );

    ImageRegionIterator< CumulativeImageType > ot( 
      cumulativeImage, 
      cumulativeImage->GetRequestedRegion() );
  
    const RealType spacing = inputImage->GetSpacing()[ dim ];

    it.GoToBegin();
    ot.GoToBegin();
    while( !it.IsAtEnd() )
      {
      ot.Value() += vnl_math_sqr( it.Get() / spacing );
      ++it;
      ++ot;
      }

    }

  // Release the data on the last smoother since we have acculumated
  // its data into our cumulative image.
  m_SmoothingFilters[ImageDimension-2]->GetOutput()->ReleaseData();

  // Now allocate the output image (postponed the allocation until
  // after all the subfilters ran to minimize total memory footprint)
  outputImage = this->GetOutput();
  outputImage->SetRegions( inputImage->GetBufferedRegion() );
  this->AllocateOutputs();

  // Finally convert the cumulated image to the output by 
  // taking the square root of the pixels.
  ImageRegionIterator< OutputImageType > ot( 
    outputImage, 
    outputImage->GetRequestedRegion() );

  ImageRegionIterator< CumulativeImageType > it( 
    cumulativeImage, 
    cumulativeImage->GetRequestedRegion() );

  it.GoToBegin();
  ot.GoToBegin();
  while( !it.IsAtEnd() )
    {
    ot.Set( static_cast<OutputPixelType>( vcl_sqrt( it.Get() ) ) );
    ++it;
    ++ot;
    }
}



} // end namespace itk

#endif
