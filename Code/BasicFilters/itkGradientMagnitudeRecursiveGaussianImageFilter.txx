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
#ifndef __itkGradientMagnitudeRecursiveGaussianImageFilter_txx
#define __itkGradientMagnitudeRecursiveGaussianImageFilter_txx

#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressAccumulator.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
GradientMagnitudeRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GradientMagnitudeRecursiveGaussianImageFilter()
{
  m_NormalizeAcrossScale = false;

  m_DerivativeFilter = DerivativeFilterType::New();
  m_DerivativeFilter->SetOrder(DerivativeFilterType::FirstOrder);
  m_DerivativeFilter->SetNormalizeAcrossScale(m_NormalizeAcrossScale);

  m_DerivativeFilter->ReleaseDataFlagOn();

  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i] = GaussianFilterType::New();
    m_SmoothingFilters[i]->SetOrder(GaussianFilterType::ZeroOrder);
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    m_SmoothingFilters[i]->ReleaseDataFlagOn();
    }

  m_SmoothingFilters[0]->SetInput( m_DerivativeFilter->GetOutput() );
  for ( unsigned int i = 1; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i]->SetInput( m_SmoothingFilters[i - 1]->GetOutput() );
    }

  m_SqrSpacingFilter = SqrSpacingFilterType::New();
  m_SqrSpacingFilter->SetInput( 1, m_SmoothingFilters[ImageDimension - 2]->GetOutput() );
  // run that filter in place for much efficiency
  m_SqrSpacingFilter->SetInPlace(true);

  // input of SqrtFilter is the cumulative image - we can't set it now
  m_SqrtFilter = SqrtFilterType::New();
  m_SqrtFilter->SetInPlace(false);

  this->SetSigma(1.0);

  this->InPlaceOff();
}

template< typename TInputImage, typename TOutputImage >
void
GradientMagnitudeRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
  os << "Sigma: " << m_DerivativeFilter->GetSigma() << std::endl;
}

/**
 * Set value of Sigma
 */
template< typename TInputImage, typename TOutputImage >
void
GradientMagnitudeRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetSigma(RealType sigma)
{
  if ( sigma != this->GetSigma() )
    {
    for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
      {
      m_SmoothingFilters[i]->SetSigma(sigma);
      }
    m_DerivativeFilter->SetSigma(sigma);

    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage >
typename GradientMagnitudeRecursiveGaussianImageFilter< TInputImage, TOutputImage >::RealType
GradientMagnitudeRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GetSigma()
{
  // just return the sigma value of one filter
  return m_DerivativeFilter->GetSigma();
}

template< typename TInputImage, typename TOutputImage >
void
GradientMagnitudeRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetNumberOfThreads(int nb)
{
  Superclass::SetNumberOfThreads(nb);
  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i]->SetNumberOfThreads(nb);
    }
  m_DerivativeFilter->SetNumberOfThreads(nb);
  m_SqrSpacingFilter->SetNumberOfThreads(nb);
  m_SqrtFilter->SetNumberOfThreads(nb);
}

/**
 * Set Normalize Across Scale Space
 */
template< typename TInputImage, typename TOutputImage >
void
GradientMagnitudeRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetNormalizeAcrossScale(bool normalize)
{
  if ( m_NormalizeAcrossScale != normalize )
    {
    m_NormalizeAcrossScale = normalize;

    for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
      {
      m_SmoothingFilters[i]->SetNormalizeAcrossScale(normalize);
      }
    m_DerivativeFilter->SetNormalizeAcrossScale(normalize);

    this->Modified();
    }
}

//
//
//
template< typename TInputImage, typename TOutputImage >
void
GradientMagnitudeRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
throw( InvalidRequestedRegionError )
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  typename GradientMagnitudeRecursiveGaussianImageFilter< TInputImage,
                                                          TOutputImage >::InputImagePointer image =
    const_cast< InputImageType * >( this->GetInput() );
  if ( image )
    {
    image->SetRequestedRegion( this->GetInput()->GetLargestPossibleRegion() );
    }
}

//
//
//
template< typename TInputImage, typename TOutputImage >
void
GradientMagnitudeRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  TOutputImage *out = dynamic_cast< TOutputImage * >( output );

  if ( out )
    {
    out->SetRequestedRegion( out->GetLargestPossibleRegion() );
    }
}

/**
 * Compute filter for Gaussian kernel
 */
template< typename TInputImage, typename TOutputImage >
void
GradientMagnitudeRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateData(void)
{
  itkDebugMacro(<< "GradientMagnitudeRecursiveGaussianImageFilter generating data ");

  const typename TInputImage::ConstPointer inputImage( this->GetInput() );

  typename TOutputImage::Pointer outputImage( this->GetOutput() );

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  typename CumulativeImageType::Pointer cumulativeImage = CumulativeImageType::New();
  cumulativeImage->SetRegions( inputImage->GetBufferedRegion() );
  cumulativeImage->Allocate();
  cumulativeImage->FillBuffer(NumericTraits< InternalRealType >::Zero);
  // The output's information must match the input's information
  cumulativeImage->CopyInformation( this->GetInput() );

  m_DerivativeFilter->SetInput(inputImage);

  const unsigned int numberOfFilterRuns = ImageDimension * ImageDimension;
  progress->RegisterInternalFilter(m_DerivativeFilter, 1.0f / numberOfFilterRuns);

  for ( unsigned int k = 0; k < ImageDimension - 1; k++ )
    {
    progress->RegisterInternalFilter(m_SmoothingFilters[k], 1.0f / numberOfFilterRuns);
    }

  for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    unsigned int i = 0;
    unsigned int j = 0;
    while (  i < ImageDimension - 1 )
      {
      if ( i == dim )
        {
        j++;
        }
      m_SmoothingFilters[i]->SetDirection(j);
      i++;
      j++;
      }
    m_DerivativeFilter->SetDirection(dim);

    m_SqrSpacingFilter->GetFunctor(). m_Spacing = inputImage->GetSpacing()[dim];
    m_SqrSpacingFilter->SetInput(cumulativeImage);

    // run the mini pipeline for that dimension
    // Note: we must take care to update the requested region. Without that, a
    // second run of the
    // filter with a smaller input than in the first run trigger an exception,
    // because the filter
    // ask for a larger region than available. TODO: there should be a way to do
    // that only once
    // per GenerateData() call.
    m_SqrSpacingFilter->UpdateLargestPossibleRegion();

    // and user the result as the cumulative image
    cumulativeImage = m_SqrSpacingFilter->GetOutput();
    cumulativeImage->DisconnectPipeline();

    progress->ResetFilterProgressAndKeepAccumulatedProgress();
    }
  m_SqrtFilter->SetInput(cumulativeImage);
  m_SqrtFilter->GraftOutput( this->GetOutput() );
  m_SqrtFilter->Update();
  this->GraftOutput( m_SqrtFilter->GetOutput() );
}
} // end namespace itk

#endif
