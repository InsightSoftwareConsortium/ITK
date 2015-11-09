/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkGradientMagnitudeRecursiveGaussianImageFilter_hxx
#define itkGradientMagnitudeRecursiveGaussianImageFilter_hxx

#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressAccumulator.h"
#include "itkMath.h"

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
  m_DerivativeFilter->InPlaceOff();
  m_DerivativeFilter->ReleaseDataFlagOn();

  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i] = GaussianFilterType::New();
    m_SmoothingFilters[i]->SetOrder(GaussianFilterType::ZeroOrder);
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    m_SmoothingFilters[i]->InPlaceOn();
    }

  m_SmoothingFilters[0]->SetInput( m_DerivativeFilter->GetOutput() );
  for ( unsigned int i = 1; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i]->SetInput( m_SmoothingFilters[i - 1]->GetOutput() );
    }

  m_SqrSpacingFilter = SqrSpacingFilterType::New();
  m_SqrSpacingFilter->SetInput( 1, m_SmoothingFilters[ImageDimension - 2]->GetOutput() );
  // run that filter in place for much efficiency
  m_SqrSpacingFilter->InPlaceOn();

  // input of SqrtFilter is the cumulative image - we can't set it now
  m_SqrtFilter = SqrtFilterType::New();
  m_SqrtFilter->InPlaceOn();

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
  if ( Math::NotExactlyEquals(sigma, this->GetSigma()) )
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
::SetNumberOfThreads(ThreadIdType nb)
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

  // If the last filter is running in-place then this bulk data is not
  // needed, release it to save memory
  if ( m_SqrtFilter->CanRunInPlace() )
    {
    outputImage->ReleaseData();
    }


  typename CumulativeImageType::Pointer cumulativeImage = CumulativeImageType::New();
  cumulativeImage->SetRegions( inputImage->GetBufferedRegion() );
  cumulativeImage->Allocate();
  cumulativeImage->FillBuffer(NumericTraits< InternalRealType >::ZeroValue());
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
    }
  m_SqrtFilter->SetInput(cumulativeImage);
  m_SqrtFilter->GraftOutput( this->GetOutput() );
  m_SqrtFilter->UpdateLargestPossibleRegion();
  this->GraftOutput( m_SqrtFilter->GetOutput() );
}
} // end namespace itk

#endif
