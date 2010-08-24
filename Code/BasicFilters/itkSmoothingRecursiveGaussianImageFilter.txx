/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmoothingRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSmoothingRecursiveGaussianImageFilter_txx
#define __itkSmoothingRecursiveGaussianImageFilter_txx

#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressAccumulator.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SmoothingRecursiveGaussianImageFilter()
{
  m_NormalizeAcrossScale = false;

  m_FirstSmoothingFilter = FirstGaussianFilterType::New();
  m_FirstSmoothingFilter->SetOrder(FirstGaussianFilterType::ZeroOrder);
  m_FirstSmoothingFilter->SetDirection(0);
  m_FirstSmoothingFilter->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
  m_FirstSmoothingFilter->ReleaseDataFlagOn();

  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i] = InternalGaussianFilterType::New();
    m_SmoothingFilters[i]->SetOrder(InternalGaussianFilterType::ZeroOrder);
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    m_SmoothingFilters[i]->SetDirection(i + 1);
    m_SmoothingFilters[i]->ReleaseDataFlagOn();
    }

  m_SmoothingFilters[0]->SetInput( m_FirstSmoothingFilter->GetOutput() );
  for ( unsigned int i = 1; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i]->SetInput(
      m_SmoothingFilters[i - 1]->GetOutput() );
    }

  m_CastingFilter = CastingFilterType::New();
  m_CastingFilter->SetInput( m_SmoothingFilters[ImageDimension - 2]->GetOutput() );

  //
  // NB: We must call SetSigma in order to initialize the smoothing
  // filters with the default scale.  However, m_Sigma must first be
  // initialized (it is used inside SetSigma) and it must be different
  // from 1.0 or the call will be ignored.
  this->m_Sigma.Fill(0.0);
  this->SetSigma(1.0);
}

template< typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetNumberOfThreads(int nb)
{
  Superclass::SetNumberOfThreads(nb);
  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i]->SetNumberOfThreads(nb);
    }
  m_FirstSmoothingFilter->SetNumberOfThreads(nb);
}

// Set value of Sigma (isotropic)

template< typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetSigma(ScalarRealType sigma)
{
  SigmaArrayType sigmas(sigma);

  this->SetSigmaArray(sigmas);
}

// Set value of Sigma (an-isotropic)

template< typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetSigmaArray(const SigmaArrayType & sigma)
{
  if ( this->m_Sigma != sigma )
    {
    this->m_Sigma = sigma;
    for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
      {
      m_SmoothingFilters[i]->SetSigma(m_Sigma[i + 1]);
      }
    m_FirstSmoothingFilter->SetSigma(m_Sigma[0]);

    this->Modified();
    }
}

// Get the sigma array.
template< typename TInputImage, typename TOutputImage >
typename SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >::SigmaArrayType
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GetSigmaArray() const
{
  return m_Sigma;
}

// Get the sigma scalar. If the sigma is anisotropic, we will just
// return the sigma along the first dimension.
template< typename TInputImage, typename TOutputImage >
typename SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >::ScalarRealType
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GetSigma() const
{
  return m_Sigma[0];
}

/**
 * Set Normalize Across Scale Space
 */
template< typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetNormalizeAcrossScale(bool normalize)
{
  m_NormalizeAcrossScale = normalize;

  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(normalize);
    }
  m_FirstSmoothingFilter->SetNormalizeAcrossScale(normalize);

  this->Modified();
}

//
//
//
template< typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
throw( InvalidRequestedRegionError )
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  typename SmoothingRecursiveGaussianImageFilter< TInputImage,
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
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
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
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateData(void)
{
  itkDebugMacro(<< "SmoothingRecursiveGaussianImageFilter generating data ");

  const typename TInputImage::ConstPointer inputImage( this->GetInput() );

  const typename TInputImage::RegionType region = inputImage->GetRequestedRegion();
  const typename TInputImage::SizeType size   = region.GetSize();

  for ( unsigned int d = 0; d < ImageDimension; d++ )
    {
    if ( size[d] < 4 )
      {
      itkExceptionMacro(
        "The number of pixels along dimension " << d
                                                <<
        " is less than 4. This filter requires a minimum of four pixels along the dimension to be processed.");
      }
    }

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Register the filter with the with progress accumulator using
  // equal weight proportion
  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    progress->RegisterInternalFilter( m_SmoothingFilters[i], 1.0 / ( ImageDimension ) );
    }

  progress->RegisterInternalFilter( m_FirstSmoothingFilter, 1.0 / ( ImageDimension ) );
  m_FirstSmoothingFilter->SetInput(inputImage);
  // graft our output to the internal filter to force the proper regions
  // to be generated
  m_CastingFilter->GraftOutput( this->GetOutput() );
  m_CastingFilter->Update();
  this->GraftOutput( m_CastingFilter->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
  os << "Sigma: " << m_Sigma << std::endl;
}
} // end namespace itk

#endif
