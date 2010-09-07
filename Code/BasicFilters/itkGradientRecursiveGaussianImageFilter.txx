/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGradientRecursiveGaussianImageFilter_txx
#define __itkGradientRecursiveGaussianImageFilter_txx

#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GradientRecursiveGaussianImageFilter()
{
  m_NormalizeAcrossScale = false;
  this->m_UseImageDirection = true;

  unsigned int imageDimensionMinus1 = ImageDimension - 1;
  if ( ImageDimension > 1 )
    {
    m_SmoothingFilters.resize(imageDimensionMinus1);
    }

  for ( unsigned int i = 0; i < imageDimensionMinus1; i++ )
    {
    m_SmoothingFilters[i] = GaussianFilterType::New();
    m_SmoothingFilters[i]->SetOrder(GaussianFilterType::ZeroOrder);
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    m_SmoothingFilters[i]->ReleaseDataFlagOn();
    }

  m_DerivativeFilter = DerivativeFilterType::New();
  m_DerivativeFilter->SetOrder(DerivativeFilterType::FirstOrder);
  m_DerivativeFilter->SetNormalizeAcrossScale(m_NormalizeAcrossScale);

  m_DerivativeFilter->SetInput( this->GetInput() );

  if ( ImageDimension > 1 )
    {
    m_SmoothingFilters[0]->SetInput( m_DerivativeFilter->GetOutput() );
    }

  for ( unsigned int i = 1; i < imageDimensionMinus1; i++ )
    {
    m_SmoothingFilters[i]->SetInput(
      m_SmoothingFilters[i - 1]->GetOutput() );
    }

  m_ImageAdaptor = OutputImageAdaptorType::New();

  this->SetSigma(1.0);
}

/**
 * Set value of Sigma
 */
template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetSigma(RealType sigma)
{
  int imageDimensionMinus1 = static_cast< int >( ImageDimension ) - 1;

  for ( int i = 0; i < imageDimensionMinus1; i++ )
    {
    m_SmoothingFilters[i]->SetSigma(sigma);
    }
  m_DerivativeFilter->SetSigma(sigma);

  this->Modified();
}

/**
 * Set Normalize Across Scale Space
 */
template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetNormalizeAcrossScale(bool normalize)
{
  m_NormalizeAcrossScale = normalize;

  int imageDimensionMinus1 = static_cast< int >( ImageDimension ) - 1;
  for ( int i = 0; i < imageDimensionMinus1; i++ )
    {
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(normalize);
    }
  m_DerivativeFilter->SetNormalizeAcrossScale(normalize);

  this->Modified();
}

//
//
//
template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
throw( InvalidRequestedRegionError )
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  typename GradientRecursiveGaussianImageFilter< TInputImage,
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
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
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
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateData(void)
{
  // Create a process accumulator for tracking the progress of this
  // minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Compute the contribution of each filter to the total progress.
  const double weight = 1.0 / ( ImageDimension * ImageDimension );

  unsigned int imageDimensionMinus1 = ImageDimension - 1;
  for ( unsigned int i = 0; i < imageDimensionMinus1; i++ )
    {
    progress->RegisterInternalFilter(m_SmoothingFilters[i], weight);
    }

  progress->RegisterInternalFilter(m_DerivativeFilter, weight);
  progress->ResetProgress();

  const typename TInputImage::ConstPointer inputImage( this->GetInput() );

  m_ImageAdaptor->SetImage( this->GetOutput() );

  m_ImageAdaptor->SetLargestPossibleRegion(
    inputImage->GetLargestPossibleRegion() );

  m_ImageAdaptor->SetBufferedRegion(
    inputImage->GetBufferedRegion() );

  m_ImageAdaptor->SetRequestedRegion(
    inputImage->GetRequestedRegion() );

  m_ImageAdaptor->Allocate();

  m_DerivativeFilter->SetInput(inputImage);

  for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    unsigned int i = 0;
    unsigned int j = 0;
    while (  i < imageDimensionMinus1 )
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

    GaussianFilterPointer lastFilter;

    if ( ImageDimension > 1 )
      {
      int imageDimensionMinus2 = static_cast< int >( ImageDimension ) - 2;
      lastFilter = m_SmoothingFilters[imageDimensionMinus2];
      lastFilter->Update();
      }
    else
      {
      m_DerivativeFilter->Update();
      }

    progress->ResetFilterProgressAndKeepAccumulatedProgress();

    // Copy the results to the corresponding component
    // on the output image of vectors
    m_ImageAdaptor->SelectNthElement(dim);

    typename RealImageType::Pointer derivativeImage;
    if ( ImageDimension > 1 )
      {
      derivativeImage = lastFilter->GetOutput();
      }
    else
      {
      derivativeImage = m_DerivativeFilter->GetOutput();
      }

    ImageRegionIteratorWithIndex< RealImageType > it(
      derivativeImage,
      derivativeImage->GetRequestedRegion() );

    ImageRegionIteratorWithIndex< OutputImageAdaptorType > ot(
      m_ImageAdaptor,
      m_ImageAdaptor->GetRequestedRegion() );

    const RealType spacing = inputImage->GetSpacing()[dim];

    it.GoToBegin();
    ot.GoToBegin();
    while ( !it.IsAtEnd() )
      {
      ot.Set(it.Get() / spacing);
      ++it;
      ++ot;
      }
    }

  // If the flag for using the input image direction is ON,
  // then we apply the direction correction to all the pixels
  // of the output gradient image.
  if ( this->m_UseImageDirection )
    {
    OutputImageType *gradientImage = this->GetOutput();
    typedef typename InputImageType::DirectionType DirectionType;
    ImageRegionIterator< OutputImageType > itr( gradientImage,
                                                gradientImage->GetRequestedRegion() );

    OutputPixelType correctedGradient;
    while ( !itr.IsAtEnd() )
      {
      const OutputPixelType & gradient = itr.Get();
      inputImage->TransformLocalVectorToPhysicalVector(gradient, correctedGradient);
      itr.Set(correctedGradient);
      ++itr;
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
  os << indent << "UseImageDirection :   "
     << ( this->m_UseImageDirection ? "On" : "Off" ) << std::endl;
}
} // end namespace itk

#endif
