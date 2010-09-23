/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLaplacianRecursiveGaussianImageFilter_txx
#define __itkLaplacianRecursiveGaussianImageFilter_txx

#include "itkLaplacianRecursiveGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressAccumulator.h"
#include "itkCastImageFilter.h"
#include "itkAddImageFilter.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
LaplacianRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::LaplacianRecursiveGaussianImageFilter()
{
  m_NormalizeAcrossScale = false;

  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i] = GaussianFilterType::New();
    m_SmoothingFilters[i]->SetOrder(GaussianFilterType::ZeroOrder);
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    m_SmoothingFilters[i]->ReleaseDataFlagOn();
    }

  m_DerivativeFilter = DerivativeFilterType::New();
  m_DerivativeFilter->SetOrder(DerivativeFilterType::SecondOrder);
  m_DerivativeFilter->SetNormalizeAcrossScale(m_NormalizeAcrossScale);

  m_DerivativeFilter->SetInput( this->GetInput() );

  m_SmoothingFilters[0]->SetInput( m_DerivativeFilter->GetOutput() );

  for ( unsigned int i = 1; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i]->SetInput(
      m_SmoothingFilters[i - 1]->GetOutput() );
    }

  m_CumulativeImage = CumulativeImageType::New();

  this->SetSigma(1.0);
}

/**
 * Set value of Sigma
 */
template< typename TInputImage, typename TOutputImage >
void
LaplacianRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetSigma(RealType sigma)
{
  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
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
LaplacianRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetNormalizeAcrossScale(bool normalize)
{
  m_NormalizeAcrossScale = normalize;

  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
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
LaplacianRecursiveGaussianImageFilter< TInputImage, TOutputImage >
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
LaplacianRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateData(void)
{
  itkDebugMacro(<< "LaplacianRecursiveGaussianImageFilter generating data ");

  // Create a process accumulator for tracking the progress of minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // dim^2 recursive gaussians + dim add filters + cast filter
  const unsigned int numberOfFilters = vnl_math_sqr( ImageDimension ) +  ImageDimension + 1;

  // register (most) filters with the progress accumulator
  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    progress->RegisterInternalFilter(m_SmoothingFilters[i],  1.0 / numberOfFilters );
    }
  progress->RegisterInternalFilter(m_DerivativeFilter,   1.0 / numberOfFilters );

  const typename TInputImage::ConstPointer inputImage( this->GetInput() );

  typename TOutputImage::Pointer outputImage( this->GetOutput() );

  outputImage = this->GetOutput();

  outputImage->SetRegions( inputImage->GetBufferedRegion() );

  outputImage->Allocate();

  m_CumulativeImage->SetRegions( inputImage->GetBufferedRegion() );
  m_CumulativeImage->Allocate();
  m_CumulativeImage->FillBuffer(NumericTraits< InternalRealType >::Zero);

  m_DerivativeFilter->SetInput(inputImage);

  // allocate the add and scale image filter just for the scope of
  // this function!
  typedef itk::BinaryFunctorImageFilter< CumulativeImageType, RealImageType, CumulativeImageType, AddMultConstFunctor > AddFilterType;
  typename AddFilterType::Pointer addFilter = AddFilterType::New();

  // register with progress accumulator
  progress->RegisterInternalFilter( addFilter,   1.0 / numberOfFilters );


  for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    unsigned int i = 0;
    unsigned int j = 0;
    while (  i < ImageDimension )
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

    GaussianFilterPointer lastFilter = m_SmoothingFilters[ImageDimension - 2];

    // scale the new value by the inverse of the spacing squared
    const RealType spacing2 = vnl_math_sqr( inputImage->GetSpacing()[dim] );
    addFilter->GetFunctor().m_Value = 1.0/spacing2;

    // Cummulate the results on the output image
    addFilter->SetInput1( m_CumulativeImage );
    addFilter->SetInput2( lastFilter->GetOutput() );
    addFilter->InPlaceOn();
    addFilter->Update();

    m_CumulativeImage = addFilter->GetOutput();
    m_CumulativeImage->DisconnectPipeline();

    // after each pass reset progress to accumulate next iteration
    progress->ResetFilterProgressAndKeepAccumulatedProgress();
    }

  // Finally convert the cumulated image to the output

  // The CastImageFilter is used here because it is multithreaded and
  // it may perform no operation if the two images types are the same
  typedef itk::CastImageFilter< CumulativeImageType, OutputImageType > CastFilterType;
  typename CastFilterType::Pointer caster = CastFilterType::New();
  caster->SetInput( m_CumulativeImage );

  // register with progress accumulator
  progress->RegisterInternalFilter( caster,   1.0 / numberOfFilters );

  // graft the our output to the casted output to share the
  // output bulk-data, meta-information and regions, then update the
  // requested image
  caster->GraftOutput( outputImage );
  caster->Update();
  this->GraftOutput( caster->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
LaplacianRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
}
} // end namespace itk

#endif
