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
#ifndef itkLaplacianRecursiveGaussianImageFilter_hxx
#define itkLaplacianRecursiveGaussianImageFilter_hxx

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

  for ( unsigned int i = 0; i < NumberOfSmoothingFilters; i++ )
    {
    m_SmoothingFilters[i] = GaussianFilterType::New();
    m_SmoothingFilters[i]->SetOrder(GaussianFilterType::ZeroOrder);
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    m_SmoothingFilters[i]->ReleaseDataFlagOn();
    m_SmoothingFilters[i]->InPlaceOn();
    }

  m_DerivativeFilter = DerivativeFilterType::New();
  m_DerivativeFilter->SetOrder(DerivativeFilterType::SecondOrder);
  m_DerivativeFilter->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
  m_DerivativeFilter->ReleaseDataFlagOn();
  m_DerivativeFilter->InPlaceOff();

  m_DerivativeFilter->SetInput( this->GetInput() );

  m_SmoothingFilters[0]->SetInput( m_DerivativeFilter->GetOutput() );

  if ( NumberOfSmoothingFilters > 1 )
    {
    for ( unsigned int i = 1; i < NumberOfSmoothingFilters; i++ )
      {
      m_SmoothingFilters[i]->SetInput(
        m_SmoothingFilters[i - 1]->GetOutput() );
      }
    }

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
  for ( unsigned int i = 0; i < NumberOfSmoothingFilters; i++ )
    {
    m_SmoothingFilters[i]->SetSigma(sigma);
    }
  m_DerivativeFilter->SetSigma(sigma);

  this->Modified();
}

/**
 * Get value of Sigma
 */
template< typename TInputImage, typename TOutputImage >
typename LaplacianRecursiveGaussianImageFilter< TInputImage, TOutputImage >::RealType
LaplacianRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GetSigma() const
{
  return m_DerivativeFilter->GetSigma();
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

  for ( unsigned int i = 0; i < NumberOfSmoothingFilters; i++ )
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

  // Set the number of threads on all the filters
  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
      m_SmoothingFilters[i]->SetNumberOfThreads(this->GetNumberOfThreads());
    }
  m_DerivativeFilter->SetNumberOfThreads(this->GetNumberOfThreads());

  // Create a process accumulator for tracking the progress of minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // dim^2 recursive gaussians + dim add filters + cast filter
  const unsigned int numberOfFilters = ( ImageDimension * ImageDimension ) +  ImageDimension + 1;

  // register (most) filters with the progress accumulator
  for ( unsigned int i = 0; i < NumberOfSmoothingFilters; i++ )
    {
    progress->RegisterInternalFilter(m_SmoothingFilters[i],  1.0 / numberOfFilters );
    }
  progress->RegisterInternalFilter(m_DerivativeFilter,   1.0 / numberOfFilters );

  const typename TInputImage::ConstPointer inputImage( this->GetInput() );

  // initialize output image
  //
  // NOTE: We intentionally don't allocate the output image here,
  // because the cast image filter will either run inplace, or alloate
  // the output there. The requested region has already been set in
  // ImageToImageFilter::GenerateInputImageFilter.
  typename TOutputImage::Pointer outputImage( this->GetOutput() );
  //outputImage->Allocate(); let the CasterImageFilter allocate the image

  //  Auxiliary image for accumulating the second-order derivatives
  typedef Image< InternalRealType, itkGetStaticConstMacro(ImageDimension) > CumulativeImageType;
  typedef typename CumulativeImageType::Pointer CumulativeImagePointer;

  // The CastImageFilter is used because it is multithreaded and
  // it may perform no operation if the two images types are the same
  typedef itk::CastImageFilter< CumulativeImageType, OutputImageType > CastFilterType;
  typename CastFilterType::Pointer caster = CastFilterType::New();
    caster->SetNumberOfThreads(this->GetNumberOfThreads());

  // If the last filter is running in-place then this bulk data is not
  // needed, release it to save memory
  if ( caster->CanRunInPlace() )
    {
    outputImage->ReleaseData();
    }


  CumulativeImagePointer cumulativeImage = CumulativeImageType::New();
  cumulativeImage->SetRegions( outputImage->GetRequestedRegion() );
  cumulativeImage->CopyInformation( inputImage );
  cumulativeImage->Allocate();
  cumulativeImage->FillBuffer(NumericTraits< InternalRealType >::ZeroValue());

  m_DerivativeFilter->SetInput(inputImage);

  // allocate the add and scale image filter just for the scope of
  // this function!
  typedef itk::BinaryFunctorImageFilter< CumulativeImageType, RealImageType, CumulativeImageType, AddMultConstFunctor > AddFilterType;
  typename AddFilterType::Pointer addFilter = AddFilterType::New();
  addFilter->SetNumberOfThreads(this->GetNumberOfThreads());

  // register with progress accumulator
  progress->RegisterInternalFilter( addFilter,   1.0 / numberOfFilters );


  for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    unsigned int i = 0;
    unsigned int j = 0;
    while (  i < NumberOfSmoothingFilters )
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
    const RealType spacing2 = itk::Math::sqr( inputImage->GetSpacing()[dim] );
    addFilter->GetFunctor().m_Value = 1.0/spacing2;

    // Cummulate the results on the output image
    addFilter->SetInput1( cumulativeImage );
    addFilter->SetInput2( lastFilter->GetOutput() );
    addFilter->InPlaceOn();
    addFilter->Update();

    cumulativeImage = addFilter->GetOutput();
    cumulativeImage->DisconnectPipeline();
    }

  // Because the output of last filter in the mini-pipeline is not
  // pipelined the data must be manually released
  if ( ImageDimension > 1 )
    {
    m_SmoothingFilters[ImageDimension - 2]->GetOutput()->ReleaseData();
    }
  else
    {
    m_DerivativeFilter->GetOutput()->ReleaseData();
    }

  // Finally convert the cumulated image to the output with a caster
  caster->SetInput( cumulativeImage );

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
