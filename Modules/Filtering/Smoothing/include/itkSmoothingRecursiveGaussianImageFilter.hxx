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
#ifndef itkSmoothingRecursiveGaussianImageFilter_hxx
#define itkSmoothingRecursiveGaussianImageFilter_hxx

#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressAccumulator.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SmoothingRecursiveGaussianImageFilter() :
  m_NormalizeAcrossScale( false )
{
  // NB: The first filter is the last dimension because it does not
  // always run in-place. As this dimension provides the least amount
  // of cache coherency, it will provide the least  amount of performance
  // gains from running in-place. In fact, some performance tests
  // indicate that the running in-place in the 3rd dimesion the
  // performance actually declines compared to not in-place methods.
  m_FirstSmoothingFilter = FirstGaussianFilterType::New();
  m_FirstSmoothingFilter->SetOrder(FirstGaussianFilterType::ZeroOrder);
  m_FirstSmoothingFilter->SetDirection(ImageDimension - 1);
  m_FirstSmoothingFilter->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
  m_FirstSmoothingFilter->ReleaseDataFlagOn();
  // InPlace will be set conditionally in the GenerateData method.

  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i] = InternalGaussianFilterType::New();
    m_SmoothingFilters[i]->SetOrder(InternalGaussianFilterType::ZeroOrder);
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    m_SmoothingFilters[i]->SetDirection(i);
    m_SmoothingFilters[i]->ReleaseDataFlagOn();
    m_SmoothingFilters[i]->InPlaceOn();
    }

  m_SmoothingFilters[0]->SetInput( m_FirstSmoothingFilter->GetOutput() );
  for ( unsigned int i = 1; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i]->SetInput(
      m_SmoothingFilters[i - 1]->GetOutput() );
    }

  m_CastingFilter = CastingFilterType::New();
  m_CastingFilter->SetInput( m_SmoothingFilters[ImageDimension - 2]->GetOutput() );
  m_CastingFilter->InPlaceOn();

  this->InPlaceOff();

  // NB: We must call SetSigma in order to initialize the smoothing
  // filters with the default scale.  However, m_Sigma must first be
  // initialized (it is used inside SetSigma), and it must be different
  // from 1.0 or the call will be ignored.
  this->m_Sigma.Fill(0.0);
  this->SetSigma(1.0);
}


template< typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetNumberOfThreads(ThreadIdType nb)
{
  Superclass::SetNumberOfThreads(nb);
  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    m_SmoothingFilters[i]->SetNumberOfThreads(nb);
    }
  m_FirstSmoothingFilter->SetNumberOfThreads(nb);
}


template< typename TInputImage, typename TOutputImage >
bool
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::CanRunInPlace( void ) const
{
  // Note: There are two different ways this filter may try to run
  // in-place:
  // 1) Similar to the standard way, when the input and output image
  // are of the same type, they can share the bulk data. The output
  // will be grafted onto the last filter. In this fashion the input
  // and output will be the same bulk data, but the intermediate
  // mini-pipeline will use different data.
  // 2) If the input image is the same type as the RealImage used for
  // the mini-pipeline, then all the filters may re-use the same
  // bulk data, stealing it from the input then moving it down the
  // pipeline filter by filter. Additionally, if the output is also
  // RealType then the last filter will run in-place making the entire
  // pipeline in-place and only utilizing on copy of the bulk data.

  return m_FirstSmoothingFilter->CanRunInPlace() || this->Superclass::CanRunInPlace();
}


template< typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetSigma(ScalarRealType sigma)
{
  SigmaArrayType sigmas(sigma);

  this->SetSigmaArray(sigmas);
}


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
      m_SmoothingFilters[i]->SetSigma(m_Sigma[i]);
      }
    m_FirstSmoothingFilter->SetSigma(m_Sigma[ImageDimension-1]);

    this->Modified();
    }
}


template< typename TInputImage, typename TOutputImage >
typename SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >::SigmaArrayType
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GetSigmaArray() const
{
  return m_Sigma;
}


template< typename TInputImage, typename TOutputImage >
typename SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >::ScalarRealType
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GetSigma() const
{
  return m_Sigma[0];
}


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

template< typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method. This should
  // copy the output requested region to the input requested region.
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


template< typename TInputImage, typename TOutputImage >
void
SmoothingRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateData(void)
{
  itkDebugMacro(<< "SmoothingRecursiveGaussianImageFilter generating data ");

  const typename TInputImage::ConstPointer inputImage( this->GetInput() );

  const typename TInputImage::RegionType region = inputImage->GetRequestedRegion();
  const typename TInputImage::SizeType   size   = region.GetSize();

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

  // If this filter is running in-place, then set the first smoothing
  // filter to steal the bulk data, by running in-place.
  if ( this->CanRunInPlace() && this->GetInPlace() )
    {
    m_FirstSmoothingFilter->InPlaceOn();

    // To make this filter's input and out share the same data, call
    // the InPlace's/Superclass's allocate methods, which takes care
    // of the needed bulk data sharing.
    this->AllocateOutputs();
    }
  else
    {
    m_FirstSmoothingFilter->InPlaceOff();
    }

  // If the last filter is running in-place then this bulk data is not
  // needed, release it to save memory.
  if ( m_CastingFilter->CanRunInPlace() )
    {
    this->GetOutput()->ReleaseData();
    }

  // Create a process accumulator for tracking the progress of this minipipeline.
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Register the filter with the with progress accumulator using
  // equal weight proportion.
  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    progress->RegisterInternalFilter( m_SmoothingFilters[i], 1.0 / ( ImageDimension ) );
    }

  progress->RegisterInternalFilter( m_FirstSmoothingFilter, 1.0 / ( ImageDimension ) );
  m_FirstSmoothingFilter->SetInput(inputImage);

  // Graft our output to the internal filter to force the proper regions
  // to be generated, and the bulk data which be be from the input due
  // to the in-place option.
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

  for( unsigned int i = 0; i < ImageDimension - 1; ++i )
    {
    itkPrintSelfObjectMacro( SmoothingFilters[i] );
    }
  itkPrintSelfObjectMacro( FirstSmoothingFilter );
  itkPrintSelfObjectMacro( CastingFilter );

  os << indent << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
  os << indent << "Sigma: " << m_Sigma << std::endl;
}

} // end namespace itk

#endif
