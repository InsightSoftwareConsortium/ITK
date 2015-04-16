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
#ifndef itkFFTWInverseFFTImageFilter_hxx
#define itkFFTWInverseFFTImageFilter_hxx

#include "itkFullToHalfHermitianImageFilter.h"
#include "itkFFTWInverseFFTImageFilter.h"
#include "itkInverseFFTImageFilter.hxx"

#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
FFTWInverseFFTImageFilter< TInputImage, TOutputImage >
::FFTWInverseFFTImageFilter()
{
  m_PlanRigor = FFTWGlobalConfiguration::GetPlanRigor();
}

template< typename TInputImage, typename TOutputImage >
void
FFTWInverseFFTImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  // Get pointers to the input and output.
  typename InputImageType::ConstPointer inputPtr  = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // We don't have a nice progress to report, but at least this simple line
  // reports the beginning and the end of the process.
  ProgressReporter progress( this, 0, 1 );

  // Allocate output buffer memory.
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  const InputSizeType inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const OutputSizeType outputSize = outputPtr->GetLargestPossibleRegion().GetSize();

  // Figure out sizes.
  // Size of input and output aren't the same which is handled in the superclass,
  // sort of.
  // The input size and output size only differ in the fastest moving dimension.
  unsigned int totalOutputSize = 1;
  unsigned int totalInputSize = 1;

  for ( unsigned i = 0; i < ImageDimension; i++ )
    {
    totalOutputSize *= outputSize[i];
    totalInputSize *= inputSize[i];
    }

  // Cut the full complex image to just the portion needed by FFTW.
  typedef FullToHalfHermitianImageFilter< InputImageType > FullToHalfFilterType;
  typename FullToHalfFilterType::Pointer fullToHalfFilter = FullToHalfFilterType::New();
  fullToHalfFilter->SetInput( this->GetInput() );
  fullToHalfFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  fullToHalfFilter->UpdateLargestPossibleRegion();

  typename FFTWProxyType::ComplexType * in =
    (typename FFTWProxyType::ComplexType *) fullToHalfFilter->GetOutput()->GetBufferPointer();

  OutputPixelType * out = outputPtr->GetBufferPointer();
  typename FFTWProxyType::PlanType plan;

  int sizes[ImageDimension];
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    sizes[(ImageDimension - 1) - i] = outputSize[i];
    }

  plan = FFTWProxyType::Plan_dft_c2r( ImageDimension, sizes, in, out, m_PlanRigor,
                                      this->GetNumberOfThreads(), false );
  FFTWProxyType::Execute( plan );

  // Some cleanup.
  FFTWProxyType::DestroyPlan( plan );
}

template <typename TInputImage, typename TOutputImage>
void
FFTWInverseFFTImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, ThreadIdType itkNotUsed(threadId) )
{
  typedef ImageRegionIterator< OutputImageType > IteratorType;
  unsigned long totalOutputSize = this->GetOutput()->GetRequestedRegion().GetNumberOfPixels();
  IteratorType it( this->GetOutput(), outputRegionForThread );
  while( !it.IsAtEnd() )
    {
    it.Set( it.Value() / totalOutputSize );
    ++it;
    }
}

template< typename TInputImage, typename TOutputImage >
void
FFTWInverseFFTImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "PlanRigor: " << FFTWGlobalConfiguration::GetPlanRigorName( m_PlanRigor )
     << " (" << m_PlanRigor << ")" << std::endl;
}

template< typename TInputImage, typename TOutputImage >
SizeValueType
FFTWInverseFFTImageFilter< TInputImage, TOutputImage >
::GetSizeGreatestPrimeFactor() const
{
  return FFTWProxyType::GREATEST_PRIME_FACTOR;
}

} // namespace itk
#endif // _itkFFTWInverseFFTImageFilter_hxx
