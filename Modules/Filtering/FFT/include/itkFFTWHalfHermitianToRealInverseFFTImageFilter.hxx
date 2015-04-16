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
#ifndef itkFFTWHalfHermitianToRealInverseFFTImageFilter_hxx
#define itkFFTWHalfHermitianToRealInverseFFTImageFilter_hxx

#include "itkFFTWHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkHalfHermitianToRealInverseFFTImageFilter.hxx"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
FFTWHalfHermitianToRealInverseFFTImageFilter< TInputImage, TOutputImage >
::FFTWHalfHermitianToRealInverseFFTImageFilter()
{
  m_PlanRigor = FFTWGlobalConfiguration::GetPlanRigor();
}

template< typename TInputImage, typename TOutputImage >
void
FFTWHalfHermitianToRealInverseFFTImageFilter< TInputImage, TOutputImage >
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

  typename FFTWProxyType::ComplexType * in;
  // The complex-to-real transform doesn't support the
  // FFTW_PRESERVE_INPUT flag at this time. So if the input can't be
  // destroyed, we have to copy the input data to a buffer before
  // running the IFFT.
  if( m_CanUseDestructiveAlgorithm )
    {
    // Ok, so lets use the input buffer directly, to save some memory.
    in = (typename FFTWProxyType::ComplexType*)inputPtr->GetBufferPointer();
    }
  else
    {
    // We must use a buffer where fftw can work and destroy what it wants.
    in = new typename FFTWProxyType::ComplexType[totalInputSize];
    }
  OutputPixelType * out = outputPtr->GetBufferPointer();
  typename FFTWProxyType::PlanType plan;

  int sizes[ImageDimension];
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    sizes[(ImageDimension - 1) - i] = outputSize[i];
    }
  plan = FFTWProxyType::Plan_dft_c2r( ImageDimension, sizes, in, out, m_PlanRigor,
                                      this->GetNumberOfThreads(),
                                      !m_CanUseDestructiveAlgorithm );
  if( !m_CanUseDestructiveAlgorithm )
    {
    // complex<double> and double[2] types are compatible memory layouts.
    // The reinterpret_cast is used here to
    // make the "C" fftw libary compatible with the c++ complex<double>.
    std::copy( inputPtr->GetBufferPointer(),
               inputPtr->GetBufferPointer()+totalInputSize,
               reinterpret_cast< typename InputImageType::PixelType * > (in) );
    }
  FFTWProxyType::Execute( plan );

  // Some cleanup.
  FFTWProxyType::DestroyPlan( plan );
  if( !m_CanUseDestructiveAlgorithm )
    {
    delete[] in;
    }
}

template <typename TInputImage, typename TOutputImage>
void
FFTWHalfHermitianToRealInverseFFTImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputRegionType& outputRegionForThread,
                       ThreadIdType itkNotUsed(threadId) )
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
FFTWHalfHermitianToRealInverseFFTImageFilter< TInputImage, TOutputImage >
::UpdateOutputData(DataObject * output)
{
  // We need to catch that information now, because it is changed
  // later during the pipeline execution, and thus can't be grabbed in
  // GenerateData().
  m_CanUseDestructiveAlgorithm = this->GetInput()->GetReleaseDataFlag();
  Superclass::UpdateOutputData( output );
}

template< typename TInputImage, typename TOutputImage >
void
FFTWHalfHermitianToRealInverseFFTImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "PlanRigor: " << FFTWGlobalConfiguration::GetPlanRigorName( m_PlanRigor )
     << " (" << m_PlanRigor << ")" << std::endl;
}

template< typename TInputImage, typename TOutputImage >
SizeValueType
FFTWHalfHermitianToRealInverseFFTImageFilter< TInputImage, TOutputImage >
::GetSizeGreatestPrimeFactor() const
{
  return FFTWProxyType::GREATEST_PRIME_FACTOR;
}

} // namespace itk
#endif // _itkFFTWHalfHermitianToRealInverseFFTImageFilter_hxx
