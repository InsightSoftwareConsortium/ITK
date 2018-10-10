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
#ifndef itkFFTWRealToHalfHermitianForwardFFTImageFilter_hxx
#define itkFFTWRealToHalfHermitianForwardFFTImageFilter_hxx

#include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkProgressReporter.h"
#include "itkMultiThreaderBase.h"

namespace itk
{
/** TODO:  There should be compile time type checks so that
           if only ITK_USE_FFTWF is defined, then only floats are valid.
           and if ITK_USE_FFTWD is defined, then only doubles are valid.
*/

template< typename TInputImage, typename TOutputImage >
FFTWRealToHalfHermitianForwardFFTImageFilter< TInputImage, TOutputImage >
::FFTWRealToHalfHermitianForwardFFTImageFilter()
{
#ifndef ITK_USE_CUFFTW
  m_PlanRigor = FFTWGlobalConfiguration::GetPlanRigor();
#endif
}

template< typename TInputImage, typename TOutputImage >
void
FFTWRealToHalfHermitianForwardFFTImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Get pointers to the input and output.
  typename InputImageType::ConstPointer inputPtr = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // We don't have a nice progress to report, but at least this simple line
  // reports the beginning and the end of the process.
  ProgressReporter progress(this, 0, 1);

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  const typename InputImageType::SizeType &   inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename OutputImageType::SizeType &   outputSize =
    outputPtr->GetLargestPossibleRegion().GetSize();

  // figure out sizes
  // size of input and output aren't the same which is handled in the superclass,
  // sort of.
  // the input size and output size only differ in the fastest moving dimension
  unsigned int totalInputSize = 1;
  unsigned int totalOutputSize = 1;

  for ( unsigned i = 0; i < ImageDimension; i++ )
    {
    totalInputSize *= inputSize[i];
    totalOutputSize *= outputSize[i];
    }

  typename FFTWProxyType::PlanType plan;
  auto * in = const_cast<InputPixelType*>(inputPtr->GetBufferPointer());
  auto * out = (typename FFTWProxyType::ComplexType*) outputPtr->GetBufferPointer();
  int flags = m_PlanRigor;
  if( !m_CanUseDestructiveAlgorithm )
    {
    // if the input is about to be destroyed, there is no need to force fftw
    // to use an non destructive algorithm. If it is not released however,
    // we must be careful to not destroy it.
    flags = flags | FFTW_PRESERVE_INPUT;
    }
  int sizes[ImageDimension];
  for(unsigned int i = 0; i < ImageDimension; i++)
    {
    sizes[(ImageDimension - 1) - i] = inputSize[i];
    }

  plan = FFTWProxyType::Plan_dft_r2c(ImageDimension, sizes, in, out, flags,
                                     MultiThreaderBase::GetGlobalDefaultNumberOfThreads());
  FFTWProxyType::Execute(plan);
  FFTWProxyType::DestroyPlan(plan);
}

template< typename TInputImage, typename TOutputImage >
void
FFTWRealToHalfHermitianForwardFFTImageFilter< TInputImage, TOutputImage >
::UpdateOutputData(DataObject * output)
{
  // We need to catch that information now, because it is changed later
  // during the pipeline execution, and thus can't be grabbed in
  // GenerateData().
  m_CanUseDestructiveAlgorithm = this->GetInput()->GetReleaseDataFlag();
  Superclass::UpdateOutputData( output );
}

template< typename TInputImage, typename TOutputImage >
void
FFTWRealToHalfHermitianForwardFFTImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

#ifndef ITK_USE_CUFFTW
  os << indent << "PlanRigor: " << FFTWGlobalConfiguration::GetPlanRigorName(m_PlanRigor) << " (" << m_PlanRigor << ")" << std::endl;
#endif
}

template< typename TInputImage, typename TOutputImage >
SizeValueType
FFTWRealToHalfHermitianForwardFFTImageFilter< TInputImage, TOutputImage >
::GetSizeGreatestPrimeFactor() const
{
  return FFTWProxyType::GREATEST_PRIME_FACTOR;
}

} // namespace itk

#endif //_itkFFTWRealToHalfHermitianForwardFFTImageFilter_hxx
