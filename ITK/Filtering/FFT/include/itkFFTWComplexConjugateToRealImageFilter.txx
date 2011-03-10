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
#ifndef __itkFFTWComplexConjugateToRealImageFilter_txx
#define __itkFFTWComplexConjugateToRealImageFilter_txx

#include "itkFFTWComplexConjugateToRealImageFilter.h"
#include "itkFFTComplexConjugateToRealImageFilter.txx"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

template< typename TPixel, unsigned int VDimension >
void
FFTWComplexConjugateToRealImageFilter< TPixel, VDimension >::
BeforeThreadedGenerateData()
{
  // get pointers to the input and output
  typename TInputImageType::ConstPointer inputPtr  = this->GetInput();
  typename TOutputImageType::Pointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // we don't have a nice progress to report, but at least this simple line
  // reports the begining and the end of the process
  ProgressReporter progress(this, 0, 1);

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  const typename TInputImageType::SizeType &   outputSize =
    outputPtr->GetLargestPossibleRegion().GetSize();
  const typename TOutputImageType::SizeType & inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();

  // figure out sizes
  // size of input and output aren't the same which is handled in the superclass,
  // sort of.
  // the input size and output size only differ in the fastest moving dimension
  unsigned int total_outputSize = 1;
  unsigned int total_inputSize = 1;

  for ( unsigned i = 0; i < VDimension; i++ )
    {
    total_outputSize *= outputSize[i];
    total_inputSize *= inputSize[i];
    }

  typename FFTWProxyType::ComplexType * in;
  // complex to real transform don't have any algorithm which support the FFTW_PRESERVE_INPUT at this time.
  // So if the input can't be destroyed, we have to copy the input data to a buffer before running
  // the ifft.
  if( m_CanUseDestructiveAlgorithm )
    {
    // ok, so lets use the input buffer directly, to save some memory
    in = (typename FFTWProxyType::ComplexType*)inputPtr->GetBufferPointer();
    }
  else
    {
    // we must use a buffer where fftw can work and destroy what it wants
    in = new typename FFTWProxyType::ComplexType[total_inputSize];
    }
  TPixel * out = outputPtr->GetBufferPointer();
  typename FFTWProxyType::PlanType plan;

  int *sizes = new int[VDimension];
  for(unsigned int i = 0; i < VDimension; i++)
    {
    sizes[(VDimension - 1) - i] = outputSize[i];
    }
  plan = FFTWProxyType::Plan_dft_c2r(VDimension,sizes,
                              in,
                              out,
                              m_PlanRigor,
                              this->GetNumberOfThreads(),
                              !m_CanUseDestructiveAlgorithm);
  delete [] sizes;
  if( !m_CanUseDestructiveAlgorithm )
    {
    memcpy(in,
           inputPtr->GetBufferPointer(),
           total_inputSize * sizeof(typename FFTWProxyType::ComplexType));
    }
  FFTWProxyType::Execute(plan);

  // some cleanup
  FFTWProxyType::DestroyPlan(plan);
  if( !m_CanUseDestructiveAlgorithm )
    {
    delete [] in;
    }
}

template <typename TPixel, unsigned int VDimension>
void
FFTWComplexConjugateToRealImageFilter< TPixel, VDimension >::
ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, int itkNotUsed(threadId) )
{
  typedef ImageRegionIterator< TOutputImageType >   IteratorType;
  unsigned long total_outputSize = this->GetOutput()->GetRequestedRegion().GetNumberOfPixels();
  IteratorType it(this->GetOutput(), outputRegionForThread);
  while( !it.IsAtEnd() )
    {
    it.Set( it.Value() / total_outputSize );
    ++it;
    }
}

template< typename TPixel, unsigned int VDimension >
bool
FFTWComplexConjugateToRealImageFilter< TPixel, VDimension >::FullMatrix()
{
  return false;
}


template< typename TPixel, unsigned int VDimension >
void
FFTWComplexConjugateToRealImageFilter< TPixel, VDimension >::
UpdateOutputData(DataObject * output)
{
  // we need to catch that information now, because it is changed later
  // during the pipeline execution, and thus can't be grabbed in
  // GenerateData().
  m_CanUseDestructiveAlgorithm = this->GetInput()->GetReleaseDataFlag();
  Superclass::UpdateOutputData( output );
}

template< typename TPixel, unsigned int VDimension >
void
FFTWComplexConjugateToRealImageFilter< TPixel, VDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "PlanRigor: " << FFTWGlobalConfiguration::GetPlanRigorName(m_PlanRigor) << " (" << m_PlanRigor << ")" << std::endl;
}

} // namespace itk
#endif // _itkFFTWComplexConjugateToRealImageFilter_txx
