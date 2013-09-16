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
#ifndef __itkFFTWComplexToComplexImageFilter_hxx
#define __itkFFTWComplexToComplexImageFilter_hxx

#include "itkFFTWComplexToComplexImageFilter.h"
#include "itkFFTComplexToComplexImageFilter.h"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"


/*
 *
 * This code was contributed in the Insight Journal paper:
 * "FFT Complex to Complex filters and helper classes"
 * by Warfield S.
 * http://hdl.handle.net/1926/326
 * http://www.insight-journal.org/browse/publication/128
 *
 */

namespace itk
{

template< typename TImage >
void
FFTWComplexToComplexImageFilter< TImage >::
BeforeThreadedGenerateData()
{
  // get pointers to the input and output
  typename InputImageType::ConstPointer inputPtr  = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // we don't have a nice progress to report, but at least this simple line
  // reports the beginning and the end of the process
  ProgressReporter progress(this, 0, 1);

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  const typename InputImageType::SizeType &   outputSize =
    outputPtr->GetLargestPossibleRegion().GetSize();
  const typename OutputImageType::SizeType & inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();

  // figure out sizes
  // size of input and output aren't the same which is handled in the superclass,
  // sort of.
  // the input size and output size only differ in the fastest moving dimension
  unsigned int total_outputSize = 1;
  unsigned int total_inputSize = 1;

  for ( unsigned i = 0; i < ImageDimension; i++ )
    {
    total_outputSize *= outputSize[i];
    total_inputSize *= inputSize[i];
    }

  int transformDirection = 1;
  if ( this->GetTransformDirection() == Superclass::INVERSE )
    {
    transformDirection = -1;
    }

  typename FFTWProxyType::PlanType plan;
  typename FFTWProxyType::ComplexType * in = (typename FFTWProxyType::ComplexType*) inputPtr->GetBufferPointer();
  typename FFTWProxyType::ComplexType * out = (typename FFTWProxyType::ComplexType*) outputPtr->GetBufferPointer();
  int flags = m_PlanRigor;
  if( !m_CanUseDestructiveAlgorithm )
    {
    // if the input is about to be destroyed, there is no need to force fftw
    // to use an non destructive algorithm. If it is not released however,
    // we must be careful to not destroy it.
    flags = flags | FFTW_PRESERVE_INPUT;
    }

  int *sizes = new int[ImageDimension];
  for(unsigned int i = 0; i < ImageDimension; i++)
    {
    sizes[(ImageDimension - 1) - i] = inputSize[i];
    }

  plan = FFTWProxyType::Plan_dft(ImageDimension,sizes,
                                    in,
                                    out,
                                    transformDirection,
                                    flags,
                                    this->GetNumberOfThreads());
  delete[] sizes;

  FFTWProxyType::Execute(plan);
  FFTWProxyType::DestroyPlan(plan);
}

template <typename TImage>
void
FFTWComplexToComplexImageFilter< TImage >::
ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, ThreadIdType itkNotUsed(threadId) )
{
  //
  // Normalize the output if backward transform
  //
  if ( this->GetTransformDirection() == Superclass::INVERSE )
    {
    typedef ImageRegionIterator< OutputImageType >   IteratorType;
    unsigned long total_outputSize = this->GetOutput()->GetRequestedRegion().GetNumberOfPixels();
    IteratorType it(this->GetOutput(), outputRegionForThread);
    while( !it.IsAtEnd() )
      {
      PixelType val = it.Value();
      val /= total_outputSize;
      it.Set(val);
      ++it;
      }
    }
}

template< typename TImage >
bool
FFTWComplexToComplexImageFilter< TImage >::FullMatrix()
{
  return false;
}


template< typename TImage >
void
FFTWComplexToComplexImageFilter< TImage >::
UpdateOutputData(DataObject * output)
{
  // we need to catch that information now, because it is changed later
  // during the pipeline execution, and thus can't be grabbed in
  // GenerateData().
  m_CanUseDestructiveAlgorithm = this->GetInput()->GetReleaseDataFlag();
  Superclass::UpdateOutputData( output );
}

template< typename TImage >
void
FFTWComplexToComplexImageFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "PlanRigor: " << FFTWGlobalConfiguration::GetPlanRigorName(m_PlanRigor) << " (" << m_PlanRigor << ")" << std::endl;
}

} // namespace itk
#endif // _itkFFTWComplexToComplexImageFilter_hxx
