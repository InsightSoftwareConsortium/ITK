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
#ifndef __itkFFTWRealToComplexConjugateImageFilter_txx
#define __itkFFTWRealToComplexConjugateImageFilter_txx

#include "itkFFTWRealToComplexConjugateImageFilter.h"
#include "itkFFTRealToComplexConjugateImageFilter.txx"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
#include "itkProgressReporter.h"

namespace itk
{
/** TODO:  There should be compile time type checks so that
           if only USE_FFTWF is defined, then only floats are valid.
           and if USE_FFTWD is defined, then only doubles are valid.
*/

template< typename TPixel, unsigned int VDimension >
void
FFTWRealToComplexConjugateImageFilter< TPixel, VDimension >::GenerateData()
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

  const typename TInputImageType::SizeType &   inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TOutputImageType::SizeType &   outputSize =
    outputPtr->GetLargestPossibleRegion().GetSize();

  // figure out sizes
  // size of input and output aren't the same which is handled in the superclass,
  // sort of.
  // the input size and output size only differ in the fastest moving dimension
  unsigned int total_inputSize = 1;
  unsigned int total_outputSize = 1;

  for ( unsigned i = 0; i < VDimension; i++ )
    {
    total_inputSize *= inputSize[i];
    total_outputSize *= outputSize[i];
    }

  typename FFTWProxyType::PlanType plan;
  TPixel * in = const_cast<TPixel*>(inputPtr->GetBufferPointer());
  typename FFTWProxyType::ComplexType * out = (typename FFTWProxyType::ComplexType*) outputPtr->GetBufferPointer();
  int flags = m_PlanRigor;
  if( !m_CanUseDestructiveAlgorithm )
    {
    // if the input is about to be destroyed, there is no need to force fftw
    // to use an non destructive algorithm. If it is not released however,
    // we must be careful to not destroy it.
    flags = flags | FFTW_PRESERVE_INPUT;
    }
  int *sizes = new int[VDimension];
  for(unsigned int i = 0; i < VDimension; i++)
    {
    sizes[(VDimension - 1) - i] = inputSize[i];
    }

  plan = FFTWProxyType::Plan_dft_r2c(VDimension,sizes,
                                    in,
                                    out,
                                    flags,
                                    this->GetNumberOfThreads());
  delete [] sizes;
  FFTWProxyType::Execute(plan);
  FFTWProxyType::DestroyPlan(plan);
}

template< typename TPixel, unsigned int VDimension >
bool
FFTWRealToComplexConjugateImageFilter< TPixel, VDimension >::FullMatrix()
{
  return false;
}

template< typename TPixel, unsigned int VDimension >
void
FFTWRealToComplexConjugateImageFilter< TPixel, VDimension >::
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
FFTWRealToComplexConjugateImageFilter< TPixel, VDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "PlanRigor: " << FFTWGlobalConfiguration::GetPlanRigorName(m_PlanRigor) << " (" << m_PlanRigor << ")" << std::endl;
}

} // namespace itk

#endif //_itkFFTWRealToComplexConjugateImageFilter_txx
