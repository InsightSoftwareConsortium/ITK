/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkFFTWComplexToComplexFFTImageFilter_hxx
#define itkFFTWComplexToComplexFFTImageFilter_hxx

#include "itkFFTWComplexToComplexFFTImageFilter.h"
#include "itkIndent.h"
#include "itkMetaDataObject.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"


/*
 *
 * This code was contributed in the Insight Journal paper:
 * "FFT Complex to Complex filters and helper classes"
 * by Warfield S.
 * https://hdl.handle.net/1926/326
 * http://www.insight-journal.org/browse/publication/128
 *
 */

namespace itk
{

template <typename TImage>
FFTWComplexToComplexFFTImageFilter<TImage>::FFTWComplexToComplexFFTImageFilter()
#ifndef ITK_USE_CUFFTW
  : m_PlanRigor(FFTWGlobalConfiguration::GetPlanRigor())
#endif
{
  this->DynamicMultiThreadingOn();
}


template <typename TImage>
void
FFTWComplexToComplexFFTImageFilter<TImage>::BeforeThreadedGenerateData()
{
  // get pointers to the input and output
  const InputImageType * input = this->GetInput();
  OutputImageType *      output = this->GetOutput();

  if (!input || !output)
  {
    return;
  }

  // we don't have a nice progress to report, but at least this simple line
  // reports the beginning and the end of the process
  ProgressReporter progress(this, 0, 1);

  // allocate output buffer memory
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  const typename OutputImageType::SizeType & inputSize = input->GetLargestPossibleRegion().GetSize();

  int transformDirection = 1;
  if (this->GetTransformDirection() == Superclass::TransformDirectionEnum::INVERSE)
  {
    transformDirection = -1;
  }

  typename FFTWProxyType::PlanType plan;
  auto *                           in = (typename FFTWProxyType::ComplexType *)input->GetBufferPointer();
  auto *                           out = (typename FFTWProxyType::ComplexType *)output->GetBufferPointer();
  int                              flags = m_PlanRigor;
  if (!m_CanUseDestructiveAlgorithm)
  {
    // if the input is about to be destroyed, there is no need to force fftw
    // to use an non destructive algorithm. If it is not released however,
    // we must be careful to not destroy it.
    flags = flags | FFTW_PRESERVE_INPUT;
  }

  int sizes[ImageDimension];
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    sizes[(ImageDimension - 1) - i] = inputSize[i];
  }

  plan =
    FFTWProxyType::Plan_dft(ImageDimension, sizes, in, out, transformDirection, flags, this->GetNumberOfWorkUnits());

  FFTWProxyType::Execute(plan);
  FFTWProxyType::DestroyPlan(plan);
}


template <typename TImage>
void
FFTWComplexToComplexFFTImageFilter<TImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  // Normalize the output if backward transform
  if (this->GetTransformDirection() == Superclass::TransformDirectionEnum::INVERSE)
  {
    using IteratorType = ImageRegionIterator<OutputImageType>;
    SizeValueType totalOutputSize = this->GetOutput()->GetRequestedRegion().GetNumberOfPixels();
    IteratorType  it(this->GetOutput(), outputRegionForThread);
    while (!it.IsAtEnd())
    {
      PixelType val = it.Value();
      val /= totalOutputSize;
      it.Set(val);
      ++it;
    }
  }
}


template <typename TImage>
void
FFTWComplexToComplexFFTImageFilter<TImage>::UpdateOutputData(DataObject * output)
{
  // we need to catch that information now, because it is changed later
  // during the pipeline execution, and thus can't be grabbed in
  // GenerateData().
  m_CanUseDestructiveAlgorithm = this->GetInput()->GetReleaseDataFlag();
  Superclass::UpdateOutputData(output);
}


template <typename TImage>
void
FFTWComplexToComplexFFTImageFilter<TImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

#ifndef ITK_USE_CUFFTW
  os << indent << "PlanRigor: " << FFTWGlobalConfiguration::GetPlanRigorName(m_PlanRigor) << " (" << m_PlanRigor << ")"
     << std::endl;
#endif
}

} // end namespace itk

#endif // _itkFFTWComplexToComplexFFTImageFilter_hxx
