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
#ifndef itkGrayscaleErodeImageFilter_hxx
#define itkGrayscaleErodeImageFilter_hxx

#include "itkGrayscaleErodeImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressAccumulator.h"
#include <string>

namespace itk
{
template <typename TInputImage, typename TOutputImage, typename TKernel>
GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::GrayscaleErodeImageFilter()
{
  m_BasicFilter = BasicFilterType::New();
  m_HistogramFilter = HistogramFilterType::New();
  m_AnchorFilter = AnchorFilterType::New();
  m_VHGWFilter = VHGWFilterType::New();
  m_Algorithm = AlgorithmEnum::HISTO;

  this->SetBoundary(NumericTraits<PixelType>::max());
}

template <typename TInputImage, typename TOutputImage, typename TKernel>
void
GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::SetNumberOfWorkUnits(ThreadIdType nb)
{
  Superclass::SetNumberOfWorkUnits(nb);
  m_HistogramFilter->SetNumberOfWorkUnits(nb);
  m_AnchorFilter->SetNumberOfWorkUnits(nb);
  m_VHGWFilter->SetNumberOfWorkUnits(nb);
  m_BasicFilter->SetNumberOfWorkUnits(nb);
}

template <typename TInputImage, typename TOutputImage, typename TKernel>
void
GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::SetKernel(const KernelType & kernel)
{
  const auto * flatKernel = dynamic_cast<const FlatKernelType *>(&kernel);

  if (flatKernel != nullptr && flatKernel->GetDecomposable())
  {
    m_AnchorFilter->SetKernel(*flatKernel);
    m_Algorithm = AlgorithmEnum::ANCHOR;
  }
  else if (m_HistogramFilter->GetUseVectorBasedAlgorithm())
  {
    // histogram based filter is as least as good as the basic one, so always
    // use it
    m_Algorithm = AlgorithmEnum::HISTO;
    m_HistogramFilter->SetKernel(kernel);
  }
  else
  {
    // basic filter can be better than the histogram based one
    // apply a poor heuristic to find the best one. What is very important is to
    // select the histogram for large kernels

    // we need to set the kernel on the histogram filter to compare basic and
    // histogram algorithm
    m_HistogramFilter->SetKernel(kernel);

    if ((ImageDimension == 2 && this->GetKernel().Size() < m_HistogramFilter->GetPixelsPerTranslation() * 5.4) ||
        (ImageDimension == 3 && this->GetKernel().Size() < m_HistogramFilter->GetPixelsPerTranslation() * 4.5))
    {
      m_BasicFilter->SetKernel(kernel);
      m_Algorithm = AlgorithmEnum::BASIC;
    }
    else
    {
      m_Algorithm = AlgorithmEnum::HISTO;
    }
  }

  Superclass::SetKernel(kernel);
}

template <typename TInputImage, typename TOutputImage, typename TKernel>
void
GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::SetBoundary(const PixelType value)
{
  m_Boundary = value;
  m_HistogramFilter->SetBoundary(value);
  m_AnchorFilter->SetBoundary(value);
  m_VHGWFilter->SetBoundary(value);
  m_BoundaryCondition.SetConstant(value);
  m_BasicFilter->OverrideBoundaryCondition(&m_BoundaryCondition);
}

template <typename TInputImage, typename TOutputImage, typename TKernel>
void
GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::SetAlgorithm(AlgorithmEnum algo)
{
  const auto * flatKernel = dynamic_cast<const FlatKernelType *>(&this->GetKernel());

  if (m_Algorithm != algo)
  {
    if (algo == AlgorithmEnum::BASIC)
    {
      m_BasicFilter->SetKernel(this->GetKernel());
    }
    else if (algo == AlgorithmEnum::HISTO)
    {
      m_HistogramFilter->SetKernel(this->GetKernel());
    }
    else if (flatKernel != nullptr && flatKernel->GetDecomposable() && algo == AlgorithmEnum::ANCHOR)
    {
      m_AnchorFilter->SetKernel(*flatKernel);
    }
    else if (flatKernel != nullptr && flatKernel->GetDecomposable() && algo == AlgorithmEnum::VHGW)
    {
      m_VHGWFilter->SetKernel(*flatKernel);
    }
    else
    {
      itkExceptionMacro(<< "Invalid algorithm");
    }

    m_Algorithm = algo;
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage, typename TKernel>
void
GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to the appropriate erosion filter
  if (m_Algorithm == AlgorithmEnum::BASIC)
  {
    itkDebugMacro("Running BasicErodeImageFilter");
    m_BasicFilter->SetInput(this->GetInput());
    progress->RegisterInternalFilter(m_BasicFilter, 1.0f);

    m_BasicFilter->GraftOutput(this->GetOutput());
    m_BasicFilter->Update();
    this->GraftOutput(m_BasicFilter->GetOutput());
  }
  else if (m_Algorithm == AlgorithmEnum::HISTO)
  {
    itkDebugMacro("Running MovingHistogramErodeImageFilter");
    m_HistogramFilter->SetInput(this->GetInput());
    progress->RegisterInternalFilter(m_HistogramFilter, 1.0f);

    m_HistogramFilter->GraftOutput(this->GetOutput());
    m_HistogramFilter->Update();
    this->GraftOutput(m_HistogramFilter->GetOutput());
  }
  else if (m_Algorithm == AlgorithmEnum::ANCHOR)
  {
    itkDebugMacro("Running AnchorErodeImageFilter");
    m_AnchorFilter->SetInput(this->GetInput());
    progress->RegisterInternalFilter(m_AnchorFilter, 0.9f);

    typename CastFilterType::Pointer cast = CastFilterType::New();
    cast->SetInput(m_AnchorFilter->GetOutput());
    progress->RegisterInternalFilter(cast, 0.1f);

    cast->GraftOutput(this->GetOutput());
    cast->Update();
    this->GraftOutput(cast->GetOutput());
  }
  else if (m_Algorithm == AlgorithmEnum::VHGW)
  {
    itkDebugMacro("Running VanHerkGilWermanErodeImageFilter");
    m_VHGWFilter->SetInput(this->GetInput());
    progress->RegisterInternalFilter(m_VHGWFilter, 0.9f);

    typename CastFilterType::Pointer cast = CastFilterType::New();
    cast->SetInput(m_VHGWFilter->GetOutput());
    progress->RegisterInternalFilter(cast, 0.1f);

    cast->GraftOutput(this->GetOutput());
    cast->Update();
    this->GraftOutput(cast->GetOutput());
  }
}

template <typename TInputImage, typename TOutputImage, typename TKernel>
void
GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::Modified() const
{
  Superclass::Modified();
  m_BasicFilter->Modified();
  m_HistogramFilter->Modified();
  m_AnchorFilter->Modified();
  m_VHGWFilter->Modified();
}

template <typename TInputImage, typename TOutputImage, typename TKernel>
void
GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Boundary: " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_Boundary) << std::endl;
  os << indent << "Algorithm: " << m_Algorithm << std::endl;
}
} // end namespace itk
#endif
