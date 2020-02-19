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

#ifndef itkKrcahPreprocessingImageToImageFilter_hxx
#define itkKrcahPreprocessingImageToImageFilter_hxx

#include "itkKrcahPreprocessingImageToImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkMath.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
KrcahPreprocessingImageToImageFilter<TInputImage, TOutputImage>::KrcahPreprocessingImageToImageFilter()
  : m_Sigma(1.0f)
  , m_ScalingConstant(10.0f)

{
  /* Instantiate all filters */
  m_GaussianFilter = GaussianFilterType::New();
  m_SubtractFilter = SubstractFilterType::New();
  m_MultiplyFilter = MultiplyFilterType::New();
  m_AddFilter = AddFilterType::New();
}

template <typename TInputImage, typename TOutputImage>
void
KrcahPreprocessingImageToImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // Gaussian filter needs expanding around kernel. Since this filter is typically used
  // with the Kcrah enhancing filter, we just expand everything.
  Superclass::GenerateInputRequestedRegion();
  auto * input = const_cast<TInputImage *>(this->GetInput());
  if (input)
  {
    input->SetRequestedRegionToLargestPossibleRegion();
  }
}

template <typename TInputImage, typename TOutputImage>
void
KrcahPreprocessingImageToImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  /* Get Input */
  InputImageConstPointer input = this->GetInput();

  /* I*G */
  m_GaussianFilter->SetInput(input);
  m_GaussianFilter->SetVariance(Math::squared_magnitude(this->GetSigma()));

  /* I - I*G */
  m_SubtractFilter->SetInput1(input);
  m_SubtractFilter->SetInput2(m_GaussianFilter->GetOutput());

  /* k(I-(I*G)) */
  m_MultiplyFilter->SetInput(m_SubtractFilter->GetOutput());
  m_MultiplyFilter->SetConstant(this->GetScalingConstant());

  /* I+k*(I-(I*G)) */
  m_AddFilter->SetInput1(input);
  m_AddFilter->SetInput2(m_MultiplyFilter->GetOutput());

  /* Release data if asked */
  if (this->GetReleaseInternalFilterData())
  {
    m_GaussianFilter->ReleaseDataFlagOn();
    m_SubtractFilter->ReleaseDataFlagOn();
    m_MultiplyFilter->ReleaseDataFlagOn();
    m_AddFilter->ReleaseDataFlagOn();
  }

  /* Setup progress reporter */
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(m_GaussianFilter, 0.25f);
  progress->RegisterInternalFilter(m_SubtractFilter, 0.25f);
  progress->RegisterInternalFilter(m_MultiplyFilter, 0.25f);
  progress->RegisterInternalFilter(m_AddFilter, 0.25f);

  /* Graft Output */
  m_AddFilter->GraftOutput(this->GetOutput());
  m_AddFilter->Update();
  this->GraftOutput(m_AddFilter->GetOutput());
}

template <typename TInputImage, typename TOutputImage>
void
KrcahPreprocessingImageToImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "GaussianFilter: " << m_GaussianFilter.GetPointer() << std::endl;
  os << indent << "SubtractFilter: " << m_SubtractFilter.GetPointer() << std::endl;
  os << indent << "MultiplyFilter: " << m_MultiplyFilter.GetPointer() << std::endl;
  os << indent << "AddFilter: " << m_AddFilter.GetPointer() << std::endl;
  os << indent << "Sigma: " << GetSigma() << std::endl;
  os << indent << "ScalingConstant: " << GetScalingConstant() << std::endl;
  os << indent << "ReleaseInternalFilterData: " << GetReleaseInternalFilterData() << std::endl;
}

} // namespace itk

#endif // itkKrcahPreprocessingImageToImageFilter_hxx
