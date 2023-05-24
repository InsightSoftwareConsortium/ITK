/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkLaplacianSharpeningImageFilter_hxx
#define itkLaplacianSharpeningImageFilter_hxx

#include "itkLaplacianImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkStatisticsImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkIntensityWindowingImageFilter.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
LaplacianSharpeningImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  Superclass::EnlargeOutputRequestedRegion(output);

  if (output != nullptr)
  {
    output->SetRequestedRegionToLargestPossibleRegion();
  }
}

template <typename TInputImage, typename TOutputImage>
void
LaplacianSharpeningImageFilter<TInputImage, TOutputImage>::GenerateData()
{

  auto localInput = TInputImage::New();
  localInput->Graft(this->GetInput());

  // Calculate needed input image statistics

  typename StatisticsImageFilter<InputImageType>::Pointer inputCalculator =
    StatisticsImageFilter<InputImageType>::New();

  inputCalculator->SetInput(localInput);
  inputCalculator->Update();

  auto inputMinimum = static_cast<RealType>(inputCalculator->GetMinimum());
  auto inputMaximum = static_cast<RealType>(inputCalculator->GetMaximum());
  auto inputMean = static_cast<RealType>(inputCalculator->GetMean());

  auto inputShift = inputMinimum;
  auto inputScale = inputMaximum - inputMinimum;

  // Calculate the Laplacian filtered image

  using RealImageType = Image<RealType, ImageDimension>;

  using LaplacianImageFilter = LaplacianImageFilter<InputImageType, RealImageType>;
  typename LaplacianImageFilter::Pointer laplacianFilter = LaplacianImageFilter::New();
  laplacianFilter->SetInput(localInput);
  laplacianFilter->SetUseImageSpacing(m_UseImageSpacing);
  laplacianFilter->Update();

  // Calculate needed laplacian filtered image statistics

  typename MinimumMaximumImageCalculator<RealImageType>::Pointer filteredCalculator =
    MinimumMaximumImageCalculator<RealImageType>::New();

  filteredCalculator->SetImage(laplacianFilter->GetOutput());
  filteredCalculator->SetRegion(laplacianFilter->GetOutput()->GetRequestedRegion());
  filteredCalculator->Compute();

  RealType filteredShift = static_cast<RealType>(filteredCalculator->GetMinimum());
  RealType filteredScale = static_cast<RealType>(filteredCalculator->GetMaximum()) - filteredShift;

  // Combine the input image and the laplacian image

  using AddImageFilterType = AddImageFilter<RealImageType>;
  using MultiplyImageFilterType = MultiplyImageFilter<RealImageType>;
  using SubtractImageFilterType = SubtractImageFilter<InputImageType, RealImageType, RealImageType>;

  auto addImageFilter1 = AddImageFilterType::New();
  addImageFilter1->SetInput(laplacianFilter->GetOutput());
  addImageFilter1->SetConstant2(-filteredShift);

  auto multiplyImageFilter = MultiplyImageFilterType::New();
  multiplyImageFilter->SetInput(addImageFilter1->GetOutput());
  multiplyImageFilter->SetConstant(inputScale / filteredScale);

  auto addImageFilter2 = AddImageFilterType::New();
  addImageFilter2->SetInput(multiplyImageFilter->GetOutput());
  addImageFilter2->SetConstant2(inputShift);

  auto subtractImageFilter = SubtractImageFilterType::New();
  subtractImageFilter->SetInput1(localInput);
  subtractImageFilter->SetInput2(addImageFilter2->GetOutput());

  // Calculate needed combined image statistics

  typename StatisticsImageFilter<RealImageType>::Pointer enhancedCalculator =
    StatisticsImageFilter<RealImageType>::New();

  enhancedCalculator->SetInput(subtractImageFilter->GetOutput());
  enhancedCalculator->Update();

  auto enhancedMean = static_cast<RealType>(enhancedCalculator->GetMean());

  // Allocate and write the output

  using IntensityWindowingFilterType = IntensityWindowingImageFilter<RealImageType, OutputImageType>;
  auto intensityWindowingFilter = IntensityWindowingFilterType::New();
  intensityWindowingFilter->SetInput(subtractImageFilter->GetOutput());
  intensityWindowingFilter->SetOutputMinimum(inputMinimum);
  intensityWindowingFilter->SetOutputMaximum(inputMaximum);
  intensityWindowingFilter->SetWindowMinimum(inputMinimum - (inputMean - enhancedMean));
  intensityWindowingFilter->SetWindowMaximum(inputMaximum - (inputMean - enhancedMean));
  intensityWindowingFilter->Update();

  this->GraftOutput(intensityWindowingFilter->GetOutput());
}

template <typename TInputImage, typename TOutputImage>
void
LaplacianSharpeningImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "UseImageSpacing: " << (m_UseImageSpacing ? "On" : "Off") << std::endl;
}

} // end namespace itk

#endif
