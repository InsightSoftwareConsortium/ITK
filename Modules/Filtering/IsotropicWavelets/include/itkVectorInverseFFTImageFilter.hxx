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
#ifndef itkVectorInverseFFTImageFilter_hxx
#define itkVectorInverseFFTImageFilter_hxx
#include "itkVectorInverseFFTImageFilter.h"
#include <itkVectorIndexSelectionCastImageFilter.h>
#include <itkComposeImageFilter.h>
#include <itkProgressAccumulator.h>

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
VectorInverseFFTImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  auto progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  this->AllocateOutputs();

  using InputSingleImageType = Image<typename InputImageType::PixelType::ComponentType, ImageDimension>;
  using VectorCastFilterType = VectorIndexSelectionCastImageFilter<InputImageType, InputSingleImageType>;
  using FFTInverseFilterType = InverseFFTImageFilter<InputSingleImageType>;
  using OutputSingleImageType = typename FFTInverseFilterType::OutputImageType;
  using ComposeFilterType = ComposeImageFilter<OutputSingleImageType, OutputImageType>;

  auto vectorCastFilter = VectorCastFilterType::New();
  vectorCastFilter->SetInput(this->GetInput());
  progress->RegisterInternalFilter(vectorCastFilter, 1.0 / this->GetInput()->GetNumberOfComponentsPerPixel());
  auto fftInverseFilter = FFTInverseFilterType::New();
  auto composeFilter = ComposeFilterType::New();

  std::vector<typename OutputSingleImageType::Pointer> inverseFFToutputs;
  for (unsigned int c = 0; c < this->GetInput()->GetNumberOfComponentsPerPixel(); c++)
  {
    vectorCastFilter->SetIndex(c);
    vectorCastFilter->Update();
    fftInverseFilter->SetInput(vectorCastFilter->GetOutput());
    fftInverseFilter->Update();
    inverseFFToutputs.push_back(fftInverseFilter->GetOutput());
    inverseFFToutputs.back()->DisconnectPipeline();
    composeFilter->SetInput(c, inverseFFToutputs.back());
  }
  composeFilter->GraftOutput(outputPtr);
  composeFilter->Update();

  this->GraftOutput(composeFilter->GetOutput());
}

template <typename TInputImage, typename TOutputImage>
void
VectorInverseFFTImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
