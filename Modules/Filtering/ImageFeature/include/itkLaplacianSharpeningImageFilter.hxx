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
#include "itkImageRegionIterator.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
void
LaplacianSharpeningImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "UseImageSpacing: " << (m_UseImageSpacing ? "On" : "Off") << std::endl;
}

template <typename TInputImage, typename TOutputImage>
void
LaplacianSharpeningImageFilter<TInputImage, TOutputImage>::GenerateData()
{

  // Calculate the Laplacian filtered image

  using LaplacianImageFilter = LaplacianImageFilter<InputImageType, OutputImageType>;
  typename LaplacianImageFilter::Pointer laplacianFilter = LaplacianImageFilter::New();
  laplacianFilter->SetInput(this->GetInput());
  laplacianFilter->SetUseImageSpacing(m_UseImageSpacing);
  laplacianFilter->Update();

  // Determine how the data will need to scaled to be properly combined

  typename MinimumMaximumImageCalculator<InputImageType>::Pointer inputCalculator =
    MinimumMaximumImageCalculator<InputImageType>::New();
  typename MinimumMaximumImageCalculator<OutputImageType>::Pointer filteredCalculator =
    MinimumMaximumImageCalculator<OutputImageType>::New();

  inputCalculator->SetImage(this->GetInput());
  inputCalculator->SetRegion(this->GetInput()->GetRequestedRegion());
  inputCalculator->Compute();

  filteredCalculator->SetImage(laplacianFilter->GetOutput());
  filteredCalculator->SetRegion(laplacianFilter->GetOutput()->GetRequestedRegion());
  filteredCalculator->Compute();

  RealType inputShift = static_cast<RealType>(inputCalculator->GetMinimum());
  RealType inputScale = static_cast<RealType>(inputCalculator->GetMaximum()) - inputShift;

  RealType filteredShift = static_cast<RealType>(filteredCalculator->GetMinimum());
  RealType filteredScale = static_cast<RealType>(filteredCalculator->GetMaximum()) - filteredShift;

  ImageRegionIterator<OutputImageType>     it(laplacianFilter->GetOutput(),
                                          laplacianFilter->GetOutput()->GetRequestedRegion());
  ImageRegionConstIterator<InputImageType> inIt(this->GetInput(), this->GetInput()->GetRequestedRegion());

  // combine the input and laplacian images
  RealType inputSum = 0.0;
  RealType enhancedSum = 0.0;
  while (!it.IsAtEnd())
  {
    RealType value = static_cast<RealType>(it.Get()); // laplacian value

    // rescale to [0,1]
    value = (value - filteredShift) / filteredScale;

    // rescale to the input dynamic range
    value = value * inputScale + inputShift;

    // combine the input and laplacian image (note that we subtract
    // the laplacian due to the signs in our laplacian kernel).
    RealType invalue = static_cast<RealType>(inIt.Get());
    value = invalue - value;
    it.Set(value);

    inputSum += invalue;
    enhancedSum += value;
    ++it;
    ++inIt;
  }
  RealType numberOfVoxels = static_cast<RealType>(this->GetInput()->GetRequestedRegion().GetNumberOfPixels());
  RealType inputMean = inputSum / numberOfVoxels;
  RealType enhancedMean = enhancedSum / numberOfVoxels;

  auto inputMinimum = static_cast<RealType>(inputCalculator->GetMinimum());
  auto inputMaximum = static_cast<RealType>(inputCalculator->GetMaximum());

  // Allocate and write the output

  typename TOutputImage::Pointer output = this->GetOutput();
  output->CopyInformation(this->GetInput());
  output->SetRegions(output->GetRequestedRegion());
  output->Allocate();

  ImageRegionIterator<OutputImageType> outIt(output, output->GetRequestedRegion());
  it.GoToBegin();
  while (!outIt.IsAtEnd())
  {
    // adjust value to make the mean intensities before and after match
    RealType outValue = static_cast<RealType>(it.Get()) - enhancedMean + inputMean;

    if (outValue < inputMinimum)
    {
      outValue = inputMinimum;
    }
    else if (outValue > inputMaximum)
    {
      outValue = inputMaximum;
    }

    outIt.Set(static_cast<OutputPixelType>(outValue));

    ++outIt;
    ++it;
  }
}
} // end namespace itk

#endif
