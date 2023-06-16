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
#include "itkMinimumMaximumImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkUnaryGeneratorImageFilter.h"

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

  // Calculate the needed input image statistics

  typename StatisticsImageFilter<InputImageType>::Pointer inputCalculator =
    StatisticsImageFilter<InputImageType>::New();

  inputCalculator->SetInput(localInput);
  inputCalculator->Update();

  auto inputMinimum = static_cast<RealType>(inputCalculator->GetMinimum());
  auto inputMaximum = static_cast<RealType>(inputCalculator->GetMaximum());
  auto inputMean = static_cast<RealType>(inputCalculator->GetMean());

  auto inputShift = inputMinimum;
  auto inputScale = inputMaximum - inputMinimum;
  inputCalculator = nullptr;

  // Calculate the Laplacian filtered image

  using RealImageType = Image<RealType, ImageDimension>;

  // Create the Laplacian operator
  LaplacianOperator<RealType, ImageDimension> oper;
  double                                      s[ImageDimension];
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    if (localInput->GetSpacing()[i] == 0.0)
    {
      itkExceptionMacro("Image spacing cannot be zero");
    }
    else if (this->m_UseImageSpacing)
    {
      s[i] = 1.0 / localInput->GetSpacing()[i];
    }
    else
    {
      s[i] = 1.0;
    }
  }
  oper.SetDerivativeScalings(s);
  oper.CreateOperator();
  // Calculate the Laplacian filtered image

  // do calculations in floating point
  using RealImageType = Image<RealType, ImageDimension>;
  using NOIF = NeighborhoodOperatorImageFilter<InputImageType, RealImageType>;
  ZeroFluxNeumannBoundaryCondition<InputImageType> nbc;

  auto laplacianFilter = NOIF::New();
  laplacianFilter->OverrideBoundaryCondition(static_cast<typename NOIF::ImageBoundaryConditionPointerType>(&nbc));

  //
  // set up the mini-pipeline
  //
  laplacianFilter->SetOperator(oper);
  laplacianFilter->SetInput(localInput);
  laplacianFilter->Update();

  auto filteredMinMaxFilter = MinimumMaximumImageFilter<RealImageType>::New();
  filteredMinMaxFilter->SetInput(laplacianFilter->GetOutput());
  filteredMinMaxFilter->Update();

  RealType filteredShift = static_cast<RealType>(filteredMinMaxFilter->GetMinimum());
  RealType filteredScale = static_cast<RealType>(filteredMinMaxFilter->GetMaximum()) - filteredShift;
  filteredMinMaxFilter = nullptr;

  // Combine the input image and the laplacian image
  auto binaryFilter = itk::BinaryGeneratorImageFilter<RealImageType, InputImageType, RealImageType>::New();
  binaryFilter->SetInput1(laplacianFilter->GetOutput());
  binaryFilter->SetInput2(localInput);


  binaryFilter->SetFunctor(
    [filteredShift, inputScale, filteredScale, inputShift](const typename RealImageType::PixelType &  filteredPixel,
                                                           const typename InputImageType::PixelType & inputPixel) {
      return inputPixel - ((filteredPixel - filteredShift) * (inputScale / filteredScale) + inputShift);
    });
  binaryFilter->InPlaceOn();
  binaryFilter->Update();

  // Calculate needed combined image statistics
  typename StatisticsImageFilter<RealImageType>::Pointer enhancedCalculator =
    StatisticsImageFilter<RealImageType>::New();

  enhancedCalculator->SetInput(binaryFilter->GetOutput());
  enhancedCalculator->Update();

  auto enhancedMean = static_cast<RealType>(enhancedCalculator->GetMean());
  enhancedCalculator = nullptr;

  // Shift and window the output
  auto shiftAndClampFilter = UnaryGeneratorImageFilter<RealImageType, OutputImageType>::New();

  shiftAndClampFilter->SetInput(binaryFilter->GetOutput());
  shiftAndClampFilter->SetFunctor(
    [enhancedMean, inputMean, inputMinimum, inputMaximum](const typename RealImageType::PixelType & value) {
      // adjust value to make the mean intensities before and after match
      auto shiftedValue = value - enhancedMean + inputMean;
      return static_cast<OutputPixelType>(std::clamp(shiftedValue, inputMinimum, inputMaximum));
    });
  shiftAndClampFilter->GraftOutput(this->GetOutput());
  shiftAndClampFilter->Update();


  this->GraftOutput(shiftAndClampFilter->GetOutput());
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
