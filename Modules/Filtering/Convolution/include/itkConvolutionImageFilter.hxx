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

#ifndef itkConvolutionImageFilter_hxx
#define itkConvolutionImageFilter_hxx


#include "itkConstantPadImageFilter.h"
#include "itkCropImageFilter.h"
#include "itkFlipImageFilter.h"
#include "itkImageBase.h"
#include "itkImageKernelOperator.h"
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkNormalizeToConstantImageFilter.h"

namespace itk
{
template <typename TInputImage, typename TKernelImage, typename TOutputImage>
void
ConvolutionImageFilter<TInputImage, TKernelImage, TOutputImage>::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  // Create a process accumulator for tracking the progress of this minipipeline
  auto progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Build a mini-pipeline that involves a
  // NeighborhoodOperatorImageFilter to compute the convolution, a
  // normalization filter for the kernel, and a pad filter for making
  // the kernel an odd size.
  if (this->GetNormalize())
  {
    using RealPixelType = typename NumericTraits<typename TKernelImage::PixelType>::RealType;
    using RealImageType = Image<RealPixelType, ImageDimension>;

    using NormalizeFilterType = NormalizeToConstantImageFilter<KernelImageType, RealImageType>;
    auto normalizeFilter = NormalizeFilterType::New();
    normalizeFilter->SetConstant(NumericTraits<RealPixelType>::OneValue());
    normalizeFilter->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
    normalizeFilter->SetInput(this->GetKernelImage());
    normalizeFilter->ReleaseDataFlagOn();
    progress->RegisterInternalFilter(normalizeFilter, 0.1f);
    normalizeFilter->UpdateLargestPossibleRegion();

    this->ComputeConvolution(normalizeFilter->GetOutput(), progress);
  }
  else
  {
    this->ComputeConvolution(this->GetKernelImage(), progress);
  }
}

template <typename TInputImage, typename TKernelImage, typename TOutputImage>
template <typename TImage>
void
ConvolutionImageFilter<TInputImage, TKernelImage, TOutputImage>::ComputeConvolution(const TImage *        kernelImage,
                                                                                    ProgressAccumulator * progress)
{
  using KernelImagePixelType = typename TImage::PixelType;
  using KernelOperatorType = ImageKernelOperator<KernelImagePixelType, ImageDimension>;
  KernelOperatorType kernelOperator;

  const bool kernelNeedsPadding = this->GetKernelNeedsPadding();

  float optionalFilterWeights = 0.0f;
  if (this->GetNormalize())
  {
    optionalFilterWeights += 0.1f;
  }
  if (this->GetKernelNeedsPadding())
  {
    optionalFilterWeights += 0.1f;
  }
  if (this->GetOutputRegionMode() == ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::VALID)
  {
    optionalFilterWeights += 0.1f;
  }

  // Flip the kernel
  using FlipperType = FlipImageFilter<TImage>;
  auto           flipper = FlipperType::New();
  constexpr auto axesArray = MakeFilled<typename FlipperType::FlipAxesArrayType>(true);
  flipper->SetFlipAxes(axesArray);
  flipper->SetInput(kernelImage);

  if (kernelNeedsPadding)
  {
    // Pad the kernel if necessary to an odd size in each dimension.
    using PadImageFilterType = ConstantPadImageFilter<TImage, TImage>;
    auto kernelPadImageFilter = PadImageFilterType::New();
    kernelPadImageFilter->SetConstant(KernelImagePixelType{});
    kernelPadImageFilter->SetPadLowerBound(this->GetKernelPadSize());
    kernelPadImageFilter->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
    kernelPadImageFilter->ReleaseDataFlagOn();
    kernelPadImageFilter->SetInput(flipper->GetOutput());
    progress->RegisterInternalFilter(kernelPadImageFilter, 0.1f);
    kernelPadImageFilter->UpdateLargestPossibleRegion();

    kernelOperator.SetImageKernel(kernelPadImageFilter->GetOutput());
  }
  else
  {
    flipper->UpdateLargestPossibleRegion();
    kernelOperator.SetImageKernel(flipper->GetOutput());
  }

  const KernelSizeType radius = this->GetKernelRadius(kernelImage);
  kernelOperator.CreateToRadius(radius);

  auto localInput = InputImageType::New();
  localInput->Graft(this->GetInput());

  // The NeighborhoodOperatorImageFilter does all the work.
  using ConvolutionFilterType = NeighborhoodOperatorImageFilter<InputImageType, OutputImageType, KernelImagePixelType>;
  auto convolutionFilter = ConvolutionFilterType::New();
  convolutionFilter->SetOperator(kernelOperator);
  convolutionFilter->OverrideBoundaryCondition(this->GetBoundaryCondition());
  convolutionFilter->SetInput(localInput);
  convolutionFilter->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  convolutionFilter->ReleaseDataFlagOn();
  progress->RegisterInternalFilter(convolutionFilter, 1.0f - optionalFilterWeights);

  if (this->GetOutputRegionMode() == ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::SAME)
  {
    // Graft the output of the convolution filter onto this filter's
    // output.
    convolutionFilter->GraftOutput(this->GetOutput());
    convolutionFilter->GetOutput()->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
    convolutionFilter->Update();
    // Set largest possible region to that of input image
    convolutionFilter->GetOutput()->SetLargestPossibleRegion(this->GetInput()->GetLargestPossibleRegion());
    this->GraftOutput(convolutionFilter->GetOutput());
  }
  else // OutputRegionMode == Self::VALID
  {
    using CropFilterType = CropImageFilter<OutputImageType, OutputImageType>;
    using CropFilterPointer = typename CropFilterType::Pointer;
    using CropSizeType = typename CropFilterType::SizeType;

    // Set up the crop sizes.
    const CropSizeType upperCropSize(radius);
    CropSizeType       lowerCropSize(radius);

    convolutionFilter->GraftOutput(this->GetOutput());

    // For the lower crop, the crop size can be reduced by 1 in a
    // dimension when the kernel size is odd in that dimension.
    lowerCropSize -= this->GetKernelPadSize();

    // Set up the crop filter.
    const CropFilterPointer cropFilter = CropFilterType::New();
    cropFilter->SetLowerBoundaryCropSize(lowerCropSize);
    cropFilter->SetUpperBoundaryCropSize(upperCropSize);
    cropFilter->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
    cropFilter->InPlaceOn();
    progress->RegisterInternalFilter(cropFilter, 0.1f);
    cropFilter->SetInput(convolutionFilter->GetOutput());

    // Graft the minipipeline output to this filter.
    cropFilter->GetOutput()->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
    cropFilter->Update();

    // Reset the largest possible region to the valid region
    cropFilter->GetOutput()->SetLargestPossibleRegion(this->GetValidRegion());

    // Graft the output of the crop filter back onto this
    // filter's output.
    this->GraftOutput(cropFilter->GetOutput());
  }
}

template <typename TInputImage, typename TKernelImage, typename TOutputImage>
bool
ConvolutionImageFilter<TInputImage, TKernelImage, TOutputImage>::GetKernelNeedsPadding() const
{
  const KernelImageType * kernel = this->GetKernelImage();
  const InputRegionType   kernelRegion = kernel->GetLargestPossibleRegion();
  InputSizeType           kernelSize = kernelRegion.GetSize();

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    if (kernelSize[i] % 2 == 0) // Check if dimension is even
    {
      return true;
    }
  }

  return false;
}

template <typename TInputImage, typename TKernelImage, typename TOutputImage>
auto
ConvolutionImageFilter<TInputImage, TKernelImage, TOutputImage>::GetKernelPadSize() const -> KernelSizeType
{
  const KernelImageType * kernel = this->GetKernelImage();
  const KernelRegionType  kernelRegion = kernel->GetLargestPossibleRegion();
  KernelSizeType          kernelSize = kernelRegion.GetSize();
  KernelSizeType          padSize;

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    // Pad by 1 if the size of the image in this dimension is even.
    padSize[i] = 1 - (kernelSize[i] % 2);
  }

  return padSize;
}

template <typename TInputImage, typename TKernelImage, typename TOutputImage>
template <typename TImage>
auto
ConvolutionImageFilter<TInputImage, TKernelImage, TOutputImage>::GetKernelRadius(const TImage * kernelImage) const
  -> KernelSizeType
{
  // Compute the kernel radius.
  KernelSizeType radius;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    radius[i] = kernelImage->GetLargestPossibleRegion().GetSize()[i] / 2;
  }

  return radius;
}

template <typename TInputImage, typename TKernelImage, typename TOutputImage>
void
ConvolutionImageFilter<TInputImage, TKernelImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // Pad the input image with the radius of the kernel.
  if (this->GetInput())
  {
    InputRegionType inputRegion = this->GetOutput()->GetRequestedRegion();

    // Pad the output request region by the kernel radius.
    const KernelSizeType radius = this->GetKernelRadius(this->GetKernelImage());
    inputRegion.PadByRadius(radius);

    // Crop the output request region to fit within the largest
    // possible region.
    const typename InputImageType::Pointer inputPtr = const_cast<InputImageType *>(this->GetInput());
    const bool                             cropped = inputRegion.Crop(inputPtr->GetLargestPossibleRegion());
    if (!cropped)
    {
      InvalidRequestedRegionError e(__FILE__, __LINE__);
      e.SetLocation(ITK_LOCATION);
      e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
      e.SetDataObject(inputPtr);
      throw e;
    }

    // Input is an image, cast away the constness so we can set
    // the requested region.
    inputPtr->SetRequestedRegion(inputRegion);
  }

  // Request the largest possible region for the kernel image.
  if (this->GetKernelImage())
  {
    // Input kernel is an image, cast away the constness so we can set
    // the requested region.
    const typename KernelImageType::Pointer kernelPtr = const_cast<KernelImageType *>(this->GetKernelImage());
    kernelPtr->SetRequestedRegionToLargestPossibleRegion();
  }
}
} // namespace itk
#endif
