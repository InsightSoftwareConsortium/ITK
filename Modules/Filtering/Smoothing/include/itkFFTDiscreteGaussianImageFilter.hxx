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
#ifndef itkFFTDiscreteGaussianImageFilter_hxx
#define itkFFTDiscreteGaussianImageFilter_hxx

#include "itkGaussianOperator.h"
#include "itkGaussianImageSource.h"
#include "itkFFTConvolutionImageFilter.h"

#include "itkProgressAccumulator.h"
#include "itkImageAlgorithm.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMacro.h"
#include "itkVariableLengthVector.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
void
FFTDiscreteGaussianImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  ImageToImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion();

  // Get pointer to input
  typename Superclass::InputImagePointer inputPtr = const_cast<TInputImage *>(this->GetInput());
  if (inputPtr.IsNull())
  {
    return;
  }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  RadiusType radius;
  radius.Fill(0);
  for (size_t dim = 0; dim < ImageDimension; ++dim)
  {
    radius[dim] = this->GetKernelRadius(dim);
  }
  inputRequestedRegion.PadByRadius(radius);

  // crop the input requested region at the input's largest possible region
  inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion());

  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

template <typename TInputImage, typename TOutputImage>
void
FFTDiscreteGaussianImageFilter<TInputImage, TOutputImage>::SetInputBoundaryCondition(
  const InputBoundaryConditionPointerType)
{
  itkWarningMacro("FFTDiscreteGaussianImageFilter ignores InputBoundaryCondition, use RealBoundaryCondition instead");
}

template <typename TInputImage, typename TOutputImage>
auto
FFTDiscreteGaussianImageFilter<TInputImage, TOutputImage>::GenerateKernelImage() -> RealImageType *
{
  m_KernelImage = RealImageType::New();
  if (m_KernelSource == FFTDiscreteGaussianImageFilterEnums::KernelSource::OPERATORS)
  {
    // Get directional 1D Gaussian kernels to compose image
    itk::VariableLengthVector<KernelType> directionalOperators;
    directionalOperators.SetSize(this->GetFilterDimensionality());
    RadiusType kernelSize;
    kernelSize.Fill(1);
    for (size_t dim = 0; dim < this->GetFilterDimensionality(); ++dim)
    {
      this->GenerateKernel(dim, directionalOperators[dim]);
      kernelSize[dim] = directionalOperators[dim].GetRadius(dim) * 2 + 1;
    }

    // Set up kernel image
    typename RealImageType::IndexType index;
    index.Fill(0);
    typename RealImageType::RegionType region;
    region.SetSize(kernelSize);
    region.SetIndex(index);
    m_KernelImage->SetRegions(region);
    m_KernelImage->Allocate();
    m_KernelImage->CopyInformation(this->GetInput());

    // Compute kernel image as product of vectors
    itk::ImageRegionIteratorWithIndex<RealImageType> kernelIt(m_KernelImage, region);
    while (!kernelIt.IsAtEnd())
    {
      auto   imageIndex = kernelIt.GetIndex();
      double val = 1;
      for (size_t dim = 0; dim < directionalOperators.GetSize(); ++dim)
      {
        val *= directionalOperators[dim].GetElement(imageIndex[dim]);
      }
      kernelIt.Set(val);

      ++kernelIt;
    }
  }
  else if (m_KernelSource == FFTDiscreteGaussianImageFilterEnums::KernelSource::IMAGE_SOURCE)
  {
    // Create kernel image for blurring in requested dimensions
    using GaussianImageSourceType = GaussianImageSource<RealImageType>;
    using KernelSizeType = typename GaussianImageSourceType::SizeType;
    using KernelMeanType = typename GaussianImageSourceType::ArrayType;

    typename GaussianImageSourceType::Pointer kernelSource = GaussianImageSourceType::New();

    auto inputSpacing = this->GetInput()->GetSpacing();
    auto inputOrigin = this->GetInput()->GetOrigin();

    kernelSource->SetScale(1.0);
    kernelSource->SetNormalized(true);

    kernelSource->SetSpacing(inputSpacing);
    kernelSource->SetOrigin(inputOrigin);
    kernelSource->SetDirection(this->GetInput()->GetDirection());

    KernelSizeType kernelSize;
    kernelSize.Fill(1);
    for (size_t dim = 0; dim < this->GetFilterDimensionality(); ++dim)
    {
      kernelSize[dim] = static_cast<SizeValueType>(this->GetKernelRadius(dim)) * 2 + 1;
    }
    kernelSource->SetSize(kernelSize);

    KernelMeanType mean;
    for (size_t dim = 0; dim < ImageDimension; ++dim)
    {
      double radius = (kernelSize[dim] - 1) / 2;
      mean[dim] = inputSpacing[dim] * radius + inputOrigin[dim]; // center pixel pos
    }
    kernelSource->SetMean(mean);
    kernelSource->SetSigma(this->GetSigmaArray());
    kernelSource->Update();
    m_KernelImage = kernelSource->GetOutput();
    m_KernelImage->DisconnectPipeline();
  }
  else
  {
    itkExceptionMacro("Unknown kernel source enum");
  }

  return m_KernelImage.GetPointer();
}

template <typename TInputImage, typename TOutputImage>
void
FFTDiscreteGaussianImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  TOutputImage * output = this->GetOutput();

  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Create an internal image to protect the input image's metadata
  // (e.g. RequestedRegion). The StreamingImageFilter changes the
  // requested region as part of its normal processing.
  auto localInput = TInputImage::New();
  localInput->Graft(this->GetInput());

  // Determine the dimensionality to filter
  unsigned int filterDimensionality = this->GetFilterDimensionality();
  if (filterDimensionality > ImageDimension)
  {
    filterDimensionality = ImageDimension;
  }
  if (filterDimensionality == 0)
  {
    // no smoothing, copy input to output
    ImageAlgorithm::Copy(localInput.GetPointer(),
                         output,
                         this->GetOutput()->GetRequestedRegion(),
                         this->GetOutput()->GetRequestedRegion());
    return;
  }

  // Create a process accumulator for tracking the progress of minipipeline
  auto progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  RealImageType * kernelImage = GenerateKernelImage();

  // Perform image convolution by FFT

  m_ConvolutionImageFilter->SetInput(this->GetInput());
  m_ConvolutionImageFilter->SetKernelImage(kernelImage);
  m_ConvolutionImageFilter->SetBoundaryCondition(this->GetRealBoundaryCondition());
  m_ConvolutionImageFilter->SetNormalize(false); // Kernel is already normalized

  progress->RegisterInternalFilter(m_ConvolutionImageFilter, 1.0f);

  // Graft this filters output onto the mini-pipeline so the mini-pipeline
  // has the correct region ivars and will write to this filters bulk data
  // output.
  m_ConvolutionImageFilter->GraftOutput(output);

  // Update the last filter in the mini-pipeline
  m_ConvolutionImageFilter->Update();

  // Graft the last output of the mini-pipeline onto this filters output so
  // the final output has the correct region ivars and a handle to the final
  // bulk data
  this->GraftOutput(output);
}
} // namespace itk

#endif
