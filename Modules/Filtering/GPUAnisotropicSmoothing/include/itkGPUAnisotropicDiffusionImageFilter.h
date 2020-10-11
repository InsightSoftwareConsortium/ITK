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
#ifndef itkGPUAnisotropicDiffusionImageFilter_h
#define itkGPUAnisotropicDiffusionImageFilter_h

#include "itkGPUDenseFiniteDifferenceImageFilter.h"
#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class GPUAnisotropicDiffusionImageFilter
 * This filter is the GPU base class for AnisotropicDiffusionImageFilter.
 * InitializeIteration() calls GPUCalculateAverageGradientMagnitudeSquared().
 *
 * \ingroup ITKGPUAnisotropicSmoothing
 */
template <typename TInputImage,
          typename TOutputImage,
          typename TParentImageFilter = AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>>
class ITK_TEMPLATE_EXPORT GPUAnisotropicDiffusionImageFilter
  : public GPUDenseFiniteDifferenceImageFilter<TInputImage, TOutputImage, TParentImageFilter>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUAnisotropicDiffusionImageFilter);

  /** Standard class type aliases. */
  using Self = GPUAnisotropicDiffusionImageFilter;
  using GPUSuperclass = GPUDenseFiniteDifferenceImageFilter<TInputImage, TOutputImage, TParentImageFilter>;
  using CPUSuperclass = AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information. */
  itkTypeMacro(GPUAnisotropicDiffusionImageFilter, GPUDenseFiniteDifferenceImageFilter);

  /** Capture information from the superclass. */
  using InputImageType = typename GPUSuperclass::InputImageType;
  using OutputImageType = typename GPUSuperclass::OutputImageType;
  using UpdateBufferType = typename GPUSuperclass::UpdateBufferType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  static constexpr unsigned int ImageDimension = GPUSuperclass::ImageDimension;

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  using PixelType = typename GPUSuperclass::PixelType;
  using TimeStepType = typename GPUSuperclass::TimeStepType;

protected:
  GPUAnisotropicDiffusionImageFilter() = default;
  ~GPUAnisotropicDiffusionImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Prepare for the iteration process. */
  void
  InitializeIteration() override;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUAnisotropicDiffusionImageFilter.hxx"
#endif

#endif
