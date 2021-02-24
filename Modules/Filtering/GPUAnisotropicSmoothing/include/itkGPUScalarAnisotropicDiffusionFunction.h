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
#ifndef itkGPUScalarAnisotropicDiffusionFunction_h
#define itkGPUScalarAnisotropicDiffusionFunction_h

#include "itkGPUAnisotropicDiffusionFunction.h"

namespace itk
{
/**
 * \class GPUScalarAnisotropicDiffusionFunction
 *
 * This class forms the base for any GPU anisotropic diffusion function that
 * operates on scalar data (see itkGPUAnisotropicDiffusionFunction).
 *
 * \ingroup ITKGPUAnisotropicSmoothing
 * */

/** Create a helper GPU Kernel class for GPUScalarAnisotropicDiffusionFunction */
itkGPUKernelClassMacro(GPUScalarAnisotropicDiffusionFunctionKernel);

template <typename TImage>
class ITK_TEMPLATE_EXPORT GPUScalarAnisotropicDiffusionFunction : public GPUAnisotropicDiffusionFunction<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUScalarAnisotropicDiffusionFunction);

  /** Standard class type aliases. */
  using Self = GPUScalarAnisotropicDiffusionFunction;
  using Superclass = GPUAnisotropicDiffusionFunction<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(GPUScalarAnisotropicDiffusionFunction, GPUAnisotropicDiffusionFunction);

  /** Inherit some parameters from the superclass type. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Inherit some parameters from the superclass type. */
  using ImageType = typename Superclass::ImageType;
  using PixelType = typename Superclass::PixelType;
  using PixelRealType = typename Superclass::PixelRealType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using TimeStepType = typename Superclass::TimeStepType;

  /** Get OpenCL Kernel source as a string, creates a GetOpenCLSource method */
  itkGetOpenCLSourceFromKernelMacro(GPUScalarAnisotropicDiffusionFunctionKernel);

  /** Compute average squared gradient of magnitude using the GPU */
  void
  GPUCalculateAverageGradientMagnitudeSquared(TImage *) override;

protected:
  GPUScalarAnisotropicDiffusionFunction();
  ~GPUScalarAnisotropicDiffusionFunction() override = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUScalarAnisotropicDiffusionFunction.hxx"
#endif

#endif
