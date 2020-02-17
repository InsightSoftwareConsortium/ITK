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
#ifndef itkGPUGradientAnisotropicDiffusionImageFilter_h
#define itkGPUGradientAnisotropicDiffusionImageFilter_h

#include "itkOpenCLUtil.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkGPUAnisotropicDiffusionImageFilter.h"
#include "itkGPUGradientNDAnisotropicDiffusionFunction.h"

namespace itk
{
/**
 * \class GPUGradientAnisotropicDiffusionImageFilter
 * This filter performs anisotropic diffusion on a scalar itk::Image using the
 * classic Perona-Malik, gradient magnitude based equation implemented in
 * itkGradientNDAnisotropicDiffusionFunction.  For detailed information on
 * anisotropic diffusion, see itkAnisotropicDiffusionFunction and
 * itkGradientNDAnisotropicDiffusionFunction.
 *
 * \par Inputs and Outputs
 * The input to this filter should be a scalar itk::Image of any
 * dimensionality.  The output image will be a diffused copy of the input.

 * \par Parameters
 * Please see the description of parameters given in
 * itkAnisotropicDiffusionImageFilter.
 *
 * \ingroup ITKGPUAnisotropicSmoothing
 */
template <typename TInputImage,
          typename TOutputImage,
          typename TParentImageFilter = GradientAnisotropicDiffusionImageFilter<TInputImage, TOutputImage>>
class GPUGradientAnisotropicDiffusionImageFilter
  : public GPUAnisotropicDiffusionImageFilter<TInputImage, TOutputImage, TParentImageFilter>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUGradientAnisotropicDiffusionImageFilter);

  /** Standard class type aliases. */
  using Self = GPUGradientAnisotropicDiffusionImageFilter;
  using GPUSuperclass = GPUAnisotropicDiffusionImageFilter<TInputImage, TOutputImage, TParentImageFilter>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard method for creation through object factory. */
  itkNewMacro(Self);

  /** Run-time class information. */
  itkTypeMacro(GPUGradientAnisotropicDiffusionImageFilter, GPUAnisotropicDiffusionImageFilter);

  /** Extract information from the superclass. */
  using UpdateBufferType = typename GPUSuperclass::UpdateBufferType;

  /** Extract information from the superclass. */
  static constexpr unsigned int ImageDimension = GPUSuperclass::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(UpdateBufferHasNumericTraitsCheck, (Concept::HasNumericTraits<typename UpdateBufferType::PixelType>));
  // End concept checking
#endif

protected:
  GPUGradientAnisotropicDiffusionImageFilter()
  {
    // Set DiffusionFunction
    typename GPUGradientNDAnisotropicDiffusionFunction<UpdateBufferType>::Pointer p =
      GPUGradientNDAnisotropicDiffusionFunction<UpdateBufferType>::New();
    this->SetDifferenceFunction(p);
  }

  ~GPUGradientAnisotropicDiffusionImageFilter() override = default;
};

} // namespace itk

#endif
