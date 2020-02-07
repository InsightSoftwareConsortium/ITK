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
#ifndef itkGPUCastImageFilter_h
#define itkGPUCastImageFilter_h

#include "itkCastImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"

#include "itkGPUFunctorBase.h"
#include "itkGPUKernelManager.h"
#include "itkGPUUnaryFunctorImageFilter.h"

namespace itk
{

/** Create a helper GPU Kernel class for GPUCastImageFilter */
itkGPUKernelClassMacro(GPUCastImageFilterKernel);

/** \class GPUCastImageFilter
 * \brief GPU version of CastImageFilter.
 *
 * \author Denis P. Shamonin and Marius Staring. Division of Image Processing,
 * Department of Radiology, Leiden, The Netherlands
 *
 * \note This work was funded by the Netherlands Organisation for
 * Scientific Research (NWO NRG-2010.02 and NWO 639.021.124).
 *
 * \note Imported from elastix. More information can be found in the Insight Journal
 * article: http://hdl.handle.net/10380/3393
 *
 * \ingroup ITKGPUImageFilterBase
 */
namespace Functor
{

template <typename TInput, typename TOutput>
class ITK_TEMPLATE_EXPORT GPUCast : public GPUFunctorBase
{
public:
  GPUCast() {}

  ~GPUCast() {}

  /** Setup GPU kernel arguments for this functor.
   * Returns current argument index to set additional arguments in the GPU kernel.
   */
  int
  SetGPUKernelArguments(GPUKernelManager::Pointer itkNotUsed(kernelManager), int itkNotUsed(kernelHandle))
  {
    return 0;
  }
};

} // end of namespace Functor

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT GPUCastImageFilter
  : public GPUUnaryFunctorImageFilter<
      TInputImage,
      TOutputImage,
      Functor::GPUCast<typename TInputImage::PixelType, typename TOutputImage::PixelType>,
      CastImageFilter<TInputImage, TOutputImage>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUCastImageFilter);

  /** Standard class typedefs. */
  using Self = GPUCastImageFilter;
  using GPUSuperclass =
    GPUUnaryFunctorImageFilter<TInputImage,
                               TOutputImage,
                               Functor::GPUCast<typename TInputImage::PixelType, typename TOutputImage::PixelType>,
                               CastImageFilter<TInputImage, TOutputImage>>;
  using CPUSuperclass = CastImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUCastImageFilter, GPUUnaryFunctorImageFilter);

  /** Pixel types. */
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Type of DataObjects to use for scalar inputs */
  using InputPixelObjectType = SimpleDataObjectDecorator<InputPixelType>;

protected:
  GPUCastImageFilter();
  ~GPUCastImageFilter() override {}

  /** Unlike CPU version, GPU version of binary threshold filter is not
  multi-threaded */
  void
  GPUGenerateData() override;

private:
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUCastImageFilter.hxx"
#endif

#endif /* itkGPUCastImageFilter_h */
