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
#ifndef itkGPUUnaryFunctorImageFilter_h
#define itkGPUUnaryFunctorImageFilter_h

#include "itkGPUFunctorBase.h"
#include "itkGPUInPlaceImageFilter.h"
#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
/** \class GPUUnaryFunctorImageFilter
 * \brief Implements pixel-wise generic operation on one image using the GPU.
 *
 * GPU version of unary functor image filter.
 * GPU Functor handles parameter setup for the GPU kernel.
 *
 * \ingroup   ITKGPUCommon
 */
template <typename TInputImage,
          typename TOutputImage,
          typename TFunction,
          typename TParentImageFilter = InPlaceImageFilter<TInputImage, TOutputImage>>
class ITK_TEMPLATE_EXPORT GPUUnaryFunctorImageFilter
  : public GPUInPlaceImageFilter<TInputImage, TOutputImage, TParentImageFilter>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUUnaryFunctorImageFilter);

  /** Standard class type aliases. */
  using Self = GPUUnaryFunctorImageFilter;
  using CPUSuperclass = TParentImageFilter;
  using GPUSuperclass = GPUInPlaceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUUnaryFunctorImageFilter, GPUInPlaceImageFilter);

  /** Some type alias. */
  using FunctorType = TFunction;

  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

#if !defined(ITK_WRAPPING_PARSER)
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }

  const FunctorType &
  GetFunctor() const
  {
    return m_Functor;
  }

  /** Set the functor object. */
  void
  SetFunctor(const FunctorType & functor)
  {
    if (m_Functor != functor)
    {
      m_Functor = functor;
      this->Modified();
    }
  }
#endif // !defined( ITK_WRAPPING_PARSER )

protected:
  GPUUnaryFunctorImageFilter() = default;
  ~GPUUnaryFunctorImageFilter() override = default;

  void
  GenerateOutputInformation() override;

  void
  GPUGenerateData() override;

  /** GPU kernel handle is defined here instead of in the child class
   * because GPUGenerateData() in this base class is used. */
  int m_UnaryFunctorImageFilterGPUKernelHandle;

private:
  FunctorType m_Functor;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUUnaryFunctorImageFilter.hxx"
#endif

#endif
