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
#ifndef itkImageToImageMetricv4GetValueAndDerivativeThreader_h
#define itkImageToImageMetricv4GetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreaderBase.h"
#include "itkNumericTraits.h"
#include "itkThreadedImageRegionPartitioner.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{

/** \class ImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Provides threading for ImageToImageMetricv4::GetValueAndDerivative.
 *
 * \tparam TDomainPartitioner type of the Domain,
 * ThreadedImageRegionPartitioner or ThreadedIndexedContainerPartitioner
 * \tparam TImageToImageMetricv4 type of the ImageToImageMetricv4
 *
 * This class implements ThreadedExecution.  Template specialization is
 * provided for ThreadedImageRegionPartitioner and
 * ThreadedIndexedContainerPartitioner.
 *
 * \sa ImageToImageMetricv4GetValueAndDerivativeThreaderBase
 * \ingroup ITKMetricsv4
 * */
template <typename TDomainPartitioner, typename TImageToImageMetricv4>
class ITK_TEMPLATE_EXPORT ImageToImageMetricv4GetValueAndDerivativeThreader
{};

/** \class ImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Specialization for ThreadedImageRegionPartitioner.
 * \ingroup ITKMetricsv4
 * */
template <typename TImageToImageMetricv4>
class ITK_TEMPLATE_EXPORT ImageToImageMetricv4GetValueAndDerivativeThreader<
  ThreadedImageRegionPartitioner<TImageToImageMetricv4::VirtualImageDimension>,
  TImageToImageMetricv4>
  : public ImageToImageMetricv4GetValueAndDerivativeThreaderBase<
      ThreadedImageRegionPartitioner<TImageToImageMetricv4::VirtualImageDimension>,
      TImageToImageMetricv4>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageToImageMetricv4GetValueAndDerivativeThreader);

  /** Standard class type aliases. */
  using Self = ImageToImageMetricv4GetValueAndDerivativeThreader;
  using Superclass = ImageToImageMetricv4GetValueAndDerivativeThreaderBase<
    ThreadedImageRegionPartitioner<TImageToImageMetricv4::VirtualImageDimension>,
    TImageToImageMetricv4>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(ImageToImageMetricv4GetValueAndDerivativeThreader,
               ImageToImageMetricv4GetValueAndDerivativeThreaderBase);

  /** Superclass types. */
  using typename Superclass::DomainType;
  using typename Superclass::AssociateType;

  /** Types of the target class. */
  using ImageToImageMetricv4Type = TImageToImageMetricv4;
  using VirtualImageType = typename Superclass::VirtualImageType;
  using typename Superclass::VirtualIndexType;
  using typename Superclass::VirtualPointType;
  using typename Superclass::FixedImagePointType;
  using typename Superclass::FixedImagePixelType;
  using typename Superclass::FixedImageGradientType;
  using typename Superclass::MovingImagePointType;
  using typename Superclass::MovingImagePixelType;
  using typename Superclass::MovingImageGradientType;

  using typename Superclass::FixedTransformType;
  using typename Superclass::FixedOutputPointType;
  using typename Superclass::MovingTransformType;
  using typename Superclass::MovingOutputPointType;

  using typename Superclass::MeasureType;
  using typename Superclass::DerivativeType;
  using typename Superclass::DerivativeValueType;
  using typename Superclass::JacobianType;

  using typename Superclass::InternalComputationValueType;
  using typename Superclass::NumberOfParametersType;
  using typename Superclass::ImageDimensionType;

protected:
  /** Constructor. */
  ImageToImageMetricv4GetValueAndDerivativeThreader() = default;

  /** Walk through the given virtual image domain, and call \c ProcessVirtualPoint on every
   * point. */
  void
  ThreadedExecution(const DomainType & imageSubRegion, const ThreadIdType threadId) override;

  /** Get cached values for efficiency. Only valid once threading has started.
   *  These methods should be used in tight loops (inlining helps measurably).
   *  Put these methods here so derived threaders can access them directly. */
  inline NumberOfParametersType
  GetCachedNumberOfParameters() const
  {
    return this->m_CachedNumberOfParameters;
  }
  inline NumberOfParametersType
  GetCachedNumberOfLocalParameters() const
  {
    return this->m_CachedNumberOfLocalParameters;
  }
};

/** \class ImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Specialization for ThreadedIndexedContainerPartitioner.
 * \ingroup ITKMetricsv4
 * */
template <typename TImageToImageMetricv4>
class ITK_TEMPLATE_EXPORT
  ImageToImageMetricv4GetValueAndDerivativeThreader<ThreadedIndexedContainerPartitioner, TImageToImageMetricv4>
  : public ImageToImageMetricv4GetValueAndDerivativeThreaderBase<ThreadedIndexedContainerPartitioner,
                                                                 TImageToImageMetricv4>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageToImageMetricv4GetValueAndDerivativeThreader);

  /** Standard class type aliases. */
  using Self = ImageToImageMetricv4GetValueAndDerivativeThreader;
  using Superclass =
    ImageToImageMetricv4GetValueAndDerivativeThreaderBase<ThreadedIndexedContainerPartitioner, TImageToImageMetricv4>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(ImageToImageMetricv4GetValueAndDerivativeThreader,
               ImageToImageMetricv4GetValueAndDerivativeThreaderBase);

  /** Superclass types. */
  using typename Superclass::DomainType;
  using typename Superclass::AssociateType;

  /** Types of the target class. */
  using ImageToImageMetricv4Type = typename Superclass::ImageToImageMetricv4Type;
  using VirtualImageType = typename Superclass::VirtualImageType;
  using typename Superclass::VirtualIndexType;
  using typename Superclass::VirtualPointType;
  using typename Superclass::FixedImagePointType;
  using typename Superclass::FixedImagePixelType;
  using typename Superclass::FixedImageGradientType;
  using typename Superclass::MovingImagePointType;
  using typename Superclass::MovingImagePixelType;
  using typename Superclass::MovingImageGradientType;

  using typename Superclass::FixedTransformType;
  using typename Superclass::FixedOutputPointType;
  using typename Superclass::MovingTransformType;
  using typename Superclass::MovingOutputPointType;

  using typename Superclass::MeasureType;
  using typename Superclass::DerivativeType;
  using typename Superclass::DerivativeValueType;
  using typename Superclass::JacobianType;

  using typename Superclass::InternalComputationValueType;
  using typename Superclass::NumberOfParametersType;

protected:
  /** Constructor. */
  ImageToImageMetricv4GetValueAndDerivativeThreader() = default;

  /** Walk through the given virtual image domain, and call \c ProcessVirtualPoint on every
   * point. */
  void
  ThreadedExecution(const DomainType & indexSubRange, const ThreadIdType threadId) override;

  /** Get cached values for efficiency. Only valid once threading has started.
   *  These methods should be used in tight loops (inlining helps measurably).
   *  Put these methods here so derived threaders can access them directly. */
  inline NumberOfParametersType
  GetCachedNumberOfParameters() const
  {
    return this->m_CachedNumberOfParameters;
  }
  inline NumberOfParametersType
  GetCachedNumberOfLocalParameters() const
  {
    return this->m_CachedNumberOfLocalParameters;
  }
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToImageMetricv4GetValueAndDerivativeThreader.hxx"
#endif

#endif
