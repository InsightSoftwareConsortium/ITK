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
#ifndef itkMeanSquaresImageToImageMetricv4_h
#define itkMeanSquaresImageToImageMetricv4_h

#include "itkImageToImageMetricv4.h"
#include "itkMeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader.h"
#include "itkDefaultImageToImageMetricTraitsv4.h"

namespace itk
{

/** \class MeanSquaresImageToImageMetricv4
 *
 *  \brief Class implementing a mean squares metric.
 *
 *  This class supports vector images of type VectorImage
 *  and Image< VectorType, imageDimension >.
 *
 *  See
 *  MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader::ProcessPoint for algorithm implementation.
 *
 * \ingroup ITKMetricsv4
 */
template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage = TFixedImage,
          typename TInternalComputationValueType = double,
          typename TMetricTraits =
            DefaultImageToImageMetricTraitsv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType>>
class ITK_TEMPLATE_EXPORT MeanSquaresImageToImageMetricv4
  : public ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MeanSquaresImageToImageMetricv4);

  /** Standard class type aliases. */
  using Self = MeanSquaresImageToImageMetricv4;
  using Superclass =
    ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeanSquaresImageToImageMetricv4, ImageToImageMetricv4);

  using DerivativeType = typename Superclass::DerivativeType;

  using FixedImagePointType = typename Superclass::FixedImagePointType;
  using FixedImagePixelType = typename Superclass::FixedImagePixelType;
  using FixedImageGradientType = typename Superclass::FixedImageGradientType;

  using MovingImagePointType = typename Superclass::MovingImagePointType;
  using MovingImagePixelType = typename Superclass::MovingImagePixelType;
  using MovingImageGradientType = typename Superclass::MovingImageGradientType;

  using MovingTransformType = typename Superclass::MovingTransformType;
  using JacobianType = typename Superclass::JacobianType;
  using VirtualImageType = typename Superclass::VirtualImageType;
  using VirtualIndexType = typename Superclass::VirtualIndexType;
  using VirtualPointType = typename Superclass::VirtualPointType;
  using VirtualPointSetType = typename Superclass::VirtualPointSetType;

  /* Image dimension accessors */
  static constexpr typename TVirtualImage::ImageDimensionType VirtualImageDimension = TVirtualImage::ImageDimension;
  static constexpr typename TFixedImage::ImageDimensionType   FixedImageDimension = TFixedImage::ImageDimension;
  static constexpr typename TMovingImage::ImageDimensionType  MovingImageDimension = TMovingImage::ImageDimension;

protected:
  MeanSquaresImageToImageMetricv4();
  ~MeanSquaresImageToImageMetricv4() override = default;

  friend class MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader<
    ThreadedImageRegionPartitioner<Superclass::VirtualImageDimension>,
    Superclass,
    Self>;
  friend class MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader<ThreadedIndexedContainerPartitioner,
                                                                            Superclass,
                                                                            Self>;
  using MeanSquaresDenseGetValueAndDerivativeThreaderType =
    MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader<
      ThreadedImageRegionPartitioner<Superclass::VirtualImageDimension>,
      Superclass,
      Self>;
  using MeanSquaresSparseGetValueAndDerivativeThreaderType =
    MeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader<ThreadedIndexedContainerPartitioner, Superclass, Self>;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMeanSquaresImageToImageMetricv4.hxx"
#endif

#endif
