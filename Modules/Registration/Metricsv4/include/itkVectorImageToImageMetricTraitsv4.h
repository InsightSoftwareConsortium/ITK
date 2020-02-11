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
#ifndef itkVectorImageToImageMetricTraitsv4_h
#define itkVectorImageToImageMetricTraitsv4_h

#include "itkImage.h"
#include "itkCovariantVector.h"
#include "itkObjectToObjectMetricBase.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkCentralDifferenceImageFunction.h"

namespace itk
{
/** \class VectorImageToImageMetricTraitsv4
 * \brief A simple structure holding type information for ImageToImageMetricv4 classes
 *
 * This class provides type information for class members and methods
 * used in gradient calculation. This class is used for images with
 * vector pixel types, including VectorImage. For images with scalar pixel types, see
 * itkDefaultImageToImageMetricTraitsv4.
 *
 * \sa itkDefaultImageToImageMetricTraitsv4
 *
 * \ingroup ITKMetricsv4
 */
template <typename TFixedImageType,
          typename TMovingImageType,
          typename TVirtualImageType,
          unsigned int NumberOfComponents,
          typename TCoordRep = typename ObjectToObjectMetricBase::CoordinateRepresentationType>
class VectorImageToImageMetricTraitsv4
{
public:
  /** Standard class type aliases. */
  using Self = VectorImageToImageMetricTraitsv4;

  using FixedImageType = TFixedImageType;
  using MovingImageType = TMovingImageType;
  using VirtualImageType = TVirtualImageType;

  using FixedImagePixelType = typename FixedImageType::PixelType;
  using MovingImagePixelType = typename MovingImageType::PixelType;

  using CoordinateRepresentationType = TCoordRep;

  /* Image dimension accessors */
  using ImageDimensionType = unsigned int;
  static constexpr ImageDimensionType FixedImageDimension = FixedImageType::ImageDimension;
  static constexpr ImageDimensionType MovingImageDimension = MovingImageType::ImageDimension;
  static constexpr ImageDimensionType VirtualImageDimension = VirtualImageType::ImageDimension;

  using FixedImageGradientType = Vector<CoordinateRepresentationType, FixedImageDimension * NumberOfComponents>;
  using MovingImageGradientType = Vector<CoordinateRepresentationType, MovingImageDimension * NumberOfComponents>;
  using VirtualImageGradientType = Vector<CoordinateRepresentationType, VirtualImageDimension * NumberOfComponents>;

  using FixedImageGradientConvertType = DefaultConvertPixelTraits<FixedImageGradientType>;
  using MovingImageGradientConvertType = DefaultConvertPixelTraits<MovingImageGradientType>;

  /** Type of the filter used to calculate the gradients. */
  using FixedRealType = typename NumericTraits<FixedImagePixelType>::RealType;
  using MovingRealType = typename NumericTraits<MovingImagePixelType>::RealType;

  using FixedGradientPixelType = FixedImageGradientType;
  using MovingGradientPixelType = MovingImageGradientType;

  using FixedImageGradientImageType = Image<FixedGradientPixelType, Self::FixedImageDimension>;

  using FixedImageGradientFilterType = ImageToImageFilter<FixedImageType, FixedImageGradientImageType>;

  using MovingImageGradientImageType = Image<MovingGradientPixelType, Self::MovingImageDimension>;

  using MovingImageGradientFilterType = ImageToImageFilter<MovingImageType, MovingImageGradientImageType>;

  using FixedImageComponentGradientType = CovariantVector<CoordinateRepresentationType, FixedImageDimension>;
  using MovingImageComponentGradientType = CovariantVector<CoordinateRepresentationType, MovingImageDimension>;
  using VirtualImageComponentGradientType = CovariantVector<CoordinateRepresentationType, VirtualImageDimension>;

  /** Default image gradient filter types */
  using DefaultFixedImageGradientFilter =
    GradientRecursiveGaussianImageFilter<FixedImageType, FixedImageGradientImageType>;
  using DefaultMovingImageGradientFilter =
    GradientRecursiveGaussianImageFilter<MovingImageType, MovingImageGradientImageType>;

  /** Image gradient calculator types. The TOutput template parameter
   * is chosen to match that of CentralDiffererenceImageFunction. */
  using FixedImageGradientCalculatorType =
    ImageFunction<FixedImageType, FixedImageGradientType, CoordinateRepresentationType>;
  using MovingImageGradientCalculatorType =
    ImageFunction<MovingImageType, MovingImageGradientType, CoordinateRepresentationType>;

  using DefaultFixedImageGradientCalculator =
    CentralDifferenceImageFunction<FixedImageType, CoordinateRepresentationType, FixedImageGradientType>;
  using DefaultMovingImageGradientCalculator =
    CentralDifferenceImageFunction<MovingImageType, CoordinateRepresentationType, MovingImageGradientType>;
};
} // end namespace itk

#endif
