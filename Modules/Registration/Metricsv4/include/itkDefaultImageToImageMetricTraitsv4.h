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
#ifndef itkDefaultImageToImageMetricTraitsv4_h
#define itkDefaultImageToImageMetricTraitsv4_h

#include "itkImage.h"
#include "itkCovariantVector.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkObjectToObjectMetricBase.h"

namespace itk
{
/** \class DefaultImageToImageMetricTraitsv4
 * \brief A simple structure holding type information for ImageToImageMetricv4 classes
 *
 * This class provides default type information for class members and methods
 * used in gradient calculation. This class is used for all images with
 * scalar pixel types. For images with vector pixel types, see
 * itkVectorImageToImageMetricTraitsv4.
 *
 * \sa itkVectorImageToImageMetricTraitsv4
 *
 * \ingroup ITKMetricsv4
 */
template <typename TFixedImageType, typename TMovingImageType, typename TVirtualImageType, typename TCoordRep = double>
class DefaultImageToImageMetricTraitsv4
{
public:
  /** Standard class type aliases. */
  using Self = DefaultImageToImageMetricTraitsv4;

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

  using FixedImageGradientType = CovariantVector<CoordinateRepresentationType, Self::FixedImageDimension>;
  using MovingImageGradientType = CovariantVector<CoordinateRepresentationType, Self::MovingImageDimension>;

  using VirtualImageGradientType = CovariantVector<CoordinateRepresentationType, Self::VirtualImageDimension>;

  using FixedImageComponentGradientType = FixedImageGradientType;
  using MovingImageComponentGradientType = MovingImageGradientType;
  using VirtualImageComponentGradientType = VirtualImageGradientType;

  using FixedImageGradientConvertType = DefaultConvertPixelTraits<FixedImageGradientType>;
  using MovingImageGradientConvertType = DefaultConvertPixelTraits<MovingImageGradientType>;

  /** Type of the filter used to calculate the gradients. */
  using FixedRealType = typename NumericTraits<FixedImagePixelType>::RealType;
  using FixedGradientPixelType = CovariantVector<FixedRealType, Self::FixedImageDimension>;
  using FixedImageGradientImageType = Image<FixedGradientPixelType, Self::FixedImageDimension>;

  using FixedImageGradientFilterType = ImageToImageFilter<FixedImageType, FixedImageGradientImageType>;

  using MovingRealType = typename NumericTraits<MovingImagePixelType>::RealType;
  using MovingGradientPixelType = CovariantVector<MovingRealType, Self::MovingImageDimension>;
  using MovingImageGradientImageType = Image<MovingGradientPixelType, Self::MovingImageDimension>;

  using MovingImageGradientFilterType = ImageToImageFilter<MovingImageType, MovingImageGradientImageType>;

  /** Default image gradient filter types */
  using DefaultFixedImageGradientFilter =
    GradientRecursiveGaussianImageFilter<FixedImageType, FixedImageGradientImageType>;
  using DefaultMovingImageGradientFilter =
    GradientRecursiveGaussianImageFilter<MovingImageType, MovingImageGradientImageType>;

  /** Image gradient calculator types. The TOutput template parameter
   * is chosen to match that of CentralDiffererenceImageFunction. */
  using FixedImageGradientCalculatorType =
    ImageFunction<FixedImageType, CovariantVector<double, Self::FixedImageDimension>, CoordinateRepresentationType>;
  using MovingImageGradientCalculatorType =
    ImageFunction<MovingImageType, CovariantVector<double, Self::MovingImageDimension>, CoordinateRepresentationType>;

  using DefaultFixedImageGradientCalculator =
    CentralDifferenceImageFunction<FixedImageType, CoordinateRepresentationType>;
  using DefaultMovingImageGradientCalculator =
    CentralDifferenceImageFunction<MovingImageType, CoordinateRepresentationType>;

/** Only floating-point images are currently supported. To support integer images,
 * several small changes must be made to use an internal floating-point type for
 * computations rather than the image pixel type itself. */
#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(OnlyDefinedForFloatingPointTypes0, (itk::Concept::IsFloatingPoint<FixedRealType>));
  itkConceptMacro(OnlyDefinedForFloatingPointTypes1, (itk::Concept::IsFloatingPoint<MovingRealType>));
#endif // ITK_USE_CONCEPT_CHECKING
};
} // end namespace itk

//#ifndef ITK_MANUAL_INSTANTIATION
//#include "itkDefaultImageToImageMetricTraitsv4.hxx"
//#endif

#endif
