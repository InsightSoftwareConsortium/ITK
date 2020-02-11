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
#ifndef itkBSplineSyNImageRegistrationMethod_h
#define itkBSplineSyNImageRegistrationMethod_h

#include "itkSyNImageRegistrationMethod.h"

#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransform.h"

namespace itk
{

/** \class BSplineSyNImageRegistrationMethod
 * \brief Interface method for the performing greedy B-spline SyN image registration.
 *
 * For greedy SyN we use \c m_Transform to map the time-parameterized middle
 * image to the fixed image (and vice versa using
 * \c m_Transform->GetInverseDisplacementField() ).  We employ another ivar,
 * \c m_InverseTransform, to map the time-parameterized middle image to the
 * moving image.
 *
 * Output: The output is the updated transform which has been added to the
 * composite transform.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKRegistrationMethodsv4
 */
template <typename TFixedImage,
          typename TMovingImage,
          typename TOutputTransform =
            BSplineSmoothingOnUpdateDisplacementFieldTransform<double, TFixedImage::ImageDimension>,
          typename TVirtualImage = TFixedImage,
          typename TPointSet = PointSet<unsigned int, TFixedImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT BSplineSyNImageRegistrationMethod
  : public SyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineSyNImageRegistrationMethod);

  /** Standard class type aliases. */
  using Self = BSplineSyNImageRegistrationMethod;
  using Superclass = SyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TFixedImage::ImageDimension;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineSyNImageRegistrationMethod, SyNImageRegistrationMethod);

  /** Input type alias for the images. */
  using FixedImageType = TFixedImage;
  using FixedImagePointer = typename FixedImageType::Pointer;
  using FixedImagesContainerType = typename Superclass::FixedImagesContainerType;
  using MovingImageType = TMovingImage;
  using MovingImagePointer = typename MovingImageType::Pointer;
  using MovingImagesContainerType = typename Superclass::MovingImagesContainerType;

  using PointSetType = typename Superclass::PointSetType;
  using PointSetsContainerType = typename Superclass::PointSetsContainerType;

  /** Metric and transform type alias */
  using ImageMetricType = typename Superclass::ImageMetricType;
  using ImageMetricPointer = typename ImageMetricType::Pointer;
  using MeasureType = typename ImageMetricType::MeasureType;

  using ImageMaskSpatialObjectType = typename Superclass::ImageMaskSpatialObjectType;
  using MaskImageType = typename ImageMaskSpatialObjectType::ImageType;
  using FixedImageMaskType = typename Superclass::FixedImageMaskType;
  using FixedMaskImageType = typename ImageMaskSpatialObjectType::ImageType;
  using FixedImageMasksContainerType = typename Superclass::FixedImageMasksContainerType;
  using MovingImageMaskType = typename Superclass::MovingImageMaskType;
  using MovingMaskImageType = typename ImageMaskSpatialObjectType::ImageType;
  using MovingImageMasksContainerType = typename Superclass::MovingImageMasksContainerType;

  using VirtualImageType = typename Superclass::VirtualImageType;
  using VirtualImageBaseType = typename Superclass::VirtualImageBaseType;
  using VirtualImageBaseConstPointer = typename Superclass::VirtualImageBaseConstPointer;

  using MultiMetricType = typename Superclass::MultiMetricType;
  using MetricType = typename Superclass::MetricType;
  using MetricPointer = typename MetricType::Pointer;
  using PointSetMetricType = typename Superclass::PointSetMetricType;

  using NumberOfIterationsArrayType = typename Superclass::NumberOfIterationsArrayType;

  using InitialTransformType = typename Superclass::InitialTransformType;
  using OutputTransformType = TOutputTransform;
  using OutputTransformPointer = typename OutputTransformType::Pointer;
  using RealType = typename OutputTransformType::ScalarType;
  using DerivativeType = typename OutputTransformType::DerivativeType;
  using DerivativeValueType = typename DerivativeType::ValueType;
  using DisplacementFieldType = typename OutputTransformType::DisplacementFieldType;
  using DisplacementFieldPointer = typename DisplacementFieldType::Pointer;
  using DisplacementVectorType = typename DisplacementFieldType::PixelType;

  using BSplineFilterType = DisplacementFieldToBSplineImageFilter<DisplacementFieldType>;
  using ArrayType = typename BSplineFilterType::ArrayType;
  using WeightedMaskImageType = typename BSplineFilterType::RealImageType;
  using BSplinePointSetType = typename BSplineFilterType::InputPointSetType;

  using CompositeTransformType = typename Superclass::CompositeTransformType;
  using DisplacementFieldTransformType = typename Superclass::DisplacementFieldTransformType;
  using DisplacementFieldTransformPointer = typename Superclass::DisplacementFieldTransformPointer;
  using TransformBaseType = typename CompositeTransformType::TransformType;

  using DecoratedOutputTransformType = typename Superclass::DecoratedOutputTransformType;
  using DecoratedOutputTransformPointer = typename DecoratedOutputTransformType::Pointer;

protected:
  BSplineSyNImageRegistrationMethod() = default;
  ~BSplineSyNImageRegistrationMethod() override = default;

  /** Handle optimization internally */
  void
  StartOptimization() override;

  void
  InitializeRegistrationAtEachLevel(const SizeValueType) override;

  DisplacementFieldPointer
  ComputeUpdateField(const FixedImagesContainerType,
                     const PointSetsContainerType,
                     const TransformBaseType *,
                     const MovingImagesContainerType,
                     const PointSetsContainerType,
                     const TransformBaseType *,
                     const FixedImageMasksContainerType,
                     const MovingImageMasksContainerType,
                     MeasureType &) override;
  virtual DisplacementFieldPointer
  BSplineSmoothDisplacementField(const DisplacementFieldType *,
                                 const ArrayType &,
                                 const WeightedMaskImageType *,
                                 const BSplinePointSetType *);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineSyNImageRegistrationMethod.hxx"
#endif

#endif
