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
#ifndef itkPointSetToImageMetric_h
#define itkPointSetToImageMetric_h

#include "itkImageBase.h"
#include "itkTransform.h"
#include "itkInterpolateImageFunction.h"
#include "itkSingleValuedCostFunction.h"
#include "itkMacro.h"
#include "itkGradientRecursiveGaussianImageFilter.h"

namespace itk
{
/** \class PointSetToImageMetric
 * \brief Computes similarity between a point set and an image.
 *
 * This Class is templated over the type of the input point-set and image.  It
 * expects a Transform and an Interpolator to be plugged in.  This particular
 * class is the base class for a hierarchy of similarity metrics.
 *
 * This class computes a value that measures the similarity between the values
 * associated with points in the point set and the transformed Moving image.
 * The Interpolator is used to compute intensity values on non-grid positions
 * resulting from mapping points through the Transform.
 *
 * \ingroup RegistrationMetrics
 *
 * \ingroup ITKRegistrationCommon
 */

template <typename TFixedPointSet, typename TMovingImage>
class ITK_TEMPLATE_EXPORT PointSetToImageMetric : public SingleValuedCostFunction
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointSetToImageMetric);

  /** Standard class type aliases. */
  using Self = PointSetToImageMetric;
  using Superclass = SingleValuedCostFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Type used for representing point components  */
  using CoordinateRepresentationType = Superclass::ParametersValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToImageMetric, SingleValuedCostFunction);

  /**  Type of the moving Image. */
  using MovingImageType = TMovingImage;
  using MovingImagePixelType = typename TMovingImage::PixelType;
  using MovingImageConstPointer = typename MovingImageType::ConstPointer;

  /**  Type of the fixed Image. */
  using FixedPointSetType = TFixedPointSet;
  using FixedPointSetConstPointer = typename FixedPointSetType::ConstPointer;

  /** Constants for the image dimensions */
  static constexpr unsigned int MovingImageDimension = TMovingImage::ImageDimension;
  static constexpr unsigned int FixedPointSetDimension = TFixedPointSet::PointDimension;

  using PointIterator = typename FixedPointSetType::PointsContainer::ConstIterator;
  using PointDataIterator = typename FixedPointSetType::PointDataContainer::ConstIterator;

  /**  Type of the Transform Base class */
  using TransformType =
    Transform<CoordinateRepresentationType, Self::MovingImageDimension, Self::FixedPointSetDimension>;

  using TransformPointer = typename TransformType::Pointer;
  using InputPointType = typename TransformType::InputPointType;
  using OutputPointType = typename TransformType::OutputPointType;
  using TransformParametersType = typename TransformType::ParametersType;
  using TransformJacobianType = typename TransformType::JacobianType;

  /**  Type of the Interpolator Base class */
  using InterpolatorType = InterpolateImageFunction<MovingImageType, CoordinateRepresentationType>;

  /** Gaussian filter to compute the gradient of the Moving Image */
  using RealType = typename NumericTraits<MovingImagePixelType>::RealType;
  using GradientPixelType = CovariantVector<RealType, Self::MovingImageDimension>;
  using GradientImageType = Image<GradientPixelType, Self::MovingImageDimension>;
  using GradientImagePointer = SmartPointer<GradientImageType>;
  using GradientImageFilterType = GradientRecursiveGaussianImageFilter<MovingImageType, GradientImageType>;

  using GradientImageFilterPointer = typename GradientImageFilterType::Pointer;

  using InterpolatorPointer = typename InterpolatorType::Pointer;

  /**  Type of the measure. */
  using MeasureType = Superclass::MeasureType;

  /**  Type of the derivative. */
  using DerivativeType = Superclass::DerivativeType;

  /**  Type of the parameters. */
  using ParametersType = Superclass::ParametersType;

  /** Get/Set the Fixed Image.  */
  itkSetConstObjectMacro(FixedPointSet, FixedPointSetType);
  itkGetConstObjectMacro(FixedPointSet, FixedPointSetType);

  /** Get/Set the Moving Image.  */
  itkSetConstObjectMacro(MovingImage, MovingImageType);
  itkGetConstObjectMacro(MovingImage, MovingImageType);

  /** Connect the Transform. */
  itkSetObjectMacro(Transform, TransformType);

  /** Get a pointer to the Transform.  */
  itkGetModifiableObjectMacro(Transform, TransformType);

  /** Connect the Interpolator. */
  itkSetObjectMacro(Interpolator, InterpolatorType);

  /** Get a pointer to the Interpolator.  */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Get Gradient Image. */
  itkGetModifiableObjectMacro(GradientImage, GradientImageType);

  /** Get the number of pixels considered in the computation. */
  itkGetConstReferenceMacro(NumberOfPixelsCounted, SizeValueType);

  /** Set the parameters defining the Transform. */
  void
  SetTransformParameters(const ParametersType & parameters) const;

  /** Set/Get the flag for computing the image gradient.
   *  When ON the metric derivative is computed using the Jacobian of the
   *  transformation and the image gradient. When OFF the metric derivative
   *  is computed by finite differences. Mode ON results in higher speed
   *  at the price of large memory footprint. Mode OFF results in small
   *  memory footprint at the price of large computation time
   */
  itkSetMacro(ComputeGradient, bool);
  itkGetConstReferenceMacro(ComputeGradient, bool);

  /** Return the number of parameters required by the Transform */
  unsigned int
  GetNumberOfParameters() const override
  {
    return m_Transform->GetNumberOfParameters();
  }

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  virtual void
  Initialize();

protected:
  PointSetToImageMetric();
  ~PointSetToImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  mutable SizeValueType m_NumberOfPixelsCounted;

  FixedPointSetConstPointer m_FixedPointSet;

  MovingImageConstPointer m_MovingImage;

  mutable TransformPointer m_Transform;

  InterpolatorPointer m_Interpolator;

  bool m_ComputeGradient;

  GradientImagePointer m_GradientImage;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointSetToImageMetric.hxx"
#endif

#endif
