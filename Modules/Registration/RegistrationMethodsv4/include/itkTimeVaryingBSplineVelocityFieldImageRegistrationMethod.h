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
#ifndef itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod_h
#define itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod_h

#include "itkImageRegistrationMethodv4.h"

#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkImageMaskSpatialObject.h"
#include "itkTimeVaryingBSplineVelocityFieldTransform.h"

namespace itk
{

/** \class TimeVaryingBSplineVelocityFieldImageRegistrationMethod
 * \brief Interface method for the current registration framework
 * using the time varying velocity field transform.
 *
 *
 * Output: The output is the updated transform which has been added to the
 * composite transform.
 *
 * This derived class from the SimpleImageRegistrationMethod class
 * is specialized to handle time-varying velocity field transforms
 * of which there are currently 3:
 *
 * -# TimeVaryingDisplacementFieldTransform
 * -# GaussianSmoothingOnUpdateTimeVaryingDisplacementFieldTransform
 * -# BSplineSmoothingOnUpdateTimeVaryingDisplacementFieldTransform
 *
 * The latter is derived from the former and performs an optional
 * spatial and temporal smoothing on the update and total velocity
 * fields. Integration of the velocity field is performed using
 * 4th order Runge Kutta and is performed using the class
 * itkTimeVaryingBSplineVelocityFieldIntegrationImageFilter.
 *
 * Optimization occurs in an iterative fashion where for each
 * sample time point, t, in the velocity field, we integrate
 * the velocity field in the range [0, t] to yield the
 * displacement field which warps fixed image to time point
 * t. Simultaneously, we integrate the velocity field in
 * the range [t, 1] to yield the displacement field transform
 * which warps the moving image to time point t.  The metric
 * derivative for each time point of the velocity field
 * calculated in this way produces the normalized update field
 * (or gradient) of the velocity field to be added to the total
 * field at each iteration after being multiplied by the
 * learning rate and optionally smoothed.  Mathematically,
 * this can be described as
 *
 * \f[
 * v_{total} = B_1( v_{total} + \lambda * B_2( v_{update} ) )
 * \f]
 * where

 * \f$ B_1 = \f$ bspline smoothing on the total field
 * \f$ B_2 = \f$ bspline smoothing on the update field
 * \f$ \lambda = \f$ learning rate
 * \f$ v_{update} = \f$ the normalized velocity field where we
 * normalize the velocity field at each time point
 * separately by the max norm of the field at that time
 * point. This is done due to a weakly necessary
 * (but not sufficient) condition being that the velocity
 * field have a constant norm for all time points.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKRegistrationMethodsv4
 */
template <typename TFixedImage,
          typename TMovingImage,
          typename TOutputTransform = TimeVaryingBSplineVelocityFieldTransform<double, TFixedImage::ImageDimension>,
          typename TVirtualImage = TFixedImage,
          typename TPointSet = PointSet<unsigned int, TFixedImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT TimeVaryingBSplineVelocityFieldImageRegistrationMethod
  : public ImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TimeVaryingBSplineVelocityFieldImageRegistrationMethod);

  /** Standard class type aliases. */
  using Self = TimeVaryingBSplineVelocityFieldImageRegistrationMethod;
  using Superclass = ImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TFixedImage::ImageDimension;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TimeVaryingBSplineVelocityFieldImageRegistrationMethod, SimpleImageRegistrationMethod);

  /** Input type alias for the images and transforms. */
  using FixedImageType = TFixedImage;
  using FixedImagePointer = typename FixedImageType::Pointer;
  using FixedImagesContainerType = typename Superclass::FixedImagesContainerType;
  using MovingImageType = TMovingImage;
  using MovingImagePointer = typename MovingImageType::Pointer;
  using MovingImagesContainerType = typename Superclass::MovingImagesContainerType;

  using InputPointSetType = TPointSet;
  using InputPointSetPointer = typename InputPointSetType::Pointer;
  using PointSetsContainerType = typename Superclass::PointSetsContainerType;

  /** Metric and transform type alias */
  using ImageMetricType = typename Superclass::ImageMetricType;
  using ImageMetricPointer = typename ImageMetricType::Pointer;
  using MeasureType = typename ImageMetricType::MeasureType;
  using MetricDerivativeType = typename ImageMetricType::DerivativeType;

  using VirtualImageType = typename Superclass::VirtualImageType;
  using VirtualImageBaseType = typename Superclass::VirtualImageBaseType;
  using VirtualImageBaseConstPointer = typename Superclass::VirtualImageBaseConstPointer;

  using MetricType = typename Superclass::MetricType;
  using MultiMetricType = typename Superclass::MultiMetricType;
  using MetricPointer = typename MetricType::Pointer;
  using PointSetMetricType = typename Superclass::PointSetMetricType;

  using ImageMaskSpatialObjectType = ImageMaskSpatialObject<ImageDimension>;
  using MaskImageType = typename ImageMaskSpatialObjectType::ImageType;
  using FixedImageMaskType = typename Superclass::FixedImageMaskType;
  using FixedMaskImageType = typename ImageMaskSpatialObjectType::ImageType;
  using FixedImageMasksContainerType = typename Superclass::FixedImageMasksContainerType;
  using MovingImageMaskType = typename Superclass::MovingImageMaskType;
  using MovingMaskImageType = typename ImageMaskSpatialObjectType::ImageType;
  using MovingImageMasksContainerType = typename Superclass::MovingImageMasksContainerType;

  using InitialTransformType = typename Superclass::InitialTransformType;
  using OutputTransformType = TOutputTransform;
  using OutputTransformPointer = typename OutputTransformType::Pointer;
  using RealType = typename OutputTransformType::ScalarType;
  using DerivativeType = typename OutputTransformType::DerivativeType;
  using DerivativeValueType = typename DerivativeType::ValueType;
  using DisplacementFieldType = typename OutputTransformType::DisplacementFieldType;
  using DisplacementFieldPointType = typename DisplacementFieldType::PointType;

  using ContinuousIndexType = ContinuousIndex<typename DisplacementFieldPointType::CoordRepType, ImageDimension>;

  using TimeVaryingVelocityFieldControlPointLatticeType =
    typename OutputTransformType::TimeVaryingVelocityFieldControlPointLatticeType;
  using TimeVaryingVelocityFieldControlPointLatticePointer =
    typename OutputTransformType::TimeVaryingVelocityFieldControlPointLatticePointer;
  using TimeVaryingVelocityFieldType = typename OutputTransformType::TimeVaryingVelocityFieldControlPointLatticeType;
  using TimeVaryingVelocityFieldPointer =
    typename OutputTransformType::TimeVaryingVelocityFieldControlPointLatticePointer;
  using DisplacementVectorType = typename TimeVaryingVelocityFieldControlPointLatticeType::PixelType;

  using CompositeTransformType = typename Superclass::CompositeTransformType;
  using TransformBaseType = typename CompositeTransformType::TransformType;

  using DecoratedOutputTransformType = typename Superclass::DecoratedOutputTransformType;
  using DecoratedOutputTransformPointer = typename DecoratedOutputTransformType::Pointer;

  using NumberOfIterationsArrayType = Array<SizeValueType>;

  using DisplacementFieldTransformType = DisplacementFieldTransform<RealType, ImageDimension>;
  using DisplacementFieldTransformPointer = typename DisplacementFieldTransformType::Pointer;

  using VelocityFieldPointSetType = PointSet<DisplacementVectorType, ImageDimension + 1>;
  using VelocityFieldPointSetPointer = typename VelocityFieldPointSetType::Pointer;
  using BSplineFilterType =
    BSplineScatteredDataPointSetToImageFilter<VelocityFieldPointSetType, TimeVaryingVelocityFieldType>;
  using WeightsContainerType = typename BSplineFilterType::WeightsContainerType;
  using WeightsElementType = typename WeightsContainerType::Element;
  using WeightedMaskImageType = Image<WeightsElementType, ImageDimension>;
  using TimeVaryingWeightedMaskImageType = Image<WeightsElementType, ImageDimension + 1>;

  /** Set/Get the learning rate. */
  itkSetMacro(LearningRate, RealType);
  itkGetConstMacro(LearningRate, RealType);

  /** Set/Get the number of iterations per level. */
  itkSetMacro(NumberOfIterationsPerLevel, NumberOfIterationsArrayType);
  itkGetConstMacro(NumberOfIterationsPerLevel, NumberOfIterationsArrayType);

  /** Set/Get the convergence threshold */
  itkSetMacro(ConvergenceThreshold, RealType);
  itkGetConstMacro(ConvergenceThreshold, RealType);

  /** Set/Get the convergence window size */
  itkSetMacro(ConvergenceWindowSize, unsigned int);
  itkGetConstMacro(ConvergenceWindowSize, unsigned int);

  /** Set/Get the number of time point samples. */
  itkSetMacro(NumberOfTimePointSamples, SizeValueType);
  itkGetConstMacro(NumberOfTimePointSamples, SizeValueType);

protected:
  TimeVaryingBSplineVelocityFieldImageRegistrationMethod();
  ~TimeVaryingBSplineVelocityFieldImageRegistrationMethod() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Perform the registration. */
  void
  GenerateData() override;

  /** Handle optimization internally */
  virtual void
  StartOptimization();

  /** Translate metrics to the point-set for building the (n+1)-D B-spline model */
  void
  GetMetricDerivativePointSetForAllTimePoints(VelocityFieldPointSetType *, WeightsContainerType *);

  void
  AttachMetricGradientPointSetAtSpecificTimePoint(const RealType,
                                                  VelocityFieldPointSetType *,
                                                  WeightsContainerType *,
                                                  const FixedImagesContainerType,
                                                  const PointSetsContainerType,
                                                  const TransformBaseType *,
                                                  const MovingImagesContainerType,
                                                  const PointSetsContainerType,
                                                  const TransformBaseType *,
                                                  const FixedImageMasksContainerType);

private:
  DisplacementFieldTransformPointer m_IdentityDisplacementFieldTransform;

  RealType m_LearningRate;

  RealType     m_ConvergenceThreshold;
  unsigned int m_ConvergenceWindowSize{ 10 };

  NumberOfIterationsArrayType m_NumberOfIterationsPerLevel;

  SizeValueType m_NumberOfTimePointSamples{ 4 };

  WeightsElementType m_BoundaryWeight;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod.hxx"
#endif

#endif
