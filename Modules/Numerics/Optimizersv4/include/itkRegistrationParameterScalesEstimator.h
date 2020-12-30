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
#ifndef itkRegistrationParameterScalesEstimator_h
#define itkRegistrationParameterScalesEstimator_h

#include "itkTransform.h"
#include "itkMatrixOffsetTransformBase.h"
#include "itkTranslationTransform.h"
#include "itkIdentityTransform.h"
#include "itkRigid3DPerspectiveTransform.h"

#include "itkOptimizerParameterScalesEstimator.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "ITKOptimizersv4Export.h"

namespace itk
{
/**\class RegistrationParameterScalesEstimatorEnums
 * \brief This class contains all the enum classes used by RegistrationParameterScalesEstimator class.
 * \ingroup ITKOptimizersv4
 */
class RegistrationParameterScalesEstimatorEnums
{
public:
  /**
   *\class SamplingStrategy
   * \ingroup ITKOptimizersv4
   * The strategies to sample physical points in the virtual domain. */
  enum class SamplingStrategy : uint8_t
  {
    FullDomainSampling = 0,
    CornerSampling,
    RandomSampling,
    CentralRegionSampling,
    VirtualDomainPointSetSampling
  };
};
using SamplingStrategyEnum = RegistrationParameterScalesEstimatorEnums::SamplingStrategy;
// Define how to print enumeration
extern ITKOptimizersv4_EXPORT std::ostream &
                              operator<<(std::ostream & out, const RegistrationParameterScalesEstimatorEnums::SamplingStrategy value);

/**
 *\class RegistrationParameterScalesEstimator
 *  \brief Implements a registration helper class for estimating scales of
 * transform parameters and step sizes.
 *
 * Its input is a metric, from which the fixed/moving images and
 * transform objects are obtained.
 *
 * This class implements some common methods as building blocks called by
 * subclasses with various estimation strategies. One of these methods is
 * SampleVirtualDomain, which provides various choices of sampling the image
 * domain.
 *
 * \note When used with a PointSetToPointSet type metric, a VirtualDomainPointSet
 * must be defined, for use in shift estimation. See SetVirtualDomainPointSet().
 * The virtual domain point set can be retrieved from a metric using the
 * GetVirtualTransformedPointSet() method within the metric.
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TMetric>
class ITK_TEMPLATE_EXPORT RegistrationParameterScalesEstimator
  : public OptimizerParameterScalesEstimatorTemplate<typename TMetric::ParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegistrationParameterScalesEstimator);

  /** Standard class type aliases. */
  using Self = RegistrationParameterScalesEstimator;
  using Superclass = OptimizerParameterScalesEstimatorTemplate<typename TMetric::ParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegistrationParameterScalesEstimator, Superclass);

  /** Type of scales */
  using ScalesType = typename Superclass::ScalesType;
  /** Type of parameters of the optimizer */
  using ParametersType = typename Superclass::ParametersType;
  /** Type of float */
  using FloatType = typename Superclass::FloatType;

  using MetricType = TMetric;
  using MetricPointer = typename MetricType::Pointer;
  using MetricConstPointer = typename MetricType::ConstPointer;

  /** Type of the transform to initialize */
  using FixedTransformType = typename MetricType::FixedTransformType;
  using FixedTransformConstPointer = typename FixedTransformType::ConstPointer;

  using MovingTransformType = typename MetricType::MovingTransformType;
  using MovingTransformConstPointer = typename MovingTransformType::ConstPointer;

  /** dimension accessors */
  static constexpr SizeValueType FixedDimension = TMetric::FixedDimension;
  static constexpr SizeValueType MovingDimension = TMetric::MovingDimension;
  static constexpr SizeValueType VirtualDimension = TMetric::VirtualDimension;

  using VirtualImageType = typename TMetric::VirtualImageType;
  using VirtualImageConstPointer = typename TMetric::VirtualImageConstPointer;
  using VirtualImagePointer = typename TMetric::VirtualImagePointer;
  using VirtualSpacingType = typename TMetric::VirtualSpacingType;
  using VirtualRegionType = typename TMetric::VirtualRegionType;
  using VirtualSizeType = typename TMetric::VirtualSizeType;
  using VirtualPointType = typename TMetric::VirtualPointType;
  using VirtualIndexType = typename TMetric::VirtualIndexType;

  using VirtualPointSetType = typename TMetric::VirtualPointSetType;
  using VirtualPointSetPointer = typename TMetric::VirtualPointSetPointer;

  /** Enables backwards compatibility for enum values */
  using SamplingStrategyType = SamplingStrategyEnum;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr SamplingStrategyType FullDomainSampling = SamplingStrategyType::FullDomainSampling;
  static constexpr SamplingStrategyType CornerSampling = SamplingStrategyType::CornerSampling;
  static constexpr SamplingStrategyType RandomSampling = SamplingStrategyType::RandomSampling;
  static constexpr SamplingStrategyType CentralRegionSampling = SamplingStrategyType::CentralRegionSampling;
  static constexpr SamplingStrategyType VirtualDomainPointSetSampling =
    SamplingStrategyType::VirtualDomainPointSetSampling;
#endif

  using SamplePointContainerType = std::vector<VirtualPointType>;

  /** Type of Jacobian of transform. */
  using JacobianType = typename TMetric::JacobianType;

  /** SetMetric sets the metric used in the estimation process.
   *  The transforms from the metric will be used for estimation, along
   *  with the images when appropriate.
   */
  itkSetObjectMacro(Metric, MetricType);

  /** m_TransformForward specifies which transform scales to be estimated.
   * m_TransformForward = true (default) for the moving transform parameters.
   * m_TransformForward = false for the fixed transform parameters.
   */
  itkSetMacro(TransformForward, bool);
  itkGetConstMacro(TransformForward, bool);

  /** Get/Set a point set for virtual domain sampling. */
  itkSetObjectMacro(VirtualDomainPointSet, VirtualPointSetType);
  itkSetConstObjectMacro(VirtualDomainPointSet, VirtualPointSetType);
  itkGetConstObjectMacro(VirtualDomainPointSet, VirtualPointSetType);

  /** the radius of the central region for sampling. */
  itkSetMacro(CentralRegionRadius, IndexValueType);

  /** Estimate parameter scales */
  void
  EstimateScales(ScalesType & scales) override = 0;

  /** Estimate the step scale, the impact of a step on deformation. */
  FloatType
  EstimateStepScale(const ParametersType & step) override = 0;

  /** Estimate the scales of local steps. */
  void
  EstimateLocalStepScales(const ParametersType & step, ScalesType & localStepScales) override = 0;

  /** Estimate the trusted scale for steps. It returns the voxel spacing. */
  FloatType
  EstimateMaximumStepSize() override;

  /** Set the sampling strategy automatically for scales estimation. */
  virtual void
  SetScalesSamplingStrategy();

  /** Set the sampling strategy automatically for step scale estimation. */
  virtual void
  SetStepScaleSamplingStrategy();

protected:
  RegistrationParameterScalesEstimator();
  ~RegistrationParameterScalesEstimator() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Check the metric and the transforms. */
  bool
  CheckAndSetInputs();

  /** Set and get the number of samples. */
  itkSetMacro(NumberOfRandomSamples, SizeValueType);

  /** Set the sampling strategy. This is called from SetScalesSamplingStrategy() and
   *  SetStepScaleSamplingStrategy(). */
  itkSetMacro(SamplingStrategy, SamplingStrategyType);

  /**
   * Check if the transform is a general affine transform that maps a line
   * segment to a line segment.
   */
  bool
  CheckGeneralAffineTransform();

  /**
   * The templated version of CheckGeneralAffineTransform to check if the
   * transform is a general affine transform that maps a line segment to
   * a line segment.
   */
  template <typename TTransform>
  bool
  CheckGeneralAffineTransformTemplated();

  /** Transform a physical point to a new physical point. */
  template <typename TTargetPointType>
  void
  TransformPoint(const VirtualPointType & point, TTargetPointType & mappedPoint);

  /** Transform a point to its continuous index. */
  template <typename TContinuousIndexType>
  void
  TransformPointToContinuousIndex(const VirtualPointType & point, TContinuousIndexType & mappedIndex);

  /** Compute the transform Jacobian at a physical point. */
  void
  ComputeSquaredJacobianNorms(const VirtualPointType & point, ParametersType & squareNorms);

  /** Check if the transform being optimized has local support. */
  bool
  TransformHasLocalSupportForScalesEstimation();

  /** Check if the transform being optimized is a displacement field transform. */
  bool
  IsDisplacementFieldTransform();

  /** Check if the transform being optimized is a B-spline transform. */
  bool
  IsBSplineTransform();

  /** Get the number of local parameters. */
  SizeValueType
  GetNumberOfLocalParameters();

  /** Update the transform with a change in parameters. */
  void
  UpdateTransformParameters(const ParametersType & deltaParameters);

  /** Sample the virtual domain and store the physical points in m_SamplePoints. */
  virtual void
  SampleVirtualDomain();

  /** Sample the virtual domain with all pixels. */
  void
  SampleVirtualDomainFully();

  /** Sample the virtual domain with corners. */
  void
  SampleVirtualDomainWithCorners();

  /** Sample the virtual domain randomly in a uniform distribution. */
  void
  SampleVirtualDomainRandomly();

  /** Sample the virtual domain with voxel in the central region. */
  void
  SampleVirtualDomainWithCentralRegion();

  /** Sample the virtual domain with all voxels inside a region. */
  void
  SampleVirtualDomainWithRegion(VirtualRegionType region);

  /** Sample the virtual domain with a point set */
  void
  SampleVirtualDomainWithPointSet();

  /** Get the central index of the virtual domain. */
  VirtualIndexType
  GetVirtualDomainCentralIndex();

  /** Get the central region of the virtual domain. */
  VirtualRegionType
  GetVirtualDomainCentralRegion();

  /** Get the transform in use. */
  const TransformBaseTemplate<typename TMetric::MeasureType> *
  GetTransform();

  /** Get the dimension of the target transformed to. */
  SizeValueType
  GetDimension();

  /** Get the current sampling strategy. Note that this is changed
   * internally as the class is used for scale or step estimation. */
  itkGetMacro(SamplingStrategy, SamplingStrategyType);

  /** the metric object */
  MetricPointer m_Metric;

  /** the samples in the virtual domain */
  SamplePointContainerType m_SamplePoints;

  /** Keep track of the last sampling time. */
  mutable TimeStamp m_SamplingTime;

  /**  the number of samples in the virtual domain */
  SizeValueType m_NumberOfRandomSamples;

  /** the radius of the central region for sampling */
  IndexValueType m_CentralRegionRadius;

  typename VirtualPointSetType::ConstPointer m_VirtualDomainPointSet;

  // the threshold to decide if the number of random samples uses logarithm
  static constexpr SizeValueType SizeOfSmallDomain = 1000;

private:
  /** m_TransformForward specifies which transform scales to be estimated.
   * m_TransformForward = true (default) for the moving transform parameters.
   * m_TransformForward = false for the fixed transform parameters.
   */
  bool m_TransformForward;

  // sampling strategy
  SamplingStrategyType m_SamplingStrategy;

}; // class RegistrationParameterScalesEstimator
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegistrationParameterScalesEstimator.hxx"
#endif

#endif /* itkRegistrationParameterScalesEstimator_h */
