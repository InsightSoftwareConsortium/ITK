/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

namespace itk
{

/** \class RegistrationParameterScalesEstimator
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
template < typename TMetric >
class ITK_TEMPLATE_EXPORT RegistrationParameterScalesEstimator
  : public OptimizerParameterScalesEstimatorTemplate<typename TMetric::ParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesEstimator                                              Self;
  typedef OptimizerParameterScalesEstimatorTemplate<typename TMetric::ParametersValueType>  Superclass;
  typedef SmartPointer<Self>                                                                Pointer;
  typedef SmartPointer<const Self>                                                          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( RegistrationParameterScalesEstimator, Superclass );

  /** Type of scales */
  typedef typename Superclass::ScalesType           ScalesType;
  /** Type of parameters of the optimizer */
  typedef typename Superclass::ParametersType       ParametersType;
  /** Type of float */
  typedef typename Superclass::FloatType            FloatType;

  typedef TMetric                                   MetricType;
  typedef typename MetricType::Pointer              MetricPointer;
  typedef typename MetricType::ConstPointer         MetricConstPointer;

  /** Type of the transform to initialize */
  typedef typename MetricType::FixedTransformType   FixedTransformType;
  typedef typename FixedTransformType::ConstPointer FixedTransformConstPointer;

  typedef typename MetricType::MovingTransformType    MovingTransformType;
  typedef typename MovingTransformType::ConstPointer  MovingTransformConstPointer;

  /** dimension accessors */
  itkStaticConstMacro(FixedDimension, SizeValueType, TMetric::FixedDimension );
  itkStaticConstMacro(MovingDimension, SizeValueType, TMetric::MovingDimension );
  itkStaticConstMacro(VirtualDimension, SizeValueType, TMetric::VirtualDimension );

  typedef typename TMetric::VirtualImageType          VirtualImageType;
  typedef typename TMetric::VirtualImageConstPointer  VirtualImageConstPointer;
  typedef typename TMetric::VirtualImagePointer       VirtualImagePointer;
  typedef typename TMetric::VirtualSpacingType        VirtualSpacingType;
  typedef typename TMetric::VirtualRegionType         VirtualRegionType;
  typedef typename TMetric::VirtualSizeType           VirtualSizeType;
  typedef typename TMetric::VirtualPointType          VirtualPointType;
  typedef typename TMetric::VirtualIndexType          VirtualIndexType;

  typedef typename TMetric::VirtualPointSetType       VirtualPointSetType;
  typedef typename TMetric::VirtualPointSetPointer    VirtualPointSetPointer;

  /** The strategies to sample physical points in the virtual domain. */
  typedef enum { FullDomainSampling = 0,
                 CornerSampling,
                 RandomSampling,
                 CentralRegionSampling,
                 VirtualDomainPointSetSampling }    SamplingStrategyType;

  typedef std::vector<VirtualPointType>             SamplePointContainerType;

  /** Type of Jacobian of transform. */
  typedef typename TMetric::JacobianType            JacobianType;

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
  virtual void EstimateScales(ScalesType &scales) ITK_OVERRIDE = 0;

  /** Estimate the step scale, the impact of a step on deformation. */
  virtual FloatType EstimateStepScale(const ParametersType &step) ITK_OVERRIDE = 0;

  /** Estimate the scales of local steps. */
  virtual void EstimateLocalStepScales(const ParametersType &step, ScalesType &localStepScales) ITK_OVERRIDE = 0;

  /** Estimate the trusted scale for steps. It returns the voxel spacing. */
  virtual FloatType EstimateMaximumStepSize() ITK_OVERRIDE;

  /** Set the sampling strategy automatically for scales estimation. */
  virtual void SetScalesSamplingStrategy();

  /** Set the sampling strategy automatically for step scale estimation. */
  virtual void SetStepScaleSamplingStrategy();

protected:
  RegistrationParameterScalesEstimator();
  ~RegistrationParameterScalesEstimator() ITK_OVERRIDE {};

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Check the metric and the transforms. */
  bool CheckAndSetInputs();

  /** Set and get the number of samples. */
  itkSetMacro(NumberOfRandomSamples, SizeValueType);

  /** Set the sampling strategy. This is called from SetScalesSamplingStrategy() and
   *  SetStepScaleSamplingStrategy(). */
  itkSetMacro(SamplingStrategy, SamplingStrategyType);

  /**
   * Check if the transform is a general affine transform that maps a line
   * segment to a line segment.
   */
  bool CheckGeneralAffineTransform();

  /**
   * The templated version of CheckGeneralAffineTransform to check if the
   * transform is a general affine transform that maps a line segment to
   * a line segment.
   */
  template< typename TTransform > bool CheckGeneralAffineTransformTemplated();

  /** Transform a physical point to a new physical point. */
  template< typename TTargetPointType > void TransformPoint( const VirtualPointType &point, TTargetPointType &mappedPoint);

  /** Transform a point to its continuous index. */
  template< typename TContinuousIndexType > void TransformPointToContinuousIndex( const VirtualPointType &point,TContinuousIndexType &mappedIndex);

  /** Compute the transform Jacobian at a physical point. */
  void ComputeSquaredJacobianNorms( const VirtualPointType  & p, ParametersType & squareNorms);

  /** Check if the transform being optimized has local support. */
  bool TransformHasLocalSupportForScalesEstimation();

  /** Check if the transform being optimized is a displacement field transform. */
  bool IsDisplacementFieldTransform();

  /** Check if the transform being optimized is a B-spline transform. */
  bool IsBSplineTransform();

  /** Get the number of local parameters. */
  SizeValueType GetNumberOfLocalParameters();

  /** Update the transform with a change in parameters. */
  void UpdateTransformParameters(const ParametersType &deltaParameters);

  /** Sample the virtual domain and store the physical points in m_SamplePoints. */
  virtual void SampleVirtualDomain();

  /** Sample the virtual domain with all pixels. */
  void SampleVirtualDomainFully();

  /** Sample the virtual domain with corners. */
  void SampleVirtualDomainWithCorners();

  /** Sample the virtual domain randomly in a uniform distribution. */
  void SampleVirtualDomainRandomly();

  /** Sample the virtual domain with voxel in the central region. */
  void SampleVirtualDomainWithCentralRegion();

  /** Sample the virtual domain with all voxels inside a region. */
  void SampleVirtualDomainWithRegion(VirtualRegionType region);

  /** Sample the virtual domain with a point set */
  void SampleVirtualDomainWithPointSet();

  /** Get the central index of the virtual domain. */
  VirtualIndexType GetVirtualDomainCentralIndex();

  /** Get the central region of the virtual domain. */
  VirtualRegionType GetVirtualDomainCentralRegion();

  /** Get the transform in use. */
  const TransformBaseTemplate<typename TMetric::MeasureType> *GetTransform();

  /** Get the dimension of the target transformed to. */
  SizeValueType GetDimension();

  /** Get the current sampling strategy. Note that this is changed
   * internally as the class is used for scale or step estimation. */
  itkGetMacro( SamplingStrategy, SamplingStrategyType )

  /** the metric object */
  MetricPointer                 m_Metric;

  /** the samples in the virtual domain */
  SamplePointContainerType      m_SamplePoints;

  /** Keep track of the last sampling time. */
  mutable TimeStamp             m_SamplingTime;

  /**  the number of samples in the virtual domain */
  SizeValueType                 m_NumberOfRandomSamples;

  /** the radius of the central region for sampling */
  IndexValueType                m_CentralRegionRadius;

  typename VirtualPointSetType::ConstPointer  m_VirtualDomainPointSet;

  // the threadhold to decide if the number of random samples uses logarithm
  static ITK_CONSTEXPR_VAR SizeValueType    SizeOfSmallDomain = 1000;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegistrationParameterScalesEstimator);

  /** m_TransformForward specifies which transform scales to be estimated.
   * m_TransformForward = true (default) for the moving transform parameters.
   * m_TransformForward = false for the fixed transform parameters.
   */
  bool m_TransformForward;

  // sampling stategy
  SamplingStrategyType          m_SamplingStrategy;

}; //class RegistrationParameterScalesEstimator


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationParameterScalesEstimator.hxx"
#endif

#endif /* itkRegistrationParameterScalesEstimator_h */
