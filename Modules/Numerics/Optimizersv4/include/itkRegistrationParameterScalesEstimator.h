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
#ifndef __itkRegistrationParameterScalesEstimator_h
#define __itkRegistrationParameterScalesEstimator_h

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
 * transform parameters.
 *
 * Its input includes the fixed/moving images and transform objects,
 * which are obtained from the metric object.
 *
 * This class implements some common methods as building blocks called by
 * subclasses with various estimation strategies. One of these methods is
 * SampleImageDomain, which provides various choices of sampling the image
 * domain.
 *
 * \ingroup ITKOptimizersv4
 */
template < class TMetric >
class ITK_EXPORT RegistrationParameterScalesEstimator : public OptimizerParameterScalesEstimator
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesEstimator  Self;
  typedef OptimizerParameterScalesEstimator     Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( RegistrationParameterScalesEstimator, OptimizerParameterScalesEstimator );

  /** Type of scales */
  typedef typename Superclass::ScalesType           ScalesType;
  /** Type of paramters of the optimizer */
  typedef typename Superclass::ParametersType       ParametersType;
  /** Type of float */
  typedef typename Superclass::FloatType            FloatType;

  typedef TMetric                                   MetricType;
  typedef typename MetricType::ConstPointer         MetricConstPointer;

  /** Type of the transform to initialize */
  typedef typename MetricType::FixedTransformType   FixedTransformType;
  typedef typename FixedTransformType::ConstPointer FixedTransformConstPointer;

  typedef typename MetricType::MovingTransformType  MovingTransformType;
  typedef typename MovingTransformType::ConstPointer
                                                    MovingTransformConstPointer;

  /** Image Types to use in the initialization of the transform */
  typedef typename TMetric::FixedImageType          FixedImageType;
  typedef typename TMetric::MovingImageType         MovingImageType;
  typedef typename TMetric::VirtualImageType        VirtualImageType;

  typedef typename FixedImageType::ConstPointer     FixedImageConstPointer;
  typedef typename MovingImageType::ConstPointer    MovingImageConstPointer;
  typedef typename VirtualImageType::ConstPointer   VirtualImageConstPointer;

  /* Image dimension accessors */
  itkStaticConstMacro(FixedImageDimension, SizeValueType,
      ::itk::GetImageDimension<FixedImageType>::ImageDimension);
  itkStaticConstMacro(MovingImageDimension, SizeValueType,
      ::itk::GetImageDimension<MovingImageType>::ImageDimension);
  itkStaticConstMacro(VirtualImageDimension, SizeValueType,
      ::itk::GetImageDimension<VirtualImageType>::ImageDimension);

  typedef typename VirtualImageType::RegionType     VirtualRegionType;
  typedef typename VirtualImageType::SizeType       VirtualSizeType;
  typedef typename VirtualImageType::PointType      VirtualPointType;
  typedef typename VirtualImageType::IndexType      VirtualIndexType;

  typedef typename FixedImageType::PointType        FixedPointType;
  typedef typename FixedImageType::IndexType        FixedIndexType;
  typedef typename FixedImageType::PointValueType   FixedPointValueType;
  typedef typename itk::ContinuousIndex< FixedPointValueType,
          FixedImageType::ImageDimension >          FixedContinuousIndexType;

  typedef typename MovingImageType::PointType       MovingPointType;
  typedef typename MovingImageType::IndexType       MovingIndexType;
  typedef typename MovingImageType::PointValueType  MovingPointValueType;
  typedef typename itk::ContinuousIndex< MovingPointValueType,
          MovingImageType::ImageDimension >         MovingContinuousIndexType;

  /** The strategies to sample physical points in the virtual domain. */
  typedef enum { FullDomainSampling, CornerSampling, RandomSampling,
                 CentralRegionSampling }
          SamplingStrategyType;

  typedef std::vector<VirtualPointType>             ImageSampleContainerType;

  /** Type of Jacobian of transform. */
  typedef typename TMetric::JacobianType            JacobianType;

  /** SetMetric sets the metric used in the estimation process.
   *  The images and transforms from the metric will be used for estimation.
   */
  itkSetObjectMacro(Metric, MetricType);

  /** m_TransformForward specifies which transform scales to be estimated.
   * m_TransformForward = true for the moving transform parameters.
   * m_TransformForward = false for the fixed transform parameters.
   */
  itkSetMacro(TransformForward, bool);
  itkGetConstMacro(TransformForward, bool);

  /** the radius of the central region for sampling. */
  itkSetMacro(CentralRegionRadius, IndexValueType);

  /** Estimate parameter scales */
  virtual void EstimateScales(ScalesType &scales) = 0;

  /** Estimate the step scale, the impact of a step on deformation. */
  virtual FloatType EstimateStepScale(const ParametersType &step) = 0;

  /** Estimate the scales of local steps. */
  virtual void EstimateLocalStepScales(const ParametersType &step,
    ScalesType &localStepScales) = 0;

  /** Estimate the trusted scale for steps. It returns the voxel spacing. */
  virtual FloatType EstimateMaximumStepSize();

  /** Set the sampling strategy automatically for scales estimation. */
  virtual void SetScalesSamplingStrategy();

  /** Set the sampling strategy automatically for step scale estimation. */
  virtual void SetStepScaleSamplingStrategy();

protected:
  RegistrationParameterScalesEstimator();
  ~RegistrationParameterScalesEstimator(){};

  virtual void PrintSelf(std::ostream &os, Indent indent) const;

  /** Check and set the images and transforms from the metric. */
  bool CheckAndSetInputs();

  /** Set and get the number of image samples. */
  itkSetMacro(NumberOfRandomSamples, SizeValueType);

  /** Set the sampling strategy. */
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
  template< class TTransform > bool CheckGeneralAffineTransformTemplated();

  /** Transform a physical point to a new physical point. */
  template< class TTargetPointType > void TransformPoint(
                              const VirtualPointType &point,
                              TTargetPointType &mappedPoint);

  /** Transform a point to its continous index. */
  template< class TContinuousIndexType > void TransformPointToContinuousIndex(
                              const VirtualPointType &point,
                              TContinuousIndexType &mappedIndex);

  /** Compute the transform Jacobian at a physical point. */
  void ComputeSquaredJacobianNorms(
                              const VirtualPointType  & p,
                              ParametersType & squareNorms);

  /** Check if the transform being optimized has local support. */
  bool HasLocalSupport();

  /** Get the number of local parameters. */
  SizeValueType GetNumberOfLocalParameters();

  /** Update the transform with a change in parameters. */
  void UpdateTransformParameters(const ParametersType &deltaParameters);

  /** Sample the virtual image domain and store the physical points in m_ImageSamples. */
  virtual void SampleImageDomain();

  /** Sample the virutal domain with all pixels. */
  void SampleImageDomainFully();

  /** Sample the virutal domain with corners. */
  void SampleImageDomainWithCorners();

  /** Sample the virutal domain randomly in a uniform distribution. */
  void SampleImageDomainRandomly();

  /** Sample the virutal domain with voxel in the central region. */
  void SampleImageDomainWithCentralRegion();

  /** Sample the virtual domain with all voxels inside a region. */
  void SampleImageDomainWithRegion(VirtualRegionType region);

  /** Get the central index of the virtual domain. */
  VirtualIndexType GetVirtualImageCentralIndex();

  /** Get the central region of the virtual domain. */
  VirtualRegionType GetVirtualImageCentralRegion();

  /** Get the transform in use. */
  const TransformBase *GetTransform();

  /** Get the dimension of the target image transformed to. */
  SizeValueType GetImageDimension();

  /** Get the fixed Image. */
  itkGetConstObjectMacro(FixedImage,  FixedImageType);
  /** Get the moving Image. */
  itkGetConstObjectMacro(MovingImage, MovingImageType);
  /** Get the virtual Image. */
  itkGetConstObjectMacro(VirtualImage, VirtualImageType);

  /** Get the fixed transform. */
  itkGetConstObjectMacro(FixedTransform,  FixedTransformType);
  /** Get the moving transform. */
  itkGetConstObjectMacro(MovingTransform, MovingTransformType);

  // the metric object
  MetricConstPointer            m_Metric;

  // the image samples in the virtual image domain
  ImageSampleContainerType      m_ImageSamples;

  /** Keep track of the last sampling time. */
  mutable TimeStamp             m_SamplingTime;

  // the number of image samples in the virtual image domain
  SizeValueType                 m_NumberOfRandomSamples;

  // the radius of the central region for sampling
  IndexValueType                m_CentralRegionRadius;

  // the threadhold to decide if the number of random samples uses logarithm
  static const SizeValueType    SizeOfSmallDomain = 1000;

private:
  RegistrationParameterScalesEstimator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  // the transform objects
  FixedTransformConstPointer         m_FixedTransform;
  MovingTransformConstPointer        m_MovingTransform;

  // the fixed images
  FixedImageConstPointer             m_FixedImage;
  // the moving images
  MovingImageConstPointer            m_MovingImage;
  // the virtual image for symmetric registration
  VirtualImageConstPointer           m_VirtualImage;

  /** m_TransformForward specifies which transform scales to be estimated.
   * m_TransformForward = true for the moving transform parameters.
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

#endif /* __itkRegistrationParameterScalesEstimator_h */
