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
#ifndef itkGaussianRandomSpatialNeighborSubsampler_h
#define itkGaussianRandomSpatialNeighborSubsampler_h

#include "itkUniformRandomSpatialNeighborSubsampler.h"

namespace itk
{
namespace Statistics
{
/**
 *\class GaussianRandomSpatialNeighborSubsampler
 * \brief A subsampler that randomly selects points
 * according to a gaussian distribution
 * within the specified radius of the query point.
 *
 * This class derives from UniformRandomNeighborSubsampler and
 * overrides the GetIntegerVariate method to randomly select
 * points according to a gaussian distribution
 * within the Radius given by SetRadius(radius)
 * as long as that point is also within the RegionConstraint.
 * Use SetVariance(variance) to provide the variance of the
 * gaussian distribution.
 *
 * \sa SubsamplerBase, RegionConstrainedSubsampler
 * \sa SpatialNeighborSubsampler
 * \sa UniformRandomSpatialNeighborSubsampler
 * \ingroup ITKStatistics
 */

template <typename TSample, typename TRegion>
class ITK_TEMPLATE_EXPORT GaussianRandomSpatialNeighborSubsampler
  : public UniformRandomSpatialNeighborSubsampler<TSample, TRegion>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianRandomSpatialNeighborSubsampler);

  /** Standard class type aliases */
  using Self = GaussianRandomSpatialNeighborSubsampler<TSample, TRegion>;
  using Superclass = UniformRandomSpatialNeighborSubsampler<TSample, TRegion>;
  using Baseclass = typename Superclass::Baseclass;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(GaussianRandomSpatialNeighborSubsampler, UniformRandomSpatialNeighborSubsampler);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** type alias alias for the source data container */
  using SampleType = typename Superclass::SampleType;
  using SampleConstPointer = typename Superclass::SampleConstPointer;
  using MeasurementVectorType = typename Superclass::MeasurementVectorType;
  using InstanceIdentifier = typename Superclass::InstanceIdentifier;

  using SubsampleType = typename Superclass::SubsampleType;
  using SubsamplePointer = typename Superclass::SubsamplePointer;
  using SubsampleConstIterator = typename Superclass::SubsampleConstIterator;
  using InstanceIdentifierHolder = typename Superclass::InstanceIdentifierHolder;

  using SearchSizeType = typename Superclass::SearchSizeType;
  using RandomIntType = typename Superclass::RandomIntType;
  /** type alias related to image region */
  using RadiusType = typename Superclass::RadiusType;
  using RegionType = typename Superclass::RegionType;
  using IndexType = typename Superclass::IndexType;
  using SizeType = typename Superclass::SizeType;
  using ImageHelperType = typename Superclass::ImageHelperType;

  using RealType = double;

  using RandomGeneratorType = typename Superclass::RandomGeneratorType;
  /** Default sampling variance */
  static constexpr int DefaultVariance = 900;

  /** Set the variance */
  itkSetMacro(Variance, RealType);

  /** Get the variance */
  itkGetConstMacro(Variance, RealType);

protected:
  /**
   * Clone the current subsampler.
   * This does a complete copy of the subsampler state
   * to the new subsampler
   */
  typename LightObject::Pointer
  InternalClone() const override;

  GaussianRandomSpatialNeighborSubsampler();
  ~GaussianRandomSpatialNeighborSubsampler() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** method to randomly generate an integer in the closed range
   * [0, upperBound]
   * usign a gaussian selection method. */
  RandomIntType
  GetIntegerVariate(RandomIntType lowerBound, RandomIntType upperBound, RandomIntType mean) override;

  RealType m_Variance;
}; // end of class GaussianRandomSpatialNeighborSubsampler

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianRandomSpatialNeighborSubsampler.hxx"
#endif

#endif
