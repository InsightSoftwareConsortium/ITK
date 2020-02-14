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
#ifndef itkRegionConstrainedSubsampler_h
#define itkRegionConstrainedSubsampler_h

#include "itkSubsamplerBase.h"
#include "itkImageRegion.h"

namespace itk
{
namespace Statistics
{
/**
 *\class RegionConstrainedSubsampler
 * \brief This an abstract subsampler that constrains subsamples
 * to be contained within a given image region.
 *
 * This is an Abstract class that can not be instantiated.
 * There are multiple subsamplers that derive from this class and
 * provide specific implementations of subsampling strategies.
 *
 * This class is templatized over both the Sample and the Region
 * with an assumed consistency between the Region and the way
 * the Sample is generated.
 *
 * Use SetRegionConstraint(region) to provide the region
 * constraint.  All returned subsamples will be contained
 * within this region.  This assumes that the instance
 * identifiers in the sample correspond to an offset
 * computed with the provided region.
 *
 * \sa SubsamplerBase, SpatialNeighborSubsampler
 * \sa GaussianRandomSpatialNeighborSubsampler
 * \sa UniformRandomSpatialNeighborSubsampler
 * \ingroup ITKStatistics
 */

template <typename TSample, typename TRegion>
class ITK_TEMPLATE_EXPORT RegionConstrainedSubsampler : public SubsamplerBase<TSample>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionConstrainedSubsampler);

  /** Standard class type aliases */
  using Self = RegionConstrainedSubsampler<TSample, TRegion>;
  using Superclass = SubsamplerBase<TSample>;
  using Baseclass = typename Superclass::Baseclass;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(RegionConstrainedSubsampler, SubsamplerBase);

  /** type alias alias for the source data container */
  using SampleType = TSample;
  using SampleConstPointer = typename SampleType::ConstPointer;
  using MeasurementVectorType = typename TSample::MeasurementVectorType;
  using InstanceIdentifier = typename TSample::InstanceIdentifier;

  using SubsampleType = Subsample<TSample>;
  using SubsamplePointer = typename SubsampleType::Pointer;
  using SubsampleConstIterator = typename SubsampleType::ConstIterator;
  using InstanceIdentifierHolder = typename SubsampleType::InstanceIdentifierHolder;

  /** type alias related to image region */
  using RegionType = TRegion;
  using IndexType = typename RegionType::IndexType;
  using IndexValueType = typename IndexType::IndexValueType;
  using SizeType = typename RegionType::SizeType;

  /** Method to set the sample domain.
   * This should correspond to the entire region of the input sample. */
  void
  SetSampleRegion(const RegionType & region);

  /** Method to get the sample domain. */
  itkGetConstReferenceMacro(SampleRegion, RegionType);

  /** Method to get the flag indicating that the sample region has been initialized */
  itkGetConstReferenceMacro(SampleRegionInitialized, bool);

  /** Method to set the region constraint.
   * Any subsamples selected must ALSO be inside this region. */
  void
  SetRegionConstraint(const RegionType & region);

  /** Method to get the region constraint. */
  itkGetConstReferenceMacro(RegionConstraint, RegionType);

  /** Method to get the flag indicating that the region constraint has been initialized */
  itkGetConstReferenceMacro(RegionConstraintInitialized, bool);

  /** Main Search method that MUST be implemented by each subclass
   * The Search method will find all points similar to query and return
   * them as a Subsample.  The definition of similar will be subclass-
   * specific.  And could mean spatial similarity or feature similarity
   * etc.  */
  void
  Search(const InstanceIdentifier & query, SubsamplePointer & results) override = 0;

protected:
  /**
   * Clone the current subsampler.
   * This does a complete copy of the subsampler state
   * to the new subsampler
   */
  typename LightObject::Pointer
  InternalClone() const override;

  RegionConstrainedSubsampler();
  ~RegionConstrainedSubsampler() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  RegionType m_RegionConstraint;
  bool       m_RegionConstraintInitialized;
  RegionType m_SampleRegion;
  bool       m_SampleRegionInitialized;
}; // end of class RegionConstrainedSubsampler

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegionConstrainedSubsampler.hxx"
#endif

#endif
