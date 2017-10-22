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
#ifndef itkUniformRandomSpatialNeighborSubsampler_h
#define itkUniformRandomSpatialNeighborSubsampler_h

#include "itkSpatialNeighborSubsampler.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

namespace itk {
namespace Statistics {
/** \class UniformRandomSpatialNeighborSubsampler
 * \brief A subsampler that uniformly randomly selects points
 * within the specified radius of the query point.
 *
 * This class derives from SpatialNeighborSubsampler and
 * randomly selects points according to a uniform distribution
 * within the Radius given by SetRadius(radius)
 * as long as that point is also within the RegionConstraint.
 *
 * This class assumes that the instance identifiers in the input
 * sample correspond to the result of ComputeOffset() of the index
 * of the corresponding point in the image region.
 *
 * \sa SubsamplerBase, RegionConstrainedSubsampler
 * \sa SpatialNeighborSubsampler
 * \sa GaussianRandomSpatialNeighborSubsampler
 * \ingroup ITKStatistics
 */

template < typename TSample, typename TRegion >
  class ITK_TEMPLATE_EXPORT UniformRandomSpatialNeighborSubsampler : public SpatialNeighborSubsampler<TSample, TRegion>
{
public:
  /** Standard class typedefs */
  typedef UniformRandomSpatialNeighborSubsampler<TSample, TRegion>  Self;
  typedef SpatialNeighborSubsampler<TSample, TRegion>               Superclass;
  typedef typename Superclass::Baseclass                            Baseclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(UniformRandomSpatialNeighborSubsampler, SpatialNeighborSubsampler);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** typedef alias for the source data container */
  typedef typename Superclass::SampleType                  SampleType;
  typedef typename Superclass::SampleConstPointer          SampleConstPointer;
  typedef typename Superclass::MeasurementVectorType       MeasurementVectorType;
  typedef typename Superclass::InstanceIdentifier          InstanceIdentifier;

  typedef typename Superclass::SubsampleType            SubsampleType;
  typedef typename Superclass::SubsamplePointer         SubsamplePointer;
  typedef typename Superclass::SubsampleConstIterator   SubsampleConstIterator;
  typedef typename Superclass::InstanceIdentifierHolder InstanceIdentifierHolder;
  typedef typename Baseclass::SeedType                  SeedType;

  typedef unsigned long                       SearchSizeType;
  typedef unsigned int                        RandomIntType;

  /** typedefs related to image region */
  typedef typename Superclass::RadiusType      RadiusType;
  typedef typename Superclass::RegionType      RegionType;
  typedef typename Superclass::IndexType       IndexType;
  typedef typename Superclass::IndexValueType  IndexValueType;
  typedef typename Superclass::SizeType        SizeType;
  typedef typename Superclass::ImageHelperType ImageHelperType;


  /** typedefs related to random variate generator */
  typedef itk::Statistics::MersenneTwisterRandomVariateGenerator RandomGeneratorType;

  virtual void SetSeed(const SeedType seed) ITK_OVERRIDE
  {
    Superclass::SetSeed(seed);
    this->m_RandomNumberGenerator->SetSeed(this->m_Seed);
  }

  virtual void SetUseClockForSeed(const bool& useClock)
  {
    if (useClock != this->m_UseClockForSeed)
    {
      this->m_UseClockForSeed = useClock;
      if (this->m_UseClockForSeed)
      {
        this->m_RandomNumberGenerator->SetSeed();
      }
      this->Modified();
    }
  }

  itkBooleanMacro(UseClockForSeed);
  itkGetConstMacro(UseClockForSeed, bool);

  virtual void SetNumberOfResultsRequested(const SearchSizeType& numberRequested)
  {
    itkDebugMacro("setting NumberOfResultsRequested to " << numberRequested);
    if (this->m_RequestMaximumNumberOfResults ||
        this->m_NumberOfResultsRequested != numberRequested)
    {
      this->m_NumberOfResultsRequested = numberRequested;
      this->m_RequestMaximumNumberOfResults = false;
      this->Modified();
    }
  }
  itkGetConstMacro(NumberOfResultsRequested, SearchSizeType);

  /** Main Search method that MUST be implemented by each subclass
   * The Search method will find all points similar to query and return
   * them as a Subsample.  The definition of similar will be subclass-
   * specific.  And could mean spatial similarity or feature similarity
   * etc.  */
  virtual void Search(const InstanceIdentifier& query,
                      SubsamplePointer& results) ITK_OVERRIDE;

protected:
  /**
   * Clone the current subsampler.
   * This does a complete copy of the subsampler state
   * to the new subsampler
   */
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

  UniformRandomSpatialNeighborSubsampler();
  virtual ~UniformRandomSpatialNeighborSubsampler() ITK_OVERRIDE {};

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /** method to randomly generate an integer in the closed range
   * [lowerBound, upperBound]
   * using a uniform sampling selection method.
   * override this method to do gaussian selection */
  virtual RandomIntType GetIntegerVariate(RandomIntType lowerBound,
                                          RandomIntType upperBound,
                                          RandomIntType itkNotUsed(mean));

  SearchSizeType               m_NumberOfResultsRequested;
  RandomGeneratorType::Pointer m_RandomNumberGenerator;
  bool                         m_UseClockForSeed;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(UniformRandomSpatialNeighborSubsampler);

}; // end of class UniformRandomSpatialNeighborSubsampler

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUniformRandomSpatialNeighborSubsampler.hxx"
#endif

#endif
