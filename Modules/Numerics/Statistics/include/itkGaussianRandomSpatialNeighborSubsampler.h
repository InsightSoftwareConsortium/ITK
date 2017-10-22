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
#ifndef itkGaussianRandomSpatialNeighborSubsampler_h
#define itkGaussianRandomSpatialNeighborSubsampler_h

#include "itkUniformRandomSpatialNeighborSubsampler.h"

namespace itk {
namespace Statistics {
/** \class GaussianRandomSpatialNeighborSubsampler
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

template < typename TSample, typename TRegion >
  class ITK_TEMPLATE_EXPORT GaussianRandomSpatialNeighborSubsampler : public UniformRandomSpatialNeighborSubsampler<TSample, TRegion>
{
public:
  /** Standard class typedefs */
  typedef GaussianRandomSpatialNeighborSubsampler<TSample, TRegion>  Self;
  typedef UniformRandomSpatialNeighborSubsampler<TSample, TRegion>   Superclass;
  typedef typename Superclass::Baseclass                             Baseclass;
  typedef SmartPointer<Self>                                         Pointer;
  typedef SmartPointer<const Self>                                   ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(GaussianRandomSpatialNeighborSubsampler,
               UniformRandomSpatialNeighborSubsampler);

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

  typedef typename Superclass::SearchSizeType SearchSizeType;
  typedef typename Superclass::RandomIntType  RandomIntType;
  /** typedefs related to image region */
  typedef typename Superclass::RadiusType      RadiusType;
  typedef typename Superclass::RegionType      RegionType;
  typedef typename Superclass::IndexType       IndexType;
  typedef typename Superclass::SizeType        SizeType;
  typedef typename Superclass::ImageHelperType ImageHelperType;

  typedef double RealType;

  typedef typename Superclass::RandomGeneratorType RandomGeneratorType;
  /** Default sampling variance */
  itkStaticConstMacro(DefaultVariance, int, 900);

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
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

  GaussianRandomSpatialNeighborSubsampler();
  virtual ~GaussianRandomSpatialNeighborSubsampler() ITK_OVERRIDE {};

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

/** method to randomly generate an integer in the closed range
   * [0, upperBound]
   * usign a gaussian selection method. */
  virtual RandomIntType GetIntegerVariate(RandomIntType lowerBound,
                                          RandomIntType upperBound,
                                          RandomIntType mean) ITK_OVERRIDE;

  RealType m_Variance;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianRandomSpatialNeighborSubsampler);

}; // end of class GaussianRandomSpatialNeighborSubsampler

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianRandomSpatialNeighborSubsampler.hxx"
#endif

#endif
