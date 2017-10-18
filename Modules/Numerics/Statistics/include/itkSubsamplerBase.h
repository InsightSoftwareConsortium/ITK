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
#ifndef itkSubsamplerBase_h
#define itkSubsamplerBase_h

#include "itkObject.h"
#include "itkSample.h"
#include "itkSubsample.h"

namespace itk {
namespace Statistics {
/** \class SubsamplerBase
 * \brief This is the base subsampler class which defines the subsampler API.
 *
 * This class will search a Sample provided by SetSample and return a
 * Subsample that are related in some way to the queried value.
 * Some examples of subsampling strategies include uniform random selection,
 * selection based on KdTree, and selection based on spatial proximity.
 *
 * This is an Abstract class that can not be instantiated.
 * There are multiple subsamplers that derive from this class and
 * provide specific implementations of subsampling strategies.
 *
 * \sa RegionConstrainedSubsampler, SpatialNeighborSubsampler
 * \sa GaussianRandomSpatialNeighborSubsampler
 * \sa UniformRandomSpatialNeighborSubsampler
 * \ingroup ITKStatistics
 */

template < typename TSample >
class ITK_TEMPLATE_EXPORT SubsamplerBase : public Object
{
public:
  /** Standard class typedefs */
  typedef SubsamplerBase                        Self;
  typedef Object                                Superclass;
  typedef Self                                  Baseclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(SubsamplerBase, Object);

  /** implement type-specific clone method */
  itkCloneMacro(Self);

  /** typedef alias for the source data container */
  typedef TSample                                          SampleType;
  typedef typename SampleType::ConstPointer                SampleConstPointer;
  typedef typename TSample::MeasurementVectorType          MeasurementVectorType;
  typedef typename TSample::InstanceIdentifier             InstanceIdentifier;

  typedef Subsample<TSample>                               SubsampleType;
  typedef typename SubsampleType::Pointer                  SubsamplePointer;
  typedef typename SubsampleType::ConstIterator            SubsampleConstIterator;
  typedef typename SubsampleType::InstanceIdentifierHolder InstanceIdentifierHolder;
  typedef unsigned int                                     SeedType;

  /** Plug in the actual sample data */
  itkSetConstObjectMacro(Sample, SampleType);
  itkGetConstObjectMacro(Sample, SampleType);

  /** Indicate whether the Search method can return the query point
   * as one element of the Subsample
   */
  itkSetMacro(CanSelectQuery, bool);
  itkGetConstReferenceMacro(CanSelectQuery, bool);
  itkBooleanMacro(CanSelectQuery);

  /** Provide an interface to set the seed.
   *  The seed value will be used by subclasses where appropriate.
   */
  itkSetMacro(Seed, SeedType);
  itkGetConstReferenceMacro(Seed, SeedType);


  /** Specify whether the subsampler should return all possible
   * matches. */
  virtual void RequestMaximumNumberOfResults()
  {
    if (!this->m_RequestMaximumNumberOfResults)
    {
      this->m_RequestMaximumNumberOfResults = true;
      this->Modified();
    }
  }

  /** Main Search method that MUST be implemented by each subclass
   * The Search method will find all points similar to query and return
   * them as a Subsample.  The definition of similar will be subclass-
   * specific.  And could mean spatial similarity or feature similarity
   * etc.  */
  virtual void Search(const InstanceIdentifier& query,
                      SubsamplePointer& results) = 0;

protected:
  /**
   * Clone the current subsampler.
   * This does a complete copy of the subsampler state
   * to the new subsampler
   */
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

  SubsamplerBase();
  virtual ~SubsamplerBase() ITK_OVERRIDE {};

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  SampleConstPointer m_Sample;
  bool               m_RequestMaximumNumberOfResults;
  bool               m_CanSelectQuery;
  SeedType           m_Seed;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SubsamplerBase);

}; // end of class SubsamplerBase

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSubsamplerBase.hxx"
#endif

#endif
