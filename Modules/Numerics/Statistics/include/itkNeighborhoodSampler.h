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
#ifndef itkNeighborhoodSampler_h
#define itkNeighborhoodSampler_h

#include "itkSampleToSubsampleFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/** \class NeighborhoodSampler
 * \brief Generates a Subsample out of a Sample, based on a user-provided
 * distance to a MeasurementVector.
 *
 * This filter will take as input a Sample and produce as output a Subsample
 * containing the instances of the sample that are at a distance lower than
 * a user-provided threshold from a user-provided measurement vector.
 *
 * \sa Sample, Subsample
 *
 * \sa SampleToSubsampleFilter
 *
 * \ingroup ITKStatistics
 */

template <typename TSample>
class ITK_TEMPLATE_EXPORT NeighborhoodSampler : public SampleToSubsampleFilter<TSample>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NeighborhoodSampler);

  /** Standard class type aliases. */
  using Self = NeighborhoodSampler;
  using Superclass = SampleToSubsampleFilter<TSample>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard macros */
  itkTypeMacro(NeighborhoodSampler, SampleToSubsampleFilter);
  itkNewMacro(Self);

  /** Typedefs for Measurement vector, measurement, Instance Identifier,
   * frequency, size, size element value from the template argument TSample */
  using SampleType = typename Superclass::SampleType;
  using MeasurementVectorType = typename Superclass::MeasurementVectorType;
  using MeasurementType = typename Superclass::MeasurementType;
  using InstanceIdentifier = typename Superclass::InstanceIdentifier;
  using SubsampleType = typename Superclass::SubsampleType;
  using OutputType = typename Superclass::OutputType;

  /** Type of the distance radius. */
  using RadiusType = double;

  /** Type of DataObjects to use for distance radius input. */
  using InputRadiusObjectType = SimpleDataObjectDecorator<RadiusType>;

  /** Method to set the input value of the Radius */
  itkSetGetDecoratedInputMacro(Radius, RadiusType);

protected:
  NeighborhoodSampler() = default;
  ~NeighborhoodSampler() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNeighborhoodSampler.hxx"
#endif

#endif
