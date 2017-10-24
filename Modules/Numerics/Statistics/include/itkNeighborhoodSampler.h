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

template< typename TSample >
class ITK_TEMPLATE_EXPORT NeighborhoodSampler:public SampleToSubsampleFilter< TSample >
{
public:
  /** Standard class typedefs. */
  typedef NeighborhoodSampler                Self;
  typedef SampleToSubsampleFilter< TSample > Superclass;
  typedef SmartPointer< Self >               Pointer;
  typedef SmartPointer< const Self >         ConstPointer;

  /** Standard macros */
  itkTypeMacro(NeighborhoodSampler, SampleToSubsampleFilter);
  itkNewMacro(Self);

  /** Typedefs for Measurement vector, measurement, Instance Identifier,
   * frequency, size, size element value from the template argument TSample */
  typedef typename Superclass::SampleType             SampleType;
  typedef typename Superclass::MeasurementVectorType  MeasurementVectorType;
  typedef typename Superclass::MeasurementType        MeasurementType;
  typedef typename Superclass::InstanceIdentifier     InstanceIdentifier;
  typedef typename Superclass::SubsampleType          SubsampleType;
  typedef typename Superclass::OutputType             OutputType;

  /** Type of the distance radius. */
  typedef double RadiusType;

  /** Type of DataObjects to use for distance radius input. */
  typedef SimpleDataObjectDecorator< RadiusType > InputRadiusObjectType;

  /** Method to set the input value of the Radius */
  itkSetGetDecoratedInputMacro(Radius, RadiusType);

protected:
  NeighborhoodSampler();
  virtual ~NeighborhoodSampler() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NeighborhoodSampler);
};                                   // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodSampler.hxx"
#endif

#endif
