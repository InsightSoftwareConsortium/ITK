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
#ifndef itkRegistrationParameterScalesFromPhysicalShift_hxx
#define itkRegistrationParameterScalesFromPhysicalShift_hxx

#include "itkRegistrationParameterScalesFromPhysicalShift.h"

namespace itk
{

template< typename TMetric >
RegistrationParameterScalesFromPhysicalShift< TMetric >
::RegistrationParameterScalesFromPhysicalShift()
{
}

template< typename TMetric >
void
RegistrationParameterScalesFromPhysicalShift< TMetric >
::ComputeSampleShifts(const ParametersType &deltaParameters, ScalesType &sampleShifts)
{
  if (this->GetTransformForward())
    {
    this->ComputeSampleShiftsInternal<MovingTransformType>(deltaParameters, sampleShifts);
    }
  else
    {
    this->ComputeSampleShiftsInternal<FixedTransformType>(deltaParameters, sampleShifts);
    }
}

template< typename TMetric >
template< typename TTransform >
void
RegistrationParameterScalesFromPhysicalShift< TMetric >
::ComputeSampleShiftsInternal(const ParametersType &deltaParameters, ScalesType &sampleShifts)
{
  typedef typename TTransform::OutputPointType TransformOutputType;

  // We save the old parameters and apply the delta parameters to calculate the
  // voxel shift. After it is done, we will reset to the old parameters.
  TransformBaseTemplate<typename TMetric::MeasureType> *transform = const_cast<TransformBaseTemplate<typename TMetric::MeasureType> *>(this->GetTransform());
  const ParametersType oldParameters = transform->GetParameters();

  const SizeValueType numSamples = static_cast<const SizeValueType>( this->m_SamplePoints.size() );

  VirtualPointType point;
  TransformOutputType newMappedVoxel;

  // store the old mapped indices to reduce calls to Transform::SetParameters()
  std::vector<TransformOutputType> oldMappedVoxels(numSamples);
  sampleShifts.SetSize(numSamples);

  // compute the indices mapped by the old transform
  for (SizeValueType c=0; c<numSamples; c++)
    {
    point = this->m_SamplePoints[c];
    this->template TransformPoint<TransformOutputType>(point, oldMappedVoxels[c]);
    }

  // Apply the delta parameters to the transform
  this->UpdateTransformParameters(deltaParameters);

  // compute the indices mapped by the new transform
  for (SizeValueType c=0; c<numSamples; c++)
    {
    point = this->m_SamplePoints[c];
    this->template TransformPoint<TransformOutputType>(point, newMappedVoxel);

    // find the local shift for each sample point
    sampleShifts[c] = newMappedVoxel.EuclideanDistanceTo(oldMappedVoxels[c]);
  }

  // restore the parameters in the transform
  transform->SetParameters(oldParameters);
}

/** Print the information about this class */
template< typename TMetric >
void
RegistrationParameterScalesFromPhysicalShift< TMetric >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // namespace itk

#endif
