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
#ifndef itkRegionConstrainedSubsampler_hxx
#define itkRegionConstrainedSubsampler_hxx
#include "itkRegionConstrainedSubsampler.h"

namespace itk {
namespace Statistics {

template <typename TSample, typename TRegion>
RegionConstrainedSubsampler<TSample, TRegion>
::RegionConstrainedSubsampler()
{
  this->m_RequestMaximumNumberOfResults = true;
  this->m_RegionConstraintInitialized = false;
  this->m_SampleRegionInitialized = false;
}

template <typename TSample, typename TRegion>
typename LightObject::Pointer
RegionConstrainedSubsampler<TSample, TRegion>
::InternalClone() const
{
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval =
    dynamic_cast<Self *>(loPtr.GetPointer());
  if(rval.IsNull())
    {
    itkExceptionMacro(<< "downcast to type "
                      << this->GetNameOfClass()
                      << " failed.");
    }

  if (this->GetRegionConstraintInitialized())
    {
    rval->SetRegionConstraint(this->GetRegionConstraint());
    }
  else
    {
    rval->m_RegionConstraintInitialized = false;
    }
  if (this->GetSampleRegionInitialized())
    {
    rval->SetSampleRegion(this->GetSampleRegion());
    }
  else
    {
    rval->m_SampleRegionInitialized = false;
    }
  return loPtr;
}

template <typename TSample, typename TRegion>
void
RegionConstrainedSubsampler<TSample, TRegion>
::SetSampleRegion(const RegionType& region)
{
  itkDebugMacro("setting sample region to " << region);
  if (this->m_SampleRegion != region ||
      !(this->m_SampleRegionInitialized))
    {
    this->m_SampleRegion = region;
    this->m_SampleRegionInitialized = true;
    this->Modified();
    }
}

template <typename TSample, typename TRegion>
void
RegionConstrainedSubsampler<TSample, TRegion>
::SetRegionConstraint(const RegionType& region)
{
  itkDebugMacro("setting region constraint to " << region);
  if (this->m_RegionConstraint != region ||
      !(this->m_RegionConstraintInitialized))
    {
    this->m_RegionConstraint = region;
    this->m_RegionConstraintInitialized = true;
    this->Modified();
    }
}

template <typename TSample, typename TRegion>
void
RegionConstrainedSubsampler<TSample, TRegion>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if (m_SampleRegionInitialized)
    {
    os << indent << "Sample region initialized as: "
       << m_SampleRegion << std::endl;
    }
  else
    {
    os << indent << "Sample region not initialized yet." << std::endl;
    }

  if (m_RegionConstraintInitialized)
    {
    os << indent << "Using region constraint: " << m_RegionConstraint << std::endl;
    }
  else
    {
    os << indent << "Region constraint has not been initialized!" << std::endl;
    }

  os << std::endl;
}

}// end namespace Statistics
}// end namespace itk

#endif
