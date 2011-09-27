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
#include "itkTemporalRegion.h"

namespace itk
{

//
// Constructor
//
TemporalRegion
::TemporalRegion()
  : m_RealStart(),
    m_RealDuration(0,0),
    m_FrameStart(0),
    m_FrameDuration(0)
{
}

//
// Destructor
//
TemporalRegion
::~TemporalRegion()
{
}

// ---------------------------------------------------------------------------
void
TemporalRegion
::SetRealStart(const RealTimeStamp s)
{
  this->m_RealStart = s;
}

// ---------------------------------------------------------------------------
RealTimeStamp
TemporalRegion
::GetRealStart() const
{
  return this->m_RealStart;
}

// ---------------------------------------------------------------------------
void
TemporalRegion
::SetRealDuration(const RealTimeInterval d)
{
  this->m_RealDuration = d;
}

RealTimeInterval
TemporalRegion
::GetRealDuration() const
{
  return this->m_RealDuration;
}

// ---------------------------------------------------------------------------
void
TemporalRegion
::SetFrameStart(const FrameOffsetType s)
{
  this->m_FrameStart = s;
}

// ---------------------------------------------------------------------------
TemporalRegion::FrameOffsetType
TemporalRegion
::GetFrameStart() const
{
  return this->m_FrameStart;
}

// ---------------------------------------------------------------------------
void
TemporalRegion
::SetFrameDuration(const FrameOffsetType d)
{
  this->m_FrameDuration = d;
}

// ---------------------------------------------------------------------------
TemporalRegion::FrameOffsetType
TemporalRegion
::GetFrameDuration() const
{
  return this->m_FrameDuration;
}

// ---------------------------------------------------------------------------
TemporalRegion::RegionType
TemporalRegion
::GetRegionType() const
{
  return ITK_STRUCTURED_REGION;
}

// ---------------------------------------------------------------------------
bool
TemporalRegion
::IsEqualInFrames(const Self & region) const
{
  return m_FrameStart == region.m_FrameStart &&
    m_FrameDuration == region.m_FrameDuration;
}

// ---------------------------------------------------------------------------
bool
TemporalRegion
::IsEqualInRealTime(const Self & region) const
{
  return m_RealStart == region.m_RealStart &&
    m_RealDuration == region.m_RealDuration;
}

// ---------------------------------------------------------------------------
bool
TemporalRegion
::operator==(const Self & region) const
{
  return IsEqualInFrames(region) && IsEqualInRealTime(region);
}

// ---------------------------------------------------------------------------
bool
TemporalRegion
::operator!=(const Self & region) const
{
  return !(operator==(region));
}

// ---------------------------------------------------------------------------
void
TemporalRegion
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "RealTime Start: " << m_RealStart << std::endl;
  os << indent << "RealTime Duration: " << m_RealDuration << std::endl;
  os << indent << "Frame Start: " << m_FrameStart << std::endl;
  os << indent << "Frame Duration: " << m_FrameDuration << std::endl;
}

// ---------------------------------------------------------------------------
std::ostream & operator<<(std::ostream & os, const TemporalRegion & region)
{
  region.Print(os);
  return os;
}

} // end namespace itk
