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
#include "itkTimeProbe.h"

namespace itk
{
TimeProbe
::TimeProbe():ResourceProbe< TimeStampType, TimeStampType >("Time", "s")
{
  m_RealTimeClock   = RealTimeClock::New();
}

TimeProbe
::~TimeProbe()
{}

TimeProbe::TimeStampType
TimeProbe
::GetInstantValue(void) const
{
  return m_RealTimeClock->GetTimeInSeconds();
}

#if !defined(ITK_LEGACY_REMOVE)
TimeProbe::TimeStampType
TimeProbe
::GetMeanTime(void) const
{
  return this->GetMean();
}
#endif
} // end namespace itk
