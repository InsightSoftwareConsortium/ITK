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
#ifndef __itkTemporalRegion_h
#define __itkTemporalRegion_h

#include "itkRegion.h"
#include "itkRealTimeStamp.h"
#include "itkRealTimeInterval.h"

namespace itk
{

/** \class TemporalRegion
 * \brief Region subclass that holds a spatial region as well as beginning and
 * end time-points
 *
 * Time points are stored in both frame numbers and with RealTimeStamps
 */
class ITK_EXPORT TemporalRegion : public Region
{
public:

  /** Standard class typedefs */
  typedef TemporalRegion Self;

  itkTypeMacro(TemporalRegion, Region);

  /** Get/Set RealStart */
  void SetRealStart(RealTimeStamp s) { this->m_RealStart = s; }
  RealTimeStamp GetRealStart() { return this->m_RealStart; }

  /** Get/Set RealDuration */
  void SetRealDuration(RealTimeInterval d) { this->m_RealDuration = d; }
  RealTimeInterval GetRealDuration() { return this->m_RealDuration; }

  /** Get/Set FrameStart */
  void SetFrameStart(unsigned long s) { this->m_FrameStart = s; }
  unsigned long GetFrameStart() { return this->m_FrameStart; }

  /** Get/Set FrameDuration */
  void SetFrameDuration(unsigned long d) { this->m_FrameDuration = d; }
  unsigned long GetFrameDuration() { return this->m_FrameDuration; }

  /** Return RegionType (SRUCTURED_REGION) */
  virtual RegionType GetRegionType() const { return ITK_STRUCTURED_REGION; }

  /** Constructor */
  TemporalRegion();

  /** Destructor */
  virtual ~TemporalRegion(){};

  /** Compare two temporal regions in Frame space */
  virtual bool IsEqualInFrames(const Self & region) const;

  /** Compare two temporal regions in Frame space */
  bool IsEqualInRealTime(const Self & region) const;

  /** Compare two temporal regions. (Both Frame and RealTime) */
  bool operator==(const Self & region) const;
  bool operator!=(const Self & region) const;

protected:

  /** Time boundaries */
  RealTimeStamp m_RealStart;
  RealTimeInterval m_RealDuration;
  unsigned long m_FrameStart;
  unsigned long m_FrameDuration;

};  // end class TemporalRegion

} // end namespace itk

#endif
