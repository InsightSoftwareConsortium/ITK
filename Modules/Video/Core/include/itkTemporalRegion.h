/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkTemporalRegion_h
#define itkTemporalRegion_h

#include "itkRegion.h"
#include "itkRealTimeStamp.h"
#include "itkRealTimeInterval.h"
#include "itkNumericTraits.h"
#include <climits>
#include "ITKVideoCoreExport.h"

/** Define representations of infinite duration for frames and real time */
#define ITK_INFINITE_FRAME_DURATION itk::NumericTraits<itk::SizeValueType>::max()
#define ITK_INFINITE_REAL_DURATION RealTimeInterval(ITK_INFINITE_FRAME_DURATION, 0)

namespace itk
{
/**
 * \class TemporalRegion
 * \brief Region subclass that holds a region in time
 *
 * A temporal region is represented using a starting point and a duration. Here
 * time can be measured both in frame numbers or real time (or both).
 *
 * \ingroup ITKVideoCore
 */
class ITKVideoCore_EXPORT TemporalRegion : public Region
{
public:
  /** Standard class type aliases */
  using Self = TemporalRegion;
  using Superclass = Region;

  itkTypeMacro(TemporalRegion, Region);

  /** Typedef for frame offsets */
  using FrameOffsetType = itk::SizeValueType;

  /** Get/Set RealStart */
  void
  SetRealStart(const RealTimeStamp s);
  RealTimeStamp
  GetRealStart() const;

  /** Get/Set RealDuration */
  void
  SetRealDuration(const RealTimeInterval d);
  RealTimeInterval
  GetRealDuration() const;

  /** Get/Set FrameStart */
  void
  SetFrameStart(const FrameOffsetType s);
  FrameOffsetType
  GetFrameStart() const;

  /** Get/Set FrameDuration */
  void
  SetFrameDuration(const FrameOffsetType d);
  FrameOffsetType
  GetFrameDuration() const;

  /** Return RegionType (SRUCTURED_REGION) */
  RegionEnum
  GetRegionType() const override;

  /** Constructor */
  TemporalRegion();

  /** Destructor */
  ~TemporalRegion() override;

  /** Compare two temporal regions in Frame space */
  virtual bool
  IsEqualInFrames(const Self & region) const;

  /** Compare two temporal regions in Frame space */
  bool
  IsEqualInRealTime(const Self & region) const;

  /** Compare two temporal regions. (Both Frame and RealTime) */
  bool
  operator==(const Self & region) const;

  ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(Self);

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Time boundaries */
  /** Timestamp corresponding to the first frame in the region. */
  RealTimeStamp m_RealStart;
  /** Time interval corresponding to the entire length of time
   *  represented by the region over ALL frames */
  RealTimeInterval m_RealDuration;
  /** Index of the first frame in the region */
  FrameOffsetType m_FrameStart{ 0 };
  /** Total number of frames represented by the region (NOT individual frame duration) */
  FrameOffsetType m_FrameDuration{ 0 };

}; // end class TemporalRegion

/** ostream operator */
ITKVideoCore_EXPORT std::ostream &
                    operator<<(std::ostream & os, const TemporalRegion & region);

} // end namespace itk

#endif
