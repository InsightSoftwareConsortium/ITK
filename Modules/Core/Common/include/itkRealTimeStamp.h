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
#ifndef itkRealTimeStamp_h
#define itkRealTimeStamp_h

#include "itkRealTimeInterval.h"
#include "itkMacro.h"
#include <iostream>

namespace itk
{
/** \class RealTimeStamp
 * \brief The RealTimeStamp is a data structure for representing time with high
 * precision and a large dynamic range.
 *
 * This class represents time typically for applications that need to mark the
 * time of acquisition of data with high precision (microseconds) and a large
 * dynamic range (years).
 *
 * By default, the real time stamp is initialized to the origin of
 * the Unix epoch. That is the time 00:00:00 UTC on 1 January 1970
 * (or 1970-01-01T00:00:00Z ISO 8601)
 *
 * \sa RealTimeInterval
 * \sa RealTimeClock
 *
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT RealTimeStamp
{
public:

  typedef  RealTimeStamp     Self;

  friend class RealTimeClock;

  /** Constructor */
  RealTimeStamp();

  /** Destructor */
  ~RealTimeStamp();

  /** Native type used to represent the time in different time units. */
  typedef RealTimeInterval::TimeRepresentationType TimeRepresentationType;

  /** Return time in multiple units. */
  TimeRepresentationType GetTimeInMicroSeconds() const;
  TimeRepresentationType GetTimeInMilliSeconds() const;
  TimeRepresentationType GetTimeInSeconds() const;
  TimeRepresentationType GetTimeInMinutes() const;
  TimeRepresentationType GetTimeInHours() const;
  TimeRepresentationType GetTimeInDays() const;

  /** Arithmetic operations between RealTimeInterval and RealTimeStamp. */
  RealTimeInterval operator-( const Self & ) const;
  Self operator+( const RealTimeInterval & ) const;
  Self operator-( const RealTimeInterval & ) const;
  const Self & operator+=( const RealTimeInterval & );
  const Self & operator-=( const RealTimeInterval & );

  /** Comparison operations. */
  bool operator>( const Self & ) const;
  bool operator<( const Self & ) const;
  bool operator==( const Self & ) const;
  bool operator!=( const Self & ) const;
  bool operator<=( const Self & ) const;
  bool operator>=( const Self & ) const;

  /** Default print out of a RealTimeStamp */
  friend ITKCommon_EXPORT std::ostream & operator<<(std::ostream & os, const RealTimeStamp & v);

private:

  typedef   uint64_t   SecondsCounterType;
  typedef   uint64_t   MicroSecondsCounterType;

  /** Constructor with values. Intentionally made private */
  RealTimeStamp( SecondsCounterType, MicroSecondsCounterType );

  typedef   RealTimeInterval::SecondsDifferenceType        SecondsDifferenceType;
  typedef   RealTimeInterval::MicroSecondsDifferenceType   MicroSecondsDifferenceType;

  /** Number of Seconds and Microseconds since... */
  SecondsCounterType        m_Seconds;
  MicroSecondsCounterType   m_MicroSeconds;

};

} // end of namespace itk

#endif  // itkRealTimeStamp_h
