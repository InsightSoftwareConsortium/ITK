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
#ifndef itkRealTimeInterval_h
#define itkRealTimeInterval_h

#include "itkIntTypes.h"
#include "itkMacro.h"
#include <iostream>

namespace itk
{
/** \class RealTimeInterval
 * \brief A data structure for representing the time
 * span between two RealTimeStamps, with similar high precision and a large
 * dynamic range to what the RealTimeStamps offer.
 *
 * This class represents the difference between two time points, typically for
 * applications that need to mark the time of acquisition of data with high
 * precision (microseconds) and a large dynamic range (years). This class will
 * be the natural representation for the duration of a video sequence, or for
 * the time that has passed between the acquisition of one images and a
 * subsequent one.
 *
 * \sa RealTimeStamp
 * \sa RealTimeClock
 *
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT RealTimeInterval
{
public:

  typedef RealTimeInterval    Self;

  /** Internal types used to represent seconds and microseconds. */
  typedef   int64_t   SecondsDifferenceType;
  typedef   int64_t   MicroSecondsDifferenceType;

  /** Constructor */
  RealTimeInterval();

  /** Constructor with values. Intentionally made public */
  RealTimeInterval( SecondsDifferenceType, MicroSecondsDifferenceType );

  /** Destructor */
  ~RealTimeInterval();

  /** Native type used to represent the time in different time units. */
  typedef   double    TimeRepresentationType;

  /** Return time in multiple units. */
  TimeRepresentationType GetTimeInMicroSeconds() const;
  TimeRepresentationType GetTimeInMilliSeconds() const;
  TimeRepresentationType GetTimeInSeconds() const;
  TimeRepresentationType GetTimeInMinutes() const;
  TimeRepresentationType GetTimeInHours() const;
  TimeRepresentationType GetTimeInDays() const;

  /** Arithmetic operations between RealTimeInterval and RealTimeInterval. */
  Self operator-( const Self & ) const;
  Self operator+( const Self & ) const;
  const Self & operator-=( const Self & );
  const Self & operator+=( const Self & );

  /** Comparison operations. */
  bool operator>( const Self & ) const;
  bool operator<( const Self & ) const;
  bool operator==( const Self & ) const;
  bool operator!=( const Self & ) const;
  bool operator<=( const Self & ) const;
  bool operator>=( const Self & ) const;

  /** Set with values. The units and signs of the seconds and microseconds will
   * be harmonized internally. */
  void Set( SecondsDifferenceType, MicroSecondsDifferenceType );

  /** Default print out of a RealTimeInterval */
  friend ITKCommon_EXPORT std::ostream & operator<<(std::ostream & os, const RealTimeInterval & v);

private:

  friend class RealTimeStamp;

  /** Number of Seconds and Microseconds since... */
  SecondsDifferenceType        m_Seconds;

  /** Number of Microseconds since the second.
   *  Should be in the range -999,999 to 999,999
   *  and it must always have the same sign as
   *  the m_Seconds member variable. */
  MicroSecondsDifferenceType   m_MicroSeconds;

};

} // end of namespace itk

#endif  // itkRealTimeInterval_h
