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
#include "itkRealTimeInterval.h"
#include "itkNumericTraits.h"

// This macro ensures that the sign of the seconds is the same as the sign of
// the microseconds. In other words, both of them are measured toward the same
// direction of time.
#define ALIGN_THE_ARROW_OF_TIME( seconds, micro_seconds ) \
if( seconds > 0 && micro_seconds < 0 ) \
  { \
  seconds -= 1; \
  micro_seconds = 1000000L - micro_seconds; \
  } \
if( seconds < 0 && micro_seconds > 0 ) \
  { \
  seconds += 1; \
  micro_seconds = 1000000L + micro_seconds; \
  }

namespace itk
{

/**
 * Constructor to initialize a time stamp
 */
RealTimeInterval::RealTimeInterval()
{
  this->m_Seconds = itk::NumericTraits< SecondsDifferenceType >::ZeroValue();
  this->m_MicroSeconds = itk::NumericTraits< MicroSecondsDifferenceType >::ZeroValue();
}

/**
 * Constructor to initialize a time stamp
 */
RealTimeInterval::RealTimeInterval( SecondsDifferenceType seconds, MicroSecondsDifferenceType micro_seconds )
{
  // Ensure that microseconds are carry over
  // to seconds if there are more than a million.
  seconds += micro_seconds / 1000000L;
  micro_seconds = micro_seconds % 1000000L;

  ALIGN_THE_ARROW_OF_TIME( seconds, micro_seconds );

  this->m_Seconds = seconds;
  this->m_MicroSeconds = micro_seconds;
}

/**
 * Destructor.
 */
RealTimeInterval::~RealTimeInterval()
{
}

/**
 * Set the interval to a given combination of seconds and micro seconds.
 */
void RealTimeInterval::Set( SecondsDifferenceType seconds, MicroSecondsDifferenceType micro_seconds )
{
  // Ensure that microseconds carry over
  // to seconds if there are more than a million.
  seconds += micro_seconds / 1000000L;
  micro_seconds = micro_seconds % 1000000L;

  ALIGN_THE_ARROW_OF_TIME( seconds, micro_seconds );

  this->m_Seconds = seconds;
  this->m_MicroSeconds = micro_seconds;
}

/**
 * Return time in microseconds.
 */
RealTimeInterval::TimeRepresentationType
RealTimeInterval::GetTimeInMicroSeconds() const
{
  TimeRepresentationType result = static_cast< TimeRepresentationType >( this->m_Seconds );
  result *= 1e6;
  result += static_cast< TimeRepresentationType >( this->m_MicroSeconds );

  return result;
}

/**
 * Return time in milliseconds.
 */
RealTimeInterval::TimeRepresentationType
RealTimeInterval::GetTimeInMilliSeconds() const
{
  TimeRepresentationType result = static_cast< TimeRepresentationType >( this->m_Seconds );
  result *= 1e3;
  result += static_cast< TimeRepresentationType >( this->m_MicroSeconds ) / 1e3;

  return result;
}

/**
 * Return time in seconds.
 */
RealTimeInterval::TimeRepresentationType
RealTimeInterval::GetTimeInSeconds() const
{
  TimeRepresentationType result = static_cast< TimeRepresentationType >( this->m_MicroSeconds );
  result /= 1e6;
  result += static_cast< TimeRepresentationType >( this->m_Seconds );

  return result;
}

/**
 * Return time in minutes.
 */
RealTimeInterval::TimeRepresentationType
RealTimeInterval::GetTimeInMinutes() const
{
  const TimeRepresentationType result = this->GetTimeInSeconds() / 60.00;
  return result;
}

/**
 * Return time in hours.
 */
RealTimeInterval::TimeRepresentationType
RealTimeInterval::GetTimeInHours() const
{
  const TimeRepresentationType result = this->GetTimeInSeconds() / 3600.00;
  return result;
}

/**
 * Return time in days.
 */
RealTimeInterval::TimeRepresentationType
RealTimeInterval::GetTimeInDays() const
{
  const TimeRepresentationType result = this->GetTimeInSeconds() / 86400.00;
  return result;
}

/**
 * Compute the time difference between two time intervals.
 */
RealTimeInterval
RealTimeInterval::operator-( const RealTimeInterval & other ) const
{
  SecondsDifferenceType       seconds       = this->m_Seconds      - other.m_Seconds;
  MicroSecondsDifferenceType  micro_seconds = this->m_MicroSeconds - other.m_MicroSeconds;

  ALIGN_THE_ARROW_OF_TIME( seconds, micro_seconds );

  RealTimeInterval result;

  result.m_Seconds = seconds;
  result.m_MicroSeconds = micro_seconds;

  return result;
}

/**
 * Compute the addition between two time intervals.
 */
RealTimeInterval
RealTimeInterval::operator+( const RealTimeInterval & other ) const
{
  SecondsDifferenceType       seconds       = this->m_Seconds      + other.m_Seconds;
  MicroSecondsDifferenceType  micro_seconds = this->m_MicroSeconds + other.m_MicroSeconds;

  ALIGN_THE_ARROW_OF_TIME( seconds, micro_seconds );

  RealTimeInterval result;

  result.m_Seconds = seconds;
  result.m_MicroSeconds = micro_seconds;

  return result;
}

/**
 * Add a time interval to this time stamp and update it.
 */
const RealTimeInterval::Self &
RealTimeInterval::operator+=( const RealTimeInterval & other )
{
  SecondsDifferenceType       seconds       = this->m_Seconds      + other.m_Seconds;
  MicroSecondsDifferenceType  micro_seconds = this->m_MicroSeconds + other.m_MicroSeconds;

  ALIGN_THE_ARROW_OF_TIME( seconds, micro_seconds );

  this->m_Seconds = seconds;
  this->m_MicroSeconds = micro_seconds;

  return *this;
}

/**
 * Subtract a time interval from this time stamp and update it.
 */
const RealTimeInterval::Self &
RealTimeInterval::operator-=( const RealTimeInterval & other )
{
  SecondsDifferenceType       seconds       = this->m_Seconds      - other.m_Seconds;
  MicroSecondsDifferenceType  micro_seconds = this->m_MicroSeconds - other.m_MicroSeconds;

  ALIGN_THE_ARROW_OF_TIME( seconds, micro_seconds );

  this->m_Seconds = seconds;
  this->m_MicroSeconds = micro_seconds;

  return *this;
}

/**
 * Compare two time Intervals.
 */
bool
RealTimeInterval::operator>( const Self & other ) const
{
  if(  this->m_Seconds > other.m_Seconds )
    {
    return true;
    }

  if(  this->m_Seconds < other.m_Seconds )
    {
    return false;
    }

  return ( this->m_MicroSeconds > other.m_MicroSeconds );
}

/**
 * Compare two time Intervals.
 */
bool
RealTimeInterval::operator<( const Self & other ) const
{
  if(  this->m_Seconds < other.m_Seconds )
    {
    return true;
    }

  if(  this->m_Seconds > other.m_Seconds )
    {
    return false;
    }

  return ( this->m_MicroSeconds < other.m_MicroSeconds );
}

/**
 * Compare two time Intervals.
 */
bool
RealTimeInterval::operator>=( const Self & other ) const
{
  if(  this->m_Seconds > other.m_Seconds )
    {
    return true;
    }

  if(  this->m_Seconds < other.m_Seconds )
    {
    return false;
    }

  return ( this->m_MicroSeconds >= other.m_MicroSeconds );
}

/**
 * Compare two time Intervals.
 */
bool
RealTimeInterval::operator<=( const Self & other ) const
{
  if(  this->m_Seconds < other.m_Seconds )
    {
    return true;
    }

  if(  this->m_Seconds > other.m_Seconds )
    {
    return false;
    }

  return ( this->m_MicroSeconds <= other.m_MicroSeconds );
}

/**
 * Compare two time Intervals.
 */
bool
RealTimeInterval::operator==( const Self & other ) const
{
  return ( ( this->m_MicroSeconds == other.m_MicroSeconds ) &&
           ( this->m_Seconds == other.m_Seconds ) );
}

/**
 * Compare two time Intervals.
 */
bool
RealTimeInterval::operator!=( const Self & other ) const
{
  return ( ( this->m_MicroSeconds != other.m_MicroSeconds ) ||
           ( this->m_Seconds != other.m_Seconds ) );
}

/** Default print out of a RealTimeStamp */
std::ostream & operator<<(std::ostream & os, const RealTimeInterval & v)
{
  os << v.GetTimeInSeconds() << " seconds ";
  return os;
}


} // end of namespace itk
