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
#include <iostream>
#include "itkRealTimeClock.h"

#if defined( WIN32 ) || defined( _WIN32 )
#include <windows.h>
#else
#include <sys/time.h>
#endif  // defined(WIN32) || defined(_WIN32)

#include "itkMath.h"

namespace itk
{

RealTimeClock::RealTimeClock():m_Frequency(1)
{
#if defined( WIN32 ) || defined( _WIN32 )
  LARGE_INTEGER frequency;
  ::QueryPerformanceFrequency(&frequency);

  this->m_Frequency =
    static_cast< FrequencyType >( static_cast<__int64>( frequency.QuadPart ) );

  SYSTEMTIME st1;
  SYSTEMTIME st2;
  FILETIME   ft1;
  FILETIME   ft2;

  ::memset( &st1, 0, sizeof( st1 ) );
  ::memset( &st2, 0, sizeof( st2 ) );

  st1.wYear = 1601;
  st1.wMonth = 1;
  st1.wDay = 1;

  st2.wYear = 1970;
  st2.wMonth = 1;
  st2.wDay = 1;

  ::SystemTimeToFileTime(&st1, &ft1);
  ::SystemTimeToFileTime(&st2, &ft2);

  LARGE_INTEGER ui1;
  LARGE_INTEGER ui2;

  memcpy( &ui1, &ft1, sizeof( ui1 ) );
  memcpy( &ui2, &ft2, sizeof( ui2 ) );

  this->m_Difference =
    static_cast< TimeStampType >( ui2.QuadPart - ui1.QuadPart )
    / static_cast< TimeStampType >( 1e7 );

  FILETIME      currentTime;
  LARGE_INTEGER intTime;
  LARGE_INTEGER tick;

  ::GetSystemTimeAsFileTime(&currentTime);
  ::QueryPerformanceCounter(&tick);

  memcpy( &intTime, &currentTime, sizeof( intTime ) );

  this->m_Origin = static_cast< TimeStampType >( intTime.QuadPart ) / static_cast< TimeStampType >( 1e7 );
  this->m_Origin -= static_cast< TimeStampType >( static_cast<__int64>( tick.QuadPart ) ) / this->m_Frequency;
  this->m_Origin -= this->m_Difference;
#else
  this->m_Frequency = 1e6;
  this->m_Difference = 0.0;
  this->m_Origin = 0.0;
#endif  // defined(WIN32) || defined(_WIN32)
}


RealTimeClock::~RealTimeClock() {}

RealTimeClock::TimeStampType
RealTimeClock::GetTimeInSeconds() const
{
#if defined( WIN32 ) || defined( _WIN32 )
  LARGE_INTEGER tick;
  ::QueryPerformanceCounter(&tick);

  TimeStampType value = static_cast< TimeStampType >( static_cast<__int64>( tick.QuadPart ) ) / this->m_Frequency;
  value += this->m_Origin;
  return value;
#else
  struct timeval tval;
  ::gettimeofday(&tval, ITK_NULLPTR);

  TimeStampType value = static_cast< TimeStampType >( tval.tv_sec ) + static_cast< TimeStampType >( tval.tv_usec )
                        / this->m_Frequency;
  return value;
#endif  // defined(WIN32) || defined(_WIN32)
}

const TimeStamp &
RealTimeClock::GetTimeStamp() const
{
  return this->Superclass::GetTimeStamp();
}

RealTimeStamp
RealTimeClock::GetRealTimeStamp() const
{
#if defined( WIN32 ) || defined( _WIN32 )
  LARGE_INTEGER tick;
  ::QueryPerformanceCounter(&tick);

  TimeStampType seconds = static_cast< TimeStampType >( static_cast<__int64>( tick.QuadPart ) ) / this->m_Frequency;
  seconds += this->m_Origin;

  typedef RealTimeStamp::SecondsCounterType       SecondsCounterType;
  typedef RealTimeStamp::MicroSecondsCounterType  MicroSecondsCounterType;

  SecondsCounterType iseconds = std::floor( seconds );
  MicroSecondsCounterType useconds = std::floor( ( seconds - iseconds ) * 1e6 );

  RealTimeStamp value( iseconds, useconds );
  return value;
#else
  struct timeval tval;
  ::gettimeofday(&tval, ITK_NULLPTR);

  RealTimeStamp value( static_cast<RealTimeStamp::SecondsCounterType>(tval.tv_sec), static_cast<RealTimeStamp::MicroSecondsCounterType>(tval.tv_usec) );
  return value;
#endif  // defined(WIN32) || defined(_WIN32)
}

void RealTimeClock::PrintSelf(std::ostream & os, itk::Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Frequency of the clock: "
     << this->m_Frequency << std::endl;
  os << indent << "Difference : "
     << this->m_Difference << std::endl;
  os << indent << "Origin : "
     << this->m_Origin << std::endl;
}
} // end namespace itk
