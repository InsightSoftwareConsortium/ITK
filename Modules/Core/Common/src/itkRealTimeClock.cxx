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
#include <iostream>
#include "itkRealTimeClock.h"

#if defined(WIN32) || defined(_WIN32)
#  include <windows.h>
#else
#  include <sys/time.h>
#endif // defined(WIN32) || defined(_WIN32)

#include "itkMath.h"

namespace itk
{

RealTimeClock::RealTimeClock()
{
#if defined(WIN32) || defined(_WIN32)
  LARGE_INTEGER frequency;
  ::QueryPerformanceFrequency(&frequency);

  m_Frequency = static_cast<FrequencyType>(frequency.QuadPart);

  // Converts a FILETIME to the number of seconds since January 1, 1601.
  const auto convertToSeconds = [](const FILETIME fileTime) {
    const auto numberOfIntervals =
      uint64_t{ fileTime.dwHighDateTime } * (uint64_t{ UINT32_MAX } + 1) + uint64_t{ fileTime.dwLowDateTime };
    return static_cast<TimeStampType>(numberOfIntervals) / TimeStampType{ 1e7 };
  };

  SYSTEMTIME systemTime{};
  systemTime.wYear = 1970;
  systemTime.wMonth = 1;
  systemTime.wDay = 1;

  FILETIME fileTime;
  ::SystemTimeToFileTime(&systemTime, &fileTime);

  m_Difference = convertToSeconds(fileTime);

  FILETIME      currentTime;
  LARGE_INTEGER tick;

  ::GetSystemTimeAsFileTime(&currentTime);
  ::QueryPerformanceCounter(&tick);

  m_Origin = convertToSeconds(currentTime) - static_cast<TimeStampType>(tick.QuadPart) / m_Frequency - m_Difference;
#else
  m_Frequency = 1e6;
  m_Difference = 0.0;
  m_Origin = 0.0;
#endif // defined(WIN32) || defined(_WIN32)
}


RealTimeClock::~RealTimeClock() = default;

RealTimeClock::TimeStampType
RealTimeClock::GetTimeInSeconds() const
{
#if defined(WIN32) || defined(_WIN32)
  LARGE_INTEGER tick;
  ::QueryPerformanceCounter(&tick);

  TimeStampType value = static_cast<TimeStampType>(tick.QuadPart) / m_Frequency;
  value += m_Origin;
  return value;
#else
  struct timeval tval;
  ::gettimeofday(&tval, nullptr);

  TimeStampType value =
    static_cast<TimeStampType>(tval.tv_sec) + static_cast<TimeStampType>(tval.tv_usec) / m_Frequency;
  return value;
#endif // defined(WIN32) || defined(_WIN32)
}

const TimeStamp &
RealTimeClock::GetTimeStamp() const
{
  return this->Superclass::GetTimeStamp();
}

RealTimeStamp
RealTimeClock::GetRealTimeStamp() const
{
#if defined(WIN32) || defined(_WIN32)
  LARGE_INTEGER tick;
  ::QueryPerformanceCounter(&tick);

  TimeStampType seconds = static_cast<TimeStampType>(tick.QuadPart) / m_Frequency;
  seconds += m_Origin;

  using SecondsCounterType = RealTimeStamp::SecondsCounterType;
  using MicroSecondsCounterType = RealTimeStamp::MicroSecondsCounterType;

  SecondsCounterType      iseconds = std::floor(seconds);
  MicroSecondsCounterType useconds = std::floor((seconds - iseconds) * 1e6);

  RealTimeStamp value(iseconds, useconds);
  return value;
#else
  struct timeval tval;
  ::gettimeofday(&tval, nullptr);

  RealTimeStamp value(static_cast<RealTimeStamp::SecondsCounterType>(tval.tv_sec),
                      static_cast<RealTimeStamp::MicroSecondsCounterType>(tval.tv_usec));
  return value;
#endif // defined(WIN32) || defined(_WIN32)
}

void
RealTimeClock::PrintSelf(std::ostream & os, itk::Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Frequency of the clock: " << m_Frequency << std::endl;
  os << indent << "Difference : " << m_Difference << std::endl;
  os << indent << "Origin : " << m_Origin << std::endl;
}
} // end namespace itk
