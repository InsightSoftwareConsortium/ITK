/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRealTimeClock.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>
#include "itkRealTimeClock.h"

#if defined(WIN32) || defined(_WIN32)

#include <windows.h>

#else

#include <sys/time.h>

#endif  // defined(WIN32) || defined(_WIN32)


namespace itk
{

/** Constructor */
RealTimeClock::RealTimeClock():m_Frequency(1)
{
#if defined(WIN32) || defined(_WIN32)

  LARGE_INTEGER frequency;
  ::QueryPerformanceFrequency(&frequency);

  this->m_Frequency = double((__int64)frequency.QuadPart);

  SYSTEMTIME st1;
  SYSTEMTIME st2;
  FILETIME ft1;
  FILETIME ft2;

  ::memset(&st1, 0, sizeof(st1));
  ::memset(&st2, 0, sizeof(st2));
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
  memcpy(&ui1, &ft1, sizeof(ui1));
  memcpy(&ui2, &ft2, sizeof(ui2));
  this->m_Difference = double(ui2.QuadPart - ui1.QuadPart) / double(10000000);

  FILETIME currentTime;
  LARGE_INTEGER intTime;
  LARGE_INTEGER tick;

  ::GetSystemTimeAsFileTime(&currentTime);
  ::QueryPerformanceCounter(&tick);
  memcpy(&intTime, &currentTime, sizeof(intTime));

  this->m_Origin = double(intTime.QuadPart) / double(10000000) \
    - (double((__int64)tick.QuadPart) / this->m_Frequency) - this->m_Difference;


#else

  this->m_Frequency = 1000000;

#endif  // defined(WIN32) || defined(_WIN32)
}

/** Destructor */
RealTimeClock::~RealTimeClock()
{
}

/** Returns a timestamp in seconds */
double RealTimeClock::GetTimestamp() const
{
#if defined(WIN32) || defined(_WIN32)

  LARGE_INTEGER tick;
  ::QueryPerformanceCounter(&tick);
  return (double((__int64)tick.QuadPart) / this->m_Frequency) + this->m_Origin;

#else

  struct timeval tval;
  ::gettimeofday(&tval, 0);
  return double(tval.tv_sec) + double(tval.tv_usec) / this->m_Frequency;

#endif  // defined(WIN32) || defined(_WIN32)
}


/** Print the object */
void RealTimeClock::PrintSelf( std::ostream& os, itk::Indent indent ) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Frequency of the clock: "
    << this->m_Frequency << std::endl;
  os << indent << "Difference : "
    << this->m_Difference << std::endl;
  os << indent << "Origin : "
    << this->m_Origin << std::endl;
}

}
