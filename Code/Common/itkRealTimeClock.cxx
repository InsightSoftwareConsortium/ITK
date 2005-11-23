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

  this->m_Frequency = 
    static_cast< FrequencyType >( (__int64)frequency.QuadPart );

  SYSTEMTIME st1;
  SYSTEMTIME st2;
  FILETIME ft1;
  FILETIME ft2;

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
    static_cast< TimeStampType >( ui2.QuadPart - ui1.QuadPart) / 
    static_cast< TimeStampType >( 1e7 );

  FILETIME currentTime;
  LARGE_INTEGER intTime;
  LARGE_INTEGER tick;

  ::GetSystemTimeAsFileTime( &currentTime );
  ::QueryPerformanceCounter( &tick );

  memcpy( &intTime, &currentTime, sizeof( intTime ) );

  this->m_Origin = 
    static_cast< TimeStampType >( intTime.QuadPart ) / 
    static_cast< TimeStampType >( 1e7 );

  this->m_Origin -= 
    static_cast< TimeStampType >( (__int64)tick.QuadPart ) / 
    this->m_Frequency;
    
  this->m_Origin +=  this->m_Difference;


#else

  this->m_Frequency = 1e6;;

#endif  // defined(WIN32) || defined(_WIN32)
}

/** Destructor */
RealTimeClock::~RealTimeClock()
{
}

/** Returns a timestamp in seconds */
RealTimeClock::TimeStampType
RealTimeClock::GetTimestamp() const
{
#if defined(WIN32) || defined(_WIN32)

  LARGE_INTEGER tick;
  
  ::QueryPerformanceCounter( &tick );

  TimeStampType value = 
      static_cast< TimeStampType >( (__int64)tick.QuadPart ) / 
      this->m_Frequency;

  value += this->m_Origin;

  return value;

#else

  struct timeval tval;

  ::gettimeofday( &tval, 0 );

  TimeStampType value = 
    static_cast< TimeStampType >( tval.tv_sec ) +
    static_cast< TimeStampType >( tval.tv_usec ) / this->m_Frequency;

  return value;

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
