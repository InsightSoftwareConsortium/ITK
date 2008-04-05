/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMemoryUsageObserver.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkMemoryUsageObserver.h"

#if defined(WIN32) || defined(_WIN32)
  #include <windows.h>
#if 0
  #include <psapi.h>
#endif
#elif linux
  #include "itkSmapsFileParser.h"
#else
  #include <sys/resource.h>     // getrusage()
  #if !defined(__APPLE__) && !defined(__SUNPRO_CC) && !defined (__sun__)
    #include <malloc.h>           // mallinfo()
  #endif
#endif

namespace itk
{

MemoryUsageObserverBase::~MemoryUsageObserverBase()
{
}


#if defined(WIN32) || defined(_WIN32)

WindowsMemoryUsageObserver::~WindowsMemoryUsageObserver()
{
}

MemoryUsageObserverBase::MemoryLoadType 
WindowsMemoryUsageObserver::GetMemoryUsage()
{
#if 0
  DWORD pid = GetCurrentProcessId();
  PROCESS_MEMORY_COUNTERS memoryCounters;

  HANDLE  hProcess = OpenProcess(  PROCESS_QUERY_INFORMATION |
                                   PROCESS_VM_READ,
                                   FALSE, pid );

  if (NULL == hProcess)
    {
    // Can't determine memory usage.
    return 0;
    }

  GetProcessMemoryInfo( hProcess, &memoryCounters, sizeof(memoryCounters));

  MemoryLoadType mem = static_cast<MemoryLoadType>( 
                          static_cast<double>( memoryCounters.PagefileUsage )
                                                                          / 1024.0 );
#else
  MemoryLoadType mem = 0;
#endif
  return mem;
}

#elif linux

LinuxMemoryUsageObserver::~LinuxMemoryUsageObserver()
{
}

MemoryUsageObserverBase::MemoryLoadType 
LinuxMemoryUsageObserver::GetMemoryUsage()
{
  SmapsFileParser<SmapsData_2_6<SmapsRecord> > m_ParseSmaps;
  m_ParseSmaps.ReadFile();
  return m_ParseSmaps.GetHeapUsage() + m_ParseSmaps.GetStackUsage();
}

#else  // Unix and Mac Platforms

SysResourceMemoryUsageObserver::~SysResourceMemoryUsageObserver()
{
}

MemoryUsageObserverBase::MemoryLoadType 
SysResourceMemoryUsageObserver::GetMemoryUsage()
{
  // Maybe use getrusage() ??
  rusage resourceInfo;

  int who = RUSAGE_SELF;
  if (getrusage(who, &resourceInfo) == 0)
    {
    return resourceInfo.ru_ixrss;
    }

  return 0;
}
#if !defined(__APPLE__) && !defined(__SUNPRO_CC) && !defined (__sun__)

MallinfoMemoryUsageObserver::~MallinfoMemoryUsageObserver()
{
}

MemoryUsageObserverBase::MemoryLoadType 
MallinfoMemoryUsageObserver::GetMemoryUsage()
{
  struct mallinfo minfo = mallinfo(); 

  MemoryLoadType mem = static_cast<MemoryLoadType>( 
                          static_cast<double>( minfo.uordblks ) / 1024.0 );
  return mem;
}

#endif //  !defined(__APPLE__) && !defined(__SUNPRO_CC) && !defined (__sun__)

#endif // // Unix and Mac Platforms

}//end namespace itk
