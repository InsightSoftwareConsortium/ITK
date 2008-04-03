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

#ifdef WIN32
  #include <windows.h>
  #include <psapi.h>
#elif __linux
  #include "itkSmapsFileParser.h"
#else
  #include <sys/resource.h>     // getrusage()
  #include <malloc.h>           // mallinfo()
#endif

namespace itk
{

#ifdef WIN32

MemoryUsageObserverBase::MemoryLoadType 
WindowsMemoryUsageObserver::GetMemoryUsage()
{
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
  return mem;
}

#elif __linux

MemoryUsageObserverBase::MemoryLoadType 
LinuxMemoryUsageObserver::GetMemoryUsage()
{
  SmapsFileParser<SmapsData_2_6<SmapsRecord> > m_ParseSmaps;
  m_ParseSmaps.ReadFile();
  return m_ParseSmaps.GetHeapUsage() + m_ParseSmaps.GetStackUsage();
}

#else  // Unix and Mac Platforms

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

MemoryUsageObserverBase::MemoryLoadType 
MallinfoMemoryUsageObserver::GetMemoryUsage()
{
  struct mallinfo minfo = mallinfo(); 

  MemoryLoadType mem = static_cast<MemoryLoadType>( 
                          static_cast<double>( minfo.uordblks ) / 1024.0 );
  return mem;
}

#endif

}//end namespace itk
