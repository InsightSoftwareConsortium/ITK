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
#include "itkMemoryUsageObserver.h"

#if defined( WIN32 ) || defined( _WIN32 )
  #include <windows.h>
  #if defined( SUPPORT_PSAPI )
    #include <psapi.h>
  #endif
#endif // defined(WIN32) || defined(_WIN32)

#if defined( __SUNPRO_CC ) || defined ( __sun__ )
  #include <unistd.h>
  #include <stdio.h>
  #include <string>
  #include <sstream>
#endif // !defined(__SUNPRO_CC) && !defined (__sun__)

#if !defined( WIN32 ) && !defined( _WIN32 )
  #include <sys/resource.h>     // getrusage()
  #if defined( ITK_HAS_MALLINFO )
    #include <malloc.h>           // mallinfo()
  #endif // ITK_HAS_MALLINFO
#endif // !defined(WIN32) && !defined(_WIN32)

#if defined( __OpenBSD__ )
#include <stdlib.h>
#endif

#ifdef linux
#include <fstream>
#include <unistd.h>
#endif

#ifdef __APPLE__
#include <sys/sysctl.h>
#include <mach/mach.h>
#include <stdint.h>
#include <unistd.h>
#endif

namespace itk
{
MemoryUsageObserverBase::~MemoryUsageObserverBase()
{}

#if defined( WIN32 ) || defined( _WIN32 )

/**         ----         Windows Memory Usage Observer       ----       */

WindowsMemoryUsageObserver::WindowsMemoryUsageObserver()
{
#if defined( SUPPORT_TOOLHELP32 )
  m_hNTLib = ::LoadLibraryA("ntdll.dll");
  if ( m_hNTLib )
    {
    // load the support function from the kernel
    ZwQuerySystemInformation = ( PZwQuerySystemInformation ) ::GetProcAddress(m_hNTLib,
                                                                              "ZwQuerySystemInformation");
    }
#endif
}

WindowsMemoryUsageObserver::~WindowsMemoryUsageObserver()
{
#if defined ( SUPPORT_TOOLHELP32 )
  if ( m_hNTLib )
    {
    FreeLibrary(m_hNTLib);
    }
#endif
}

#if defined( SUPPORT_TOOLHELP32 )

#define STATUS_INFO_LENGTH_MISMATCH ( (NTSTATUS)0xC0000004L )

typedef LONG KPRIORITY;
#define SystemProcessesAndThreadsInformation    5

typedef struct _CLIENT_ID {
  DWORD UniqueProcess;
  DWORD UniqueThread;
} CLIENT_ID;

typedef struct _UNICODE_STRING {
  USHORT Length;
  USHORT MaximumLength;
  PWSTR Buffer;
} UNICODE_STRING;

typedef struct _VM_COUNTERS {
#ifdef _WIN64
  // the following was inferred by painful reverse engineering
  SIZE_T PeakVirtualSize;             // not actually
  SIZE_T PageFaultCount;
  SIZE_T PeakWorkingSetSize;
  SIZE_T WorkingSetSize;
  SIZE_T QuotaPeakPagedPoolUsage;
  SIZE_T QuotaPagedPoolUsage;
  SIZE_T QuotaPeakNonPagedPoolUsage;
  SIZE_T QuotaNonPagedPoolUsage;
  SIZE_T PagefileUsage;
  SIZE_T PeakPagefileUsage;
  SIZE_T VirtualSize;                 // not actually
#else
  SIZE_T PeakVirtualSize;
  SIZE_T VirtualSize;
  ULONG PageFaultCount;
  SIZE_T PeakWorkingSetSize;
  SIZE_T WorkingSetSize;
  SIZE_T QuotaPeakPagedPoolUsage;
  SIZE_T QuotaPagedPoolUsage;
  SIZE_T QuotaPeakNonPagedPoolUsage;
  SIZE_T QuotaNonPagedPoolUsage;
  SIZE_T PagefileUsage;
  SIZE_T PeakPagefileUsage;
#endif
} VM_COUNTERS;

typedef struct _SYSTEM_THREADS {
  LARGE_INTEGER KernelTime;
  LARGE_INTEGER UserTime;
  LARGE_INTEGER CreateTime;
  ULONG WaitTime;
  PVOID StartAddress;
  CLIENT_ID ClientId;
  KPRIORITY Priority;
  KPRIORITY BasePriority;
  ULONG ContextSwitchCount;
  LONG State;
  LONG WaitReason;
} SYSTEM_THREADS, *PSYSTEM_THREADS;

typedef struct _SYSTEM_PROCESSES { // Information Class 5
  ULONG NextEntryDelta;
  ULONG ThreadCount;
  ULONG Reserved1[6];
  LARGE_INTEGER CreateTime;
  LARGE_INTEGER UserTime;
  LARGE_INTEGER KernelTime;
  UNICODE_STRING ProcessName;
  KPRIORITY BasePriority;
#ifdef _WIN64
  ULONG pad1;
  ULONG ProcessId;
  ULONG pad2;
  ULONG InheritedFromProcessId;
  ULONG pad3;
  ULONG pad4;
  ULONG pad5;
#else
  ULONG ProcessId;
  ULONG InheritedFromProcessId;
#endif
  ULONG HandleCount;
  ULONG Reserved2[2];
  VM_COUNTERS VmCounters;
#if defined( _WIN64 ) || _WIN32_WINNT >= 0x500
  IO_COUNTERS IoCounters;
#endif
  SYSTEM_THREADS Threads[1];
} SYSTEM_PROCESSES, *PSYSTEM_PROCESSES;
#endif

MemoryUsageObserverBase::MemoryLoadType
WindowsMemoryUsageObserver::GetMemoryUsage()
{
  MemoryLoadType mem = 0;

#if defined( SUPPORT_PSAPI )
  DWORD                   pid = GetCurrentProcessId();
  PROCESS_MEMORY_COUNTERS memoryCounters;

  HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION
                                | PROCESS_VM_READ,
                                FALSE, pid);

  if ( ITK_NULLPTR == hProcess )
    {
    // Can't determine memory usage.
    return 0;
    }

  GetProcessMemoryInfo( hProcess, &memoryCounters, sizeof( memoryCounters ) );

  mem = static_cast< MemoryLoadType >(
    static_cast< double >( memoryCounters.PagefileUsage )
    / 1024.0 );
#elif defined( SUPPORT_TOOLHELP32 )

  /* Retrieve memory usage using Windows Native API. For more information,
   * read the book "Windows NT 2000 Native API Reference"
  */

  if ( !m_hNTLib )
    {
    itkGenericExceptionMacro(<< "Can't find ntdll.dll. "
                             << "You should probably disable SUPPORT_TOOLHELP32");
    }
  // the ntdll.dll library could not have been opened (file not found?)
  if ( !ZwQuerySystemInformation )
    {
    itkGenericExceptionMacro(<< "The file ntdll.dll is not supported. "
                             << "You should probably disable SUPPORT_TOOLHELP32");
    return mem;
    }

  DWORD             pid = GetCurrentProcessId();
  ULONG             n = 50;
  PSYSTEM_PROCESSES sp = new SYSTEM_PROCESSES[n];
  // as we can't know how many processes are running, we loop and test a new size
  // every time.
  while ( ZwQuerySystemInformation(SystemProcessesAndThreadsInformation,
                                   sp, n * sizeof *sp, 0)
          == STATUS_INFO_LENGTH_MISMATCH )
    {
    delete[] sp;
    n = n * 2;
    sp = new SYSTEM_PROCESSES[n];
    }
  bool done = false;
  for ( PSYSTEM_PROCESSES spp = sp;
        !done;
        spp = PSYSTEM_PROCESSES(PCHAR(spp) + spp->NextEntryDelta) )
    {
    // only the current process is interesting here
    if ( spp->ProcessId == pid )
      {
      mem = static_cast< MemoryLoadType >(
        static_cast< double >( spp->VmCounters.PagefileUsage - sizeof( *sp ) ) / 1024 );
      break;
      }
    done = ( spp->NextEntryDelta == 0 );
    }
  delete[] sp;

#else

  /* This solution is not optimal as it returns the system memory usage
   * instead of the process memory usage.
  */

  MEMORYSTATUSEX statex;

  statex.dwLength = sizeof( statex );

  GlobalMemoryStatusEx (&statex);

  mem   = static_cast< MemoryLoadType >(
    static_cast< double >( statex.ullTotalPhys - statex.ullAvailPhys ) / 1024 );
#endif
  return mem;
}

#endif // WIN32

#if defined(linux)

/**         ----         Linux Memory Usage Observer       ----       */

LinuxMemoryUsageObserver::~LinuxMemoryUsageObserver()
{}

/** Get Memory Usage - Linux version.
 *  Reference for method used:
 *  http://stackoverflow.com/questions/669438/how-to-get-memory-usage-at-run-time-in-c
 */
MemoryUsageObserverBase::MemoryLoadType
LinuxMemoryUsageObserver::GetMemoryUsage()
{
  std::ifstream procstats("/proc/self/stat",std::ios_base::in);
  // dummy vars for leading entries in stat that we don't care about
  //
  std::string pid, comm, state, ppid, pgrp, session, tty_nr;
  std::string tpgid, flags, minflt, cminflt, majflt, cmajflt;
  std::string utime, stime, cutime, cstime, priority, nice;
  std::string O, itrealvalue, starttime;

  // the two fields we want
  //
  unsigned long vsize;
  long rss;

  procstats >> pid >> comm >> state >> ppid >> pgrp >> session >> tty_nr
              >> tpgid >> flags >> minflt >> cminflt >> majflt >> cmajflt
              >> utime >> stime >> cutime >> cstime >> priority >> nice
              >> O >> itrealvalue >> starttime >> vsize >> rss; // don't care about the rest

  procstats.close();

  long page_size_kb = sysconf(_SC_PAGE_SIZE) / 1024; // in case x86-64 is configured to use 2MB pages
  //  vm_usage     = vsize / 1024.0;
  return rss * page_size_kb;
}

#endif // linux

#if defined(__APPLE__)

/**         ----         Mac OS X Memory Usage Observer       ----       */

MacOSXMemoryUsageObserver::~MacOSXMemoryUsageObserver()
{}

MemoryUsageObserverBase::MemoryLoadType
MacOSXMemoryUsageObserver::GetMemoryUsage()
{
  //
  // this method comes from
  // http://stackoverflow.com/questions/5839626/how-is-top-able-to-see-memory-usage
  task_t targetTask = mach_task_self();
  struct task_basic_info ti;
  mach_msg_type_number_t count = TASK_BASIC_INFO_64_COUNT;
  kern_return_t kr =
    task_info(targetTask, TASK_BASIC_INFO_64,
              (task_info_t) &ti, &count);
  if (kr != KERN_SUCCESS)
    {
    return 0;
    }

  // On Mac OS X, the resident_size is in bytes, not pages!
  // (This differs from the GNU Mach kernel)
  return ti.resident_size / 1024;
}

#endif // Mac OS X

#if defined( __SUNPRO_CC ) || defined ( __sun__ )

/**         ----         Sun Solaris Memory Usage Observer       ----       */

SunSolarisMemoryUsageObserver::~SunSolarisMemoryUsageObserver()
{}

/** On Sun Solaris machines, the system call pmap returns information on process.
 *  Calling "pmap PID", the output shall be like the following:
 *  102905:    *my_app*
 *  00010000    192K r-x--  /usr/bin/my_app
 *  00042000     40K rwx--    [ heap ]
 *  FF180000    664K r-x--  /usr/lib/libc.so.1
 *  FF236000     24K rwx--  /usr/lib/libc.so.1
 *  FF23C000      8K rwx--  /usr/lib/libc.so.1
 *  FF250000      8K rwx--    [ anon ]
 *  ...       ...    ...    ...
 *  FF3F6000      8K rwx--  /usr/lib/ld.so.1
 *  FFBFC000     16K rw---    [ stack ]
 *   total     1880K
 */
MemoryUsageObserverBase::MemoryLoadType
SunSolarisMemoryUsageObserver::GetMemoryUsage()
{
  MemoryLoadType mem = 0;
  int            pid = getpid();

  FILE *            fp = ITK_NULLPTR;
  std::stringstream command;

  command << "pmap " << pid << std::endl;

  if ( ( fp = popen(command.str().c_str(), "r") ) == ITK_NULLPTR )
    {
    itkGenericExceptionMacro(<< "Error using pmap. Can execute pmap command");
    }
  char remaining[256];
  int  pmappid = -1;
  fscanf(fp, "%d:%s", &pmappid, remaining);
  //the first word shall be the process ID
  if ( pmappid != pid )
    {
    itkGenericExceptionMacro(<< "Error using pmap. 1st line output shall be PID: name");
    }
  bool        heapNotFound = true;
  char        address[64], perms[32];
  int         memUsage = 0;
  std::string mapping;
  while ( heapNotFound )
    {
    if ( fscanf(fp, "%s %dK %s", address, &memUsage, perms) != 3 )
      {
      break;
      }
    if ( fgets(remaining, 256, fp) != ITK_NULLPTR )
      {
      mapping = remaining;
      if ( mapping.find("[ heap ]", 0) != std::string::npos )
        {
        mem = memUsage;
        heapNotFound = false;
        break;
        }
      // if no [ heap ] token is defined, accumulate all the [ xxx ] tokens
      else if ( mapping.find("[ ", 0) != std::string::npos
                && mapping.find(" ]", 0) != std::string::npos )
        {
        mem += memUsage;
        }
      }
    else
      {
      if ( ferror (fp) )
        {
        itkGenericExceptionMacro(<< "Error using pmap. Corrupted pmap output");
        }
      }
    }
  if ( pclose(fp) == -1 )
    {
    itkGenericExceptionMacro(<< "Error using pmap. Can't close pmap output file.");
    }
  return mem;
}

#endif //defined(__SUNPRO_CC) || defined (__sun__)

#if !defined( WIN32 ) && !defined( _WIN32 ) || defined( __OpenBSD__ )

/**         ----         SysResource Memory Usage Observer       ----       */

SysResourceMemoryUsageObserver::~SysResourceMemoryUsageObserver()
{}

MemoryUsageObserverBase::MemoryLoadType
SysResourceMemoryUsageObserver::GetMemoryUsage()
{
  // Maybe use getrusage() ??
  rusage resourceInfo;

  const int who = RUSAGE_SELF;
  if ( getrusage(who, &resourceInfo) == 0 )
    {
    return static_cast<MemoryUsageObserverBase::MemoryLoadType> (resourceInfo.ru_ixrss);
    }

  return 0;
}

#if defined( ITK_HAS_MALLINFO )

/**         ----         Mallinfo Memory Usage Observer       ----       */

MallinfoMemoryUsageObserver::~MallinfoMemoryUsageObserver()
{}

MemoryUsageObserverBase::MemoryLoadType
MallinfoMemoryUsageObserver::GetMemoryUsage()
{
  struct mallinfo minfo = mallinfo();

  MemoryLoadType mem = static_cast< MemoryLoadType >(
    static_cast< double >( minfo.uordblks ) / 1024.0 );

  return mem;
}

#endif //  ITK_HAS_MALLINFO

#endif // Unix and Mac Platforms !defined(WIN32) && !defined(_WIN32)


//Destructor for MemoryUsageObserver
MemoryUsageObserver::~MemoryUsageObserver(){}
} //end namespace itk
