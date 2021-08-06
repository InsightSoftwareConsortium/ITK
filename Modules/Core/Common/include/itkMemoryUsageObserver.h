/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkMemoryUsageObserver_h
#define itkMemoryUsageObserver_h

#include "itkConfigure.h"
#include "itkMacro.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkIntTypes.h"

#if defined(WIN32) || defined(_WIN32)
#  include <windows.h>
#  define SUPPORT_TOOLHELP32
#  if defined(SUPPORT_TOOLHELP32)
using NTSTATUS = LONG;
#  endif
#endif

namespace itk
{
/** \class MemoryUsageObserver
 * \brief Provides the memory usage of the process.
 *
 * This class represents a memory load analyser object
 * and provides a memory usage in platform-independent format.
 *
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT MemoryUsageObserverBase
{
public:
  /** Define the type for the memory usage */
  using MemoryLoadType = SizeValueType;

  /** destructor */
  virtual ~MemoryUsageObserverBase();

  /** Returns the memory load in kO */
  virtual MemoryLoadType
  GetMemoryUsage() = 0;
};

#if defined(WIN32) || defined(_WIN32)
class ITKCommon_EXPORT WindowsMemoryUsageObserver : public MemoryUsageObserverBase
{
public:
  WindowsMemoryUsageObserver();
  /** destructor */
  virtual ~WindowsMemoryUsageObserver();

  /** Returns the memory load in kO */
  virtual MemoryLoadType
  GetMemoryUsage();

protected:
#  if defined(SUPPORT_TOOLHELP32)
  using PZwQuerySystemInformation = NTSTATUS(WINAPI *)(UINT, PVOID, ULONG, PULONG);

  // handle ntdll.dll library
  HMODULE m_hNTLib;
  // Windows native API function to query system information
  PZwQuerySystemInformation ZwQuerySystemInformation;
#  endif // defined(SUPPORT_TOOLHELP32)
};
#endif // defined(WIN32) || defined(_WIN32)

#ifdef __linux__
class ITKCommon_EXPORT LinuxMemoryUsageObserver : public MemoryUsageObserverBase
{
public:
  /** destructor */
  ~LinuxMemoryUsageObserver() override;
  MemoryLoadType
  GetMemoryUsage() override;
};
#endif // __linux__

#if defined(__APPLE__)
class ITKCommon_EXPORT MacOSXMemoryUsageObserver : public MemoryUsageObserverBase
{
public:
  /** destructor */
  ~MacOSXMemoryUsageObserver() override;
  MemoryLoadType
  GetMemoryUsage() override;
};
#endif // Mac OS X

#if defined(__SUNPRO_CC) || defined(__sun__)
class ITKCommon_EXPORT SunSolarisMemoryUsageObserver : public MemoryUsageObserverBase
{
public:
  /** destructor */
  virtual ~SunSolarisMemoryUsageObserver();
  virtual MemoryLoadType
  GetMemoryUsage();
};
#endif // Sun Solaris

#if !defined(WIN32) && !defined(_WIN32)
class ITKCommon_EXPORT SysResourceMemoryUsageObserver : public MemoryUsageObserverBase
{
public:
  /** destructor */
  ~SysResourceMemoryUsageObserver() override;
  MemoryLoadType
  GetMemoryUsage() override;
};

#  if defined(ITK_HAS_MALLINFO) || defined(ITK_HAS_MALLINFO2)
/** \class MallinfoMemoryUsageObserver
 * \brief The MallinfoMemoryUsageObserver
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT MallinfoMemoryUsageObserver : public MemoryUsageObserverBase
{
public:
  /** destructor */
  ~MallinfoMemoryUsageObserver() override;
  MemoryLoadType
  GetMemoryUsage() override;
};
#  endif // Mallinfo
#endif   // !defined(WIN32) && !defined(_WIN32)

/* \class MemoryUsageObserver
 * The best MemoryUsageObserver has been chosen for each OS.
 * However, SysResourceMemoryUsageObserver is far from being accurate. Other
 * way of getting the Memory Usage shall be used.
 * For FreeBSD, some alternatives would be to parse the output of
 * "sysctl vm.vmtotal" or "sysctl -a | grep -i memory"
 */
class ITKCommon_EXPORT MemoryUsageObserver
  :
#if defined(WIN32) || defined(_WIN32)
  public WindowsMemoryUsageObserver
#elif defined(__linux__)
  public LinuxMemoryUsageObserver
#elif defined(__SUNPRO_CC) || defined(__sun__)
  public SunSolarisMemoryUsageObserver
#elif defined(__APPLE__)
  public MacOSXMemoryUsageObserver
#elif defined(__FreeBSD__) || defined(__OpenBSD__)
  public SysResourceMemoryUsageObserver
#else
  public MallinfoMemoryUsageObserver
#endif
{
public:
  /** destructor */
  ~MemoryUsageObserver() override;
};
} // end of namespace itk

#endif // itkMemoryUsageObserver_h
