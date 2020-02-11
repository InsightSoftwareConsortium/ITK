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
#ifndef itkThreadSupport_h
#define itkThreadSupport_h

#include <cstdlib>

// This implementation uses a routine called SignalObjectAndWait()
// which is only defined on WinNT 4.0 or greater systems.  We need to
// define this symbol in order to get the prototype for the
// routine. This needs to be done before we load any system headers.
#ifdef ITK_USE_WIN32_THREADS
#  ifndef _WIN32_WINNT
#    define _WIN32_WINNT 0x0501 // TBB 4.4 requires WinXP (0x0501 or greater)
#  endif
#endif

#if defined(ITK_USE_PTHREADS)
#  include <pthread.h>
#elif defined(ITK_USE_WIN32_THREADS)
#  include "itkWindows.h"
#  include <winbase.h>
#endif
#include "itkConfigure.h"


namespace itk
{
/** Platform specific type alias for simple types
 */
#if defined(ITK_USE_PTHREADS)
constexpr std::size_t ITK_MAX_THREADS = ITK_DEFAULT_MAX_THREADS;
using MutexType = pthread_mutex_t;
using FastMutexType = pthread_mutex_t;
using ThreadFunctionType = void * (*)(void *);
using ThreadProcessIdType = pthread_t;
constexpr ThreadProcessIdType ITK_DEFAULT_THREAD_ID = 0;
using ITK_THREAD_RETURN_TYPE = void *;
constexpr ITK_THREAD_RETURN_TYPE ITK_THREAD_RETURN_DEFAULT_VALUE =
  NULL;                                     /* This is from a c library, and always needs to be NULL, not nullptr */
using itk::ITK_THREAD_RETURN_DEFAULT_VALUE; // We need this out of the itk namespace for #define to work below
using ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION = itk::ITK_THREAD_RETURN_TYPE;
#elif defined(ITK_USE_WIN32_THREADS)

constexpr std::size_t ITK_MAX_THREADS = ITK_DEFAULT_MAX_THREADS;
using MutexType = HANDLE;
using FastMutexType = CRITICAL_SECTION;
using ThreadFunctionType = unsigned(__stdcall *)(void *);
using ThreadProcessIdType = HANDLE;
static const ThreadProcessIdType ITK_DEFAULT_THREAD_ID = INVALID_HANDLE_VALUE;
using ITK_THREAD_RETURN_TYPE = unsigned;
constexpr ITK_THREAD_RETURN_TYPE ITK_THREAD_RETURN_DEFAULT_VALUE = 0;
// WINAPI expands to __stdcall which specifies a function call convention and has little no meaning on variable
// declarations
#  define ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION itk::ITK_THREAD_RETURN_TYPE __stdcall
#else

constexpr std::size_t ITK_MAX_THREADS = 1;
using MutexType = int;
using FastMutexType = int;
using ThreadFunctionType = void (*)(void *);
using ThreadProcessIdType = int;
constexpr ThreadProcessIdType ITK_DEFAULT_THREAD_ID = 0;
using ITK_THREAD_RETURN_TYPE = void;
#  define ITK_THREAD_RETURN_DEFAULT_VALUE
using ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION = itk::ITK_THREAD_RETURN_TYPE;
#endif


/** Platform specific Conditional Variable type alias
 */
#if defined(ITK_USE_PTHREADS)
using ConditionVariableType = struct
{
  pthread_cond_t m_ConditionVariable;
};
#elif defined(ITK_USE_WIN32_THREADS)
using ConditionVariableType = struct
{
  int              m_NumberOfWaiters;     // number of waiting threads
  CRITICAL_SECTION m_NumberOfWaitersLock; // Serialize access to
                                          // m_NumberOfWaiters

  HANDLE m_Semaphore;      // Semaphore to queue threads
  HANDLE m_WaitersAreDone; // Auto-reset event used by the
                           // broadcast/signal thread to
                           // wait for all the waiting
                           // threads to wake up and
                           // release the semaphore

  int m_WasBroadcast; // Used as boolean. Keeps track of whether
                      // we were broadcasting or signaling
};
#else
using ConditionVariableType = struct
{};
#endif

} // namespace itk

// Compile-time conditional code for different threading models
// require that some items are #defines (always global scope) or
// can sometimes be rigorously typed.  When rigorously typed,
// we need to re-exposed to the global namespace to keep the
// use of these items consistent.
#if defined(ITK_USE_PTHREADS)
using itk::ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION;
using itk::ITK_THREAD_RETURN_DEFAULT_VALUE; // We need this out of the itk namespace for #define to work below
#elif defined(ITK_USE_WIN32_THREADS)
using itk::ITK_THREAD_RETURN_DEFAULT_VALUE; // We need this out of the itk namespace for #define to work below
#else
using itk::ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION;
#endif

// For backwards compatibility
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
using itk::ITK_MAX_THREADS;
using itk::ITK_DEFAULT_THREAD_ID;
#endif

#endif
