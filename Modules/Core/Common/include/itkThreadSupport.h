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
#ifndef itkThreadSupport_h
#define itkThreadSupport_h


// This implementation uses a routine called SignalObjectAndWait()
// which is only defined on WinNT 4.0 or greater systems.  We need to
// define this symbol in order to get the prototype for the
// routine. This needs to be done before we load any system headers.
#ifdef ITK_USE_WIN32_THREADS
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501 //TBB 4.4 requires WinXP (0x0501 or greater)
#endif
#endif

#if defined(ITK_USE_PTHREADS)
#include <pthread.h>
#elif defined(ITK_USE_WIN32_THREADS)
#include "itkWindows.h"
#include <winbase.h>
#endif


namespace itk
{
  /** Platform specific typedefs for simple types
   */
#if defined(ITK_USE_PTHREADS)
#define ITK_MAX_THREADS              128
  typedef pthread_mutex_t MutexType;
  typedef pthread_mutex_t FastMutexType;
  typedef void *( * ThreadFunctionType )(void *);
  typedef pthread_t ThreadProcessIdType;
#define ITK_THREAD_RETURN_VALUE  NULL /* This is from a c library, and always needs to be NULL, not ITK_NULLPTR */
#define ITK_THREAD_RETURN_TYPE   void *

#elif defined(ITK_USE_WIN32_THREADS)

#define ITK_MAX_THREADS              128
  typedef HANDLE                 MutexType;
  typedef CRITICAL_SECTION       FastMutexType;
  typedef unsigned(__stdcall * ThreadFunctionType)(void *);
  typedef HANDLE                 ThreadProcessIdType;
#define ITK_THREAD_RETURN_VALUE 0
#define ITK_THREAD_RETURN_TYPE unsigned __stdcall

#else

#define ITK_MAX_THREADS              1
  typedef int     MutexType;
  typedef int     FastMutexType;
  typedef void ( *ThreadFunctionType )(void *);
  typedef int     ThreadProcessIdType;
#define ITK_THREAD_RETURN_VALUE
#define ITK_THREAD_RETURN_TYPE void

#endif

  /** Platform specific Conditional Variable typedef
   */
#if defined(ITK_USE_PTHREADS)
  typedef struct {
  pthread_cond_t m_ConditionVariable;
  } ConditionVariableType;
#elif defined(ITK_USE_WIN32_THREADS)
  typedef struct {
  int m_NumberOfWaiters;                   // number of waiting threads
  CRITICAL_SECTION m_NumberOfWaitersLock;  // Serialize access to
                                           // m_NumberOfWaiters

  HANDLE m_Semaphore;                      // Semaphore to queue threads
  HANDLE m_WaitersAreDone;                 // Auto-reset event used by the
                                           // broadcast/signal thread to
                                           // wait for all the waiting
                                           // threads to wake up and
                                           // release the semaphore

  int m_WasBroadcast;                      // Used as boolean. Keeps track of whether
                                           // we were broadcasting or signaling
  } ConditionVariableType;
#else
  typedef struct { } ConditionVariableType;
#endif


}
#endif
