/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <log4cxx/config.h>

#ifdef HAVE_MS_THREAD
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0400 // SignalObjectAndWait
#endif
#include <windows.h>
#endif

#include <log4cxx/helpers/condition.h>

using namespace log4cxx::helpers;
using namespace log4cxx;

Condition::Condition()
{
#ifdef HAVE_PTHREAD
  ::pthread_cond_init(&condition, 0);
#elif defined(HAVE_MS_THREAD)
  waiters = 0;
  wasBroadCast = false;
  waitersDone = ::CreateEvent(0, FALSE, FALSE, NULL);
#endif
}

Condition::~Condition()
{
#ifdef HAVE_PTHREAD
  ::pthread_cond_destroy(&condition);
#elif defined(HAVE_MS_THREAD)
  ::CloseHandle(waitersDone);
#endif
}

void Condition::broadcast()
{
#ifdef HAVE_PTHREAD
  ::pthread_cond_broadcast(&condition);
#elif defined(HAVE_MS_THREAD)
#endif
}

void Condition::signal()
{
#ifdef HAVE_PTHREAD
  ::pthread_cond_signal(&condition);
#elif defined(HAVE_MS_THREAD)
  // If there aren't any waiters, then this is a no-op.  Note that
  // this function *must* be called with the <external_mutex> held
  // since other wise there is a race condition that can lead to the
  // lost wakeup bug...  This is needed to ensure that the <waiters>
  // value is not in an inconsistent internal state while being
  // updated by another thread.

  // if (waiters != 0) (atomic comparison)
#  if _MSC_VER == 1200  // MSDEV 6
  if ((long)InterlockedCompareExchange((void**)&waiters, 0, 0) != 0)
#  else
  if ((long)InterlockedCompareExchange(&waiters, 0, 0) != 0)
#  endif
  {
    sema.post();
  }
#endif
}

void Condition::wait(Mutex& mutex)
{
#ifdef HAVE_PTHREAD
  ::pthread_cond_wait(&condition, &mutex.mutex);
#elif defined(HAVE_MS_THREAD)

#if _MSC_VER == 1200  // MSDEV 6
  ::InterlockedIncrement((long *)&waiters);
#else
  ::InterlockedIncrement(&waiters);
#endif

    if (SignalObjectAndWait(mutex.mutex, sema.semaphore, INFINITE, FALSE)
    == WAIT_ABANDONED)
  {
    throw ConditionException();
  }

#if _MSC_VER == 1200  // MSDEV 6
  long oldWaiters = ::InterlockedDecrement((long*)&waiters);
#else
  long oldWaiters = ::InterlockedDecrement(&waiters);
#endif

  bool lastWaiter = wasBroadCast && (oldWaiters == 0);

  if (lastWaiter)
  {
    // This call atomically signals the <waiters_done_> event and
    // waits until it can acquire the mutex.  This is important to
    // prevent unfairness.
    if (SignalObjectAndWait(waitersDone, mutex.mutex, INFINITE, FALSE)
      == WAIT_ABANDONED)
    {
      throw ConditionException();
    }
  }

  mutex.lock();
#endif
}

void Condition::wait(Mutex& mutex, long timeOut)
{
}
