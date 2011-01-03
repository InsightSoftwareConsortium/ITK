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
#include "itkConditionVariable.h"

namespace itk
{
ConditionVariable::ConditionVariable()
{
#ifdef ITK_USE_PTHREADS
  pthread_mutex_init(&m_Mutex, NULL);
  pthread_cond_init(&m_ConditionVariable, NULL);
#else
#ifdef WIN32
  m_NumberOfWaiters = 0;
  m_WasBroadcast = 0;
  m_Semaphore = CreateSemaphore(NULL,         // no security
                                0,            // initial value
                                0x7fffffff,   // max count
                                NULL);        // unnamed
  InitializeCriticalSection(&m_NumberOfWaitersLock);
  m_WaitersAreDone = CreateEvent(NULL,            // no security
                                 FALSE,           // auto-reset
                                 FALSE,           // non-signaled initially
                                 NULL);           // unnamed
#endif
#endif
}

ConditionVariable::~ConditionVariable()
{
#ifdef ITK_USE_PTHREADS
  pthread_mutex_destroy(&m_Mutex);
  pthread_cond_destroy(&m_ConditionVariable);
#else
#ifdef WIN32
  CloseHandle(m_Semaphore);
  CloseHandle(m_WaitersAreDone);
  DeleteCriticalSection(&m_NumberOfWaitersLock);
#endif
#endif
}

void ConditionVariable::Signal()
{
#ifdef ITK_USE_PTHREADS
  pthread_cond_signal(&m_ConditionVariable);
#else
#ifdef WIN32
  EnterCriticalSection(&m_NumberOfWaitersLock);
  bool haveWaiters = ( m_NumberOfWaiters > 0 );
  LeaveCriticalSection(&m_NumberOfWaitersLock);

  // if there were not any waiters, then this is a no-op
  if ( haveWaiters )
    {
    ReleaseSemaphore(m_Semaphore, 1, 0);
    }
#endif
#endif
}

void ConditionVariable::Broadcast()
{
#ifdef ITK_USE_PTHREADS
  pthread_cond_broadcast(&m_ConditionVariable);
#else
#ifdef WIN32
  // This is needed to ensure that m_NumberOfWaiters and m_WasBroadcast are
  // consistent
  EnterCriticalSection(&m_NumberOfWaitersLock);
  bool haveWaiters = false;

  if ( m_NumberOfWaiters > 0 )
    {
    // We are broadcasting, even if there is just one waiter...
    // Record that we are broadcasting, which helps optimize Wait()
    // for the non-broadcast case
    m_WasBroadcast = 1;
    haveWaiters = true;
    }

  if ( haveWaiters )
    {
    // Wake up all waiters atomically
    ReleaseSemaphore(m_Semaphore, m_NumberOfWaiters, 0);

    LeaveCriticalSection(&m_NumberOfWaitersLock);

    // Wait for all the awakened threads to acquire the counting
    // semaphore
    WaitForSingleObject(m_WaitersAreDone, INFINITE);
    // This assignment is ok, even without the m_NumberOfWaitersLock held
    // because no other waiter threads can wake up to access it.
    m_WasBroadcast = 0;
    }
  else
    {
    LeaveCriticalSection(&m_NumberOfWaitersLock);
    }
#endif
#endif
}

void ConditionVariable::Wait(SimpleMutexLock *mutex)
{
#ifdef ITK_USE_PTHREADS
  pthread_cond_wait( &m_ConditionVariable, &mutex->GetMutexLock() );
#else
#ifdef WIN32
  // Avoid race conditions
  EnterCriticalSection(&m_NumberOfWaitersLock);
  m_NumberOfWaiters++;
  LeaveCriticalSection(&m_NumberOfWaitersLock);

  // This call atomically releases the mutex and waits on the
  // semaphore until signaled
  SignalObjectAndWait(mutex->GetMutexLock(), m_Semaphore, INFINITE, FALSE);

  // Reacquire lock to avoid race conditions
  EnterCriticalSection(&m_NumberOfWaitersLock);

  // We're no longer waiting....
  m_NumberOfWaiters--;

  // Check to see if we're the last waiter after the broadcast
  int lastWaiter = m_WasBroadcast && m_NumberOfWaiters == 0;

  LeaveCriticalSection(&m_NumberOfWaitersLock);

  // If we're the last waiter thread during this particular broadcast
  // then let the other threads proceed
  if ( lastWaiter )
    {
    // This call atomically signals the m_WaitersAreDone event and waits
    // until it can acquire the external mutex.  This is required to
    // ensure fairness
    SignalObjectAndWait(m_WaitersAreDone, mutex->GetMutexLock(),
                        INFINITE, FALSE);
    }
  else
    {
    // Always regain the external mutex since that's the guarentee we
    // give to our callers
    WaitForSingleObject(mutex->GetMutexLock(), INFINITE);
    }
#endif
#endif
}
} //end of namespace itk
