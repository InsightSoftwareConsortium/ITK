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
  m_ConditionVariable.m_NumberOfWaiters = 0;
  m_ConditionVariable.m_WasBroadcast = 0;
  m_ConditionVariable.m_Semaphore = CreateSemaphore(ITK_NULLPTR,         // no security
                                                    0,            // initial value
                                                    0x7fffffff,   // max count
                                                    ITK_NULLPTR);        // unnamed
  InitializeCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);
  m_ConditionVariable.m_WaitersAreDone = CreateEvent(ITK_NULLPTR,            // no security
                                                     FALSE,           // auto-reset
                                                     FALSE,           // non-signaled initially
                                                     ITK_NULLPTR);           // unnamed
}

ConditionVariable::~ConditionVariable()
{
  CloseHandle(m_ConditionVariable.m_Semaphore);
  CloseHandle(m_ConditionVariable.m_WaitersAreDone);
  DeleteCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);
}

void ConditionVariable::Signal()
{
  EnterCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);
  bool haveWaiters = ( m_ConditionVariable.m_NumberOfWaiters > 0 );
  LeaveCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);

  // if there were not any waiters, then this is a no-op
  if ( haveWaiters )
    {
    ReleaseSemaphore(m_ConditionVariable.m_Semaphore, 1, 0);
    }
}

void ConditionVariable::Broadcast()
{
  // This is needed to ensure that m_NumberOfWaiters and m_WasBroadcast are
  // consistent
  EnterCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);
  bool haveWaiters = false;

  if ( m_ConditionVariable.m_NumberOfWaiters > 0 )
    {
    // We are broadcasting, even if there is just one waiter...
    // Record that we are broadcasting, which helps optimize Wait()
    // for the non-broadcast case
    m_ConditionVariable.m_WasBroadcast = 1;
    haveWaiters = true;
    }

  if ( haveWaiters )
    {
    // Wake up all waiters atomically
    ReleaseSemaphore(m_ConditionVariable.m_Semaphore, m_ConditionVariable.m_NumberOfWaiters, 0);

    LeaveCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);

    // Wait for all the awakened threads to acquire the counting
    // semaphore
    WaitForSingleObject(m_ConditionVariable.m_WaitersAreDone, INFINITE);
    // This assignment is ok, even without the m_NumberOfWaitersLock held
    // because no other waiter threads can wake up to access it.
    m_ConditionVariable.m_WasBroadcast = 0;
    }
  else
    {
    LeaveCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);
    }
}

void ConditionVariable::Wait(SimpleMutexLock *mutex)
{
  // Avoid race conditions
  EnterCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);
  m_ConditionVariable.m_NumberOfWaiters++;
  LeaveCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);

  // This call atomically releases the mutex and waits on the
  // semaphore until signaled
  SignalObjectAndWait(mutex->GetMutexLock(), m_ConditionVariable.m_Semaphore, INFINITE, FALSE);

  // Reacquire lock to avoid race conditions
  EnterCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);

  // We're no longer waiting....
  m_ConditionVariable.m_NumberOfWaiters--;

  // Check to see if we're the last waiter after the broadcast
  int lastWaiter = m_ConditionVariable.m_WasBroadcast && m_ConditionVariable.m_NumberOfWaiters == 0;

  LeaveCriticalSection(&m_ConditionVariable.m_NumberOfWaitersLock);

  // If we're the last waiter thread during this particular broadcast
  // then let the other threads proceed
  if ( lastWaiter )
    {
    // This call atomically signals the m_WaitersAreDone event and waits
    // until it can acquire the external mutex.  This is required to
    // ensure fairness
    SignalObjectAndWait(m_ConditionVariable.m_WaitersAreDone, mutex->GetMutexLock(),
                        INFINITE, FALSE);
    }
  else
    {
    // Always regain the external mutex since that's the guarantee we
    // give to our callers
    WaitForSingleObject(mutex->GetMutexLock(), INFINITE);
    }
}
} //end of namespace itk
