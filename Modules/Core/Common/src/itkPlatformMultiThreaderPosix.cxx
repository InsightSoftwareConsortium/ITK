/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkPlatformMultiThreader.h"

#include "itkObjectFactory.h"
#include "itksys/SystemTools.hxx"
#include <unistd.h>

namespace itk
{
extern "C"
{
  using c_void_cast = void * (*)(void *);
}

#if !defined(ITK_LEGACY_REMOVE)
void
PlatformMultiThreader::MultipleMethodExecute()
{

  pthread_t process_id[ITK_MAX_THREADS];

  // obey the global maximum number of threads limit
  if (m_NumberOfWorkUnits > MultiThreaderBase::GetGlobalMaximumNumberOfThreads())
  {
    m_NumberOfWorkUnits = MultiThreaderBase::GetGlobalMaximumNumberOfThreads();
  }
  for (ThreadIdType thread_loop = 0; thread_loop < m_NumberOfWorkUnits; ++thread_loop)
  {
    if (m_MultipleMethod[thread_loop] == (ThreadFunctionType) nullptr)
    {
      itkExceptionMacro(<< "No multiple method set for: " << thread_loop);
    }
  }

  // Using POSIX threads
  //
  // We want to use pthread_create to start m_NumberOfWorkUnits - 1
  // additional
  // threads which will be used to call the NumberOfWorkUnits-1 methods
  // defined in m_MultipleMethods[](). The parent thread
  // will call m_MultipleMethods[NumberOfWorkUnits-1]().  When it is done,
  // it will wait for all the children to finish.
  //
  // First, start up the m_NumberOfWorkUnits-1 processes.  Keep track
  // of their process ids for use later in the pthread_join call

  pthread_attr_t attr;

  pthread_attr_init(&attr);
#  ifndef __CYGWIN__
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_PROCESS);
#  endif
  for (ThreadIdType thread_loop = 1; thread_loop < m_NumberOfWorkUnits; ++thread_loop)
  {
    m_ThreadInfoArray[thread_loop].UserData = m_MultipleData[thread_loop];
    m_ThreadInfoArray[thread_loop].NumberOfWorkUnits = m_NumberOfWorkUnits;
    int threadError = pthread_create(&(process_id[thread_loop]),
                                     &attr,
                                     reinterpret_cast<c_void_cast>(m_MultipleMethod[thread_loop]),
                                     ((void *)(&m_ThreadInfoArray[thread_loop])));
    if (threadError != 0)
    {
      itkExceptionMacro(<< "Unable to create a thread.  pthread_create() returned " << threadError);
    }
  }

  // Now, the parent thread calls the last method itself
  m_ThreadInfoArray[0].UserData = m_MultipleData[0];
  m_ThreadInfoArray[0].NumberOfWorkUnits = m_NumberOfWorkUnits;
  (m_MultipleMethod[0])((void *)(&m_ThreadInfoArray[0]));
  // The parent thread has finished its method - so now it
  // waits for each of the other processes to exit
  for (ThreadIdType thread_loop = 1; thread_loop < m_NumberOfWorkUnits; ++thread_loop)
  {
    pthread_join(process_id[thread_loop], nullptr);
  }
}

ThreadIdType
PlatformMultiThreader::SpawnThread(ThreadFunctionType f, void * UserData)
{
  ThreadIdType id = 0;

  while (id < ITK_MAX_THREADS)
  {
    if (!m_SpawnedThreadActiveFlagLock[id])
    {
      m_SpawnedThreadActiveFlagLock[id] = std::make_shared<std::mutex>();
    }
    m_SpawnedThreadActiveFlagLock[id]->lock();
    if (m_SpawnedThreadActiveFlag[id] == 0)
    {
      // We've got a useable thread id, so grab it
      m_SpawnedThreadActiveFlag[id] = 1;
      m_SpawnedThreadActiveFlagLock[id]->unlock();
      break;
    }
    m_SpawnedThreadActiveFlagLock[id]->unlock();

    ++id;
  }

  if (id >= ITK_MAX_THREADS)
  {
    itkExceptionMacro(<< "You have too many active threads!");
  }

  m_SpawnedThreadInfoArray[id].UserData = UserData;
  m_SpawnedThreadInfoArray[id].NumberOfWorkUnits = 1;
  m_SpawnedThreadInfoArray[id].ActiveFlag = &m_SpawnedThreadActiveFlag[id];
  m_SpawnedThreadInfoArray[id].ActiveFlagLock = m_SpawnedThreadActiveFlagLock[id];

  pthread_attr_t attr;

  pthread_attr_init(&attr);
#  ifndef __CYGWIN__
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_PROCESS);
#  endif

  int threadError = pthread_create(&(m_SpawnedThreadProcessID[id]),
                                   &attr,
                                   reinterpret_cast<c_void_cast>(f),
                                   ((void *)(&m_SpawnedThreadInfoArray[id])));
  if (threadError != 0)
  {
    itkExceptionMacro(<< "Unable to create a thread.  pthread_create() returned " << threadError);
  }

  return id;
}

void
PlatformMultiThreader::TerminateThread(ThreadIdType WorkUnitID)
{
  if (!m_SpawnedThreadActiveFlag[WorkUnitID])
  {
    return;
  }

  m_SpawnedThreadActiveFlagLock[WorkUnitID]->lock();
  m_SpawnedThreadActiveFlag[WorkUnitID] = 0;
  m_SpawnedThreadActiveFlagLock[WorkUnitID]->unlock();

  pthread_join(m_SpawnedThreadProcessID[WorkUnitID], nullptr);

  m_SpawnedThreadActiveFlagLock[WorkUnitID] = nullptr;
  m_SpawnedThreadActiveFlagLock[WorkUnitID] = nullptr;
}
#endif

void
PlatformMultiThreader::SpawnWaitForSingleMethodThread(ThreadProcessIdType threadHandle)
{
  // Using POSIX threads
  if (pthread_join(threadHandle, nullptr))
  {
    itkExceptionMacro(<< "Unable to join thread.");
  }
}

ThreadProcessIdType
PlatformMultiThreader::SpawnDispatchSingleMethodThread(PlatformMultiThreader::WorkUnitInfo * threadInfo)
{
  // Using POSIX threads
  pthread_attr_t attr;
  pthread_t      threadHandle;

  pthread_attr_init(&attr);
#if !defined(__CYGWIN__)
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);
#endif

  int threadError;
  threadError = pthread_create(
    &threadHandle, &attr, reinterpret_cast<c_void_cast>(this->SingleMethodProxy), reinterpret_cast<void *>(threadInfo));
  if (threadError != 0)
  {
    itkExceptionMacro(<< "Unable to create a thread.  pthread_create() returned " << threadError);
  }
  return threadHandle;
}

} // end namespace itk
