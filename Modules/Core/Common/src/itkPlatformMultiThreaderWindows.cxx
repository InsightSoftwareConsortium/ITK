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
#include <cstdlib>

#include "itkWindows.h"
#include <process.h>

namespace itk
{
#if !defined(ITK_LEGACY_REMOVE)
void
PlatformMultiThreader::MultipleMethodExecute()
{
  ThreadIdType threadCount;

  DWORD  threadId;
  HANDLE processId[ITK_MAX_THREADS];

  // obey the global maximum number of threads limit
  if (m_NumberOfWorkUnits > MultiThreaderBase::GetGlobalMaximumNumberOfThreads())
  {
    m_NumberOfWorkUnits = MultiThreaderBase::GetGlobalMaximumNumberOfThreads();
  }
  for (threadCount = 0; threadCount < m_NumberOfWorkUnits; ++threadCount)
  {
    if (m_MultipleMethod[threadCount] == nullptr)
    {
      itkExceptionMacro(<< "No multiple method set for: " << threadCount);
    }
  }
  // Using _beginthreadex on a PC
  //
  // We want to use _beginthreadex to start m_NumberOfWorkUnits - 1
  // additional threads which will be used to call the NumberOfWorkUnits-1
  // methods defined in this->MultipleMethods[](). The parent thread
  // will call m_MultipleMethods[NumberOfWorkUnits-1]().  When it is done,
  // it will wait for all the children to finish.
  //
  // First, start up the m_NumberOfWorkUnits-1 processes.  Keep track
  // of their process ids for use later in the waitid call
  for (threadCount = 1; threadCount < m_NumberOfWorkUnits; ++threadCount)
  {
    m_ThreadInfoArray[threadCount].UserData = m_MultipleData[threadCount];
    m_ThreadInfoArray[threadCount].NumberOfWorkUnits = m_NumberOfWorkUnits;

    processId[threadCount] = reinterpret_cast<HANDLE>(_beginthreadex(
      nullptr, 0, m_MultipleMethod[threadCount], &m_ThreadInfoArray[threadCount], 0, (unsigned int *)&threadId));

    if (processId[threadCount] == nullptr)
    {
      itkExceptionMacro("Error in thread creation!");
    }
  }

  // Now, the parent thread calls the last method itself
  m_ThreadInfoArray[0].UserData = m_MultipleData[0];
  m_ThreadInfoArray[0].NumberOfWorkUnits = m_NumberOfWorkUnits;
  (m_MultipleMethod[0])((void *)(&m_ThreadInfoArray[0]));
  // The parent thread has finished its method - so now it
  // waits for each of the other processes to
  // exit
  for (threadCount = 1; threadCount < m_NumberOfWorkUnits; ++threadCount)
  {
    WaitForSingleObject(processId[threadCount], INFINITE);
  }
  // close the threads
  for (threadCount = 1; threadCount < m_NumberOfWorkUnits; ++threadCount)
  {
    CloseHandle(processId[threadCount]);
  }
}

ThreadIdType
PlatformMultiThreader::SpawnThread(ThreadFunctionType f, void * UserData)
{
  ThreadIdType id = 0;

  DWORD threadId;

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

  // Using _beginthreadex on a PC
  //
  m_SpawnedThreadProcessID[id] = reinterpret_cast<HANDLE>(
    _beginthreadex(nullptr, 0, f, &m_SpawnedThreadInfoArray[id], 0, (unsigned int *)&threadId));
  if (m_SpawnedThreadProcessID[id] == nullptr)
  {
    itkExceptionMacro("Error in thread creation !!!");
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

  WaitForSingleObject(m_SpawnedThreadProcessID[WorkUnitID], INFINITE);
  CloseHandle(m_SpawnedThreadProcessID[WorkUnitID]);
  m_SpawnedThreadActiveFlagLock[WorkUnitID] = nullptr;
}
#endif

void
PlatformMultiThreader::SpawnWaitForSingleMethodThread(ThreadProcessIdType threadHandle)
{
  // Using _beginthreadex on a PC
  WaitForSingleObject(threadHandle, INFINITE);
  CloseHandle(threadHandle);
}

ThreadProcessIdType
PlatformMultiThreader::SpawnDispatchSingleMethodThread(PlatformMultiThreader::WorkUnitInfo * threadInfo)
{
  // Using _beginthreadex on a PC
  DWORD threadId;
  auto  threadHandle = reinterpret_cast<HANDLE>(
    _beginthreadex(nullptr, 0, this->SingleMethodProxy, threadInfo, 0, (unsigned int *)&threadId));
  if (threadHandle == nullptr)
  {
    itkExceptionMacro("Error in thread creation !!!");
  }
  return threadHandle;
}

} // end namespace itk
