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
#include "itkMultiThreader.h"
#include "itkObjectFactory.h"
#include "itksys/SystemTools.hxx"
#include <stdlib.h>

#include "itkWindows.h"
#include <process.h>

namespace itk
{
void MultiThreader::MultipleMethodExecute()
{
  ThreadIdType threadCount;

  DWORD  threadId;
  HANDLE processId[ITK_MAX_THREADS];

  // obey the global maximum number of threads limit
  if( m_NumberOfThreads > m_GlobalMaximumNumberOfThreads )
    {
    m_NumberOfThreads = m_GlobalMaximumNumberOfThreads;
    }
  for( threadCount = 0; threadCount < m_NumberOfThreads; ++threadCount )
    {
    if( m_MultipleMethod[threadCount] == (ThreadFunctionType)0 )
      {
      itkExceptionMacro(<< "No multiple method set for: " << threadCount);
      return;
      }
    }
  // Using _beginthreadex on a PC
  //
  // We want to use _beginthreadex to start m_NumberOfThreads - 1
  // additional threads which will be used to call the NumberOfThreads-1
  // methods defined in this->MultipleMethods[](). The parent thread
  // will call m_MultipleMethods[NumberOfThreads-1]().  When it is done,
  // it will wait for all the children to finish.
  //
  // First, start up the m_NumberOfThreads-1 processes.  Keep track
  // of their process ids for use later in the waitid call
  for( threadCount = 1; threadCount < m_NumberOfThreads; ++threadCount )
    {
    m_ThreadInfoArray[threadCount].UserData =
      m_MultipleData[threadCount];
    m_ThreadInfoArray[threadCount].NumberOfThreads = m_NumberOfThreads;

    processId[threadCount] = (void *)
      _beginthreadex(0, 0,
                     m_MultipleMethod[threadCount],
                     &m_ThreadInfoArray[threadCount], 0,
                     (unsigned int *)&threadId);

    if( processId[threadCount] == ITK_NULLPTR )
      {
      itkExceptionMacro("Error in thread creation!");
      }
    }

  // Now, the parent thread calls the last method itself
  m_ThreadInfoArray[0].UserData = m_MultipleData[0];
  m_ThreadInfoArray[0].NumberOfThreads = m_NumberOfThreads;
  ( m_MultipleMethod[0] )( (void *)( &m_ThreadInfoArray[0] ) );
  // The parent thread has finished its method - so now it
  // waits for each of the other processes to
  // exit
  for( threadCount = 1; threadCount < m_NumberOfThreads; ++threadCount )
    {
    WaitForSingleObject(processId[threadCount], INFINITE);
    }
  // close the threads
  for( threadCount = 1; threadCount < m_NumberOfThreads; ++threadCount )
    {
    CloseHandle(processId[threadCount]);
    }
}

ThreadIdType MultiThreader::SpawnThread(ThreadFunctionType f, void *UserData)
{
  ThreadIdType id = 0;

  DWORD threadId;

  while( id < ITK_MAX_THREADS )
    {
    if( !m_SpawnedThreadActiveFlagLock[id]  )
      {
      m_SpawnedThreadActiveFlagLock[id] = MutexLock::New();
      }
    m_SpawnedThreadActiveFlagLock[id]->Lock();
    if( m_SpawnedThreadActiveFlag[id] == 0 )
      {
      // We've got a useable thread id, so grab it
      m_SpawnedThreadActiveFlag[id] = 1;
      m_SpawnedThreadActiveFlagLock[id]->Unlock();
      break;
      }
    m_SpawnedThreadActiveFlagLock[id]->Unlock();

    id++;
    }

  if( id >= ITK_MAX_THREADS )
    {
    itkExceptionMacro(<< "You have too many active threads!");
    }

  m_SpawnedThreadInfoArray[id].UserData        = UserData;
  m_SpawnedThreadInfoArray[id].NumberOfThreads = 1;
  m_SpawnedThreadInfoArray[id].ActiveFlag = &m_SpawnedThreadActiveFlag[id];
  m_SpawnedThreadInfoArray[id].ActiveFlagLock = m_SpawnedThreadActiveFlagLock[id];

  // Using _beginthreadex on a PC
  //
  m_SpawnedThreadProcessID[id] = (void *)
    _beginthreadex(0, 0, f,
                   &m_SpawnedThreadInfoArray[id], 0,
                   (unsigned int *)&threadId);
  if( m_SpawnedThreadProcessID[id] == 0 )
    {
    itkExceptionMacro("Error in thread creation !!!");
    }
  return id;
}

void MultiThreader::TerminateThread(ThreadIdType ThreadID)
{
  if( !m_SpawnedThreadActiveFlag[ThreadID] )
    {
    return;
    }

  m_SpawnedThreadActiveFlagLock[ThreadID]->Lock();
  m_SpawnedThreadActiveFlag[ThreadID] = 0;
  m_SpawnedThreadActiveFlagLock[ThreadID]->Unlock();

  WaitForSingleObject(m_SpawnedThreadProcessID[ThreadID], INFINITE);
  CloseHandle(m_SpawnedThreadProcessID[ThreadID]);
  m_SpawnedThreadActiveFlagLock[ThreadID] = 0;
}

void
MultiThreader
::SpawnWaitForSingleMethodThread(ThreadProcessIdType threadHandle)
{
  // Using _beginthreadex on a PC
  WaitForSingleObject(threadHandle, INFINITE);
  CloseHandle(threadHandle);
}

ThreadProcessIdType
MultiThreader
::SpawnDispatchSingleMethodThread(MultiThreader::ThreadInfoStruct *threadInfo)
{
  // Using _beginthreadex on a PC
  DWORD  threadId;
  HANDLE threadHandle =  (HANDLE)_beginthreadex(0, 0,
                                                this->SingleMethodProxy,
                                                threadInfo, 0, (unsigned int *)&threadId);
  if ( threadHandle == ITK_NULLPTR )
    {
    itkExceptionMacro("Error in thread creation !!!");
    }
  return threadHandle;
}

} // end namespace itk
