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

namespace itk
{
ThreadIdType MultiThreader::GetGlobalDefaultNumberOfThreadsByPlatform()
{
  int num;
  // If we are not multithreading, the number of threads should
  // always be 1
  num = 1;
  return num;
}

void MultiThreader::MultipleMethodExecute()
{
  ThreadIdType thread_loop;

  // obey the global maximum number of threads limit
  if ( m_NumberOfThreads > m_GlobalMaximumNumberOfThreads )
    {
    m_NumberOfThreads = m_GlobalMaximumNumberOfThreads;
    }

  for ( thread_loop = 0; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
    if ( m_MultipleMethod[thread_loop] == (ThreadFunctionType)0 )
      {
      itkExceptionMacro(<< "No multiple method set for: " << thread_loop);
      return;
      }
    }

  // There is no multi threading, so there is only one thread.
  m_ThreadInfoArray[0].UserData    = m_MultipleData[0];
  m_ThreadInfoArray[0].NumberOfThreads = m_NumberOfThreads;
  ( m_MultipleMethod[0] )( (void *)( &m_ThreadInfoArray[0] ) );
}

ThreadIdType MultiThreader::SpawnThread(ThreadFunctionType f, void *UserData)
{
  int id = 0;

  while ( id < ITK_MAX_THREADS )
    {
    if ( !m_SpawnedThreadActiveFlagLock[id]  )
      {
      m_SpawnedThreadActiveFlagLock[id] = MutexLock::New();
      }
    m_SpawnedThreadActiveFlagLock[id]->Lock();
    if ( m_SpawnedThreadActiveFlag[id] == 0 )
      {
      // We've got a useable thread id, so grab it
      m_SpawnedThreadActiveFlag[id] = 1;
      m_SpawnedThreadActiveFlagLock[id]->Unlock();
      break;
      }
    m_SpawnedThreadActiveFlagLock[id]->Unlock();

    id++;
    }

  if ( id >= ITK_MAX_THREADS )
    {
    itkExceptionMacro(<< "You have too many active threads!");
    }

  m_SpawnedThreadInfoArray[id].UserData        = UserData;
  m_SpawnedThreadInfoArray[id].NumberOfThreads = 1;
  m_SpawnedThreadInfoArray[id].ActiveFlag = &m_SpawnedThreadActiveFlag[id];
  m_SpawnedThreadInfoArray[id].ActiveFlagLock = m_SpawnedThreadActiveFlagLock[id];

  // There is no multi threading, so there is only one thread.
  // This won't work - so give an error message.
  itkExceptionMacro(<< "Cannot spawn thread in a single threaded environment!");
  m_SpawnedThreadActiveFlagLock[id] = 0;
  id = -1;
  return id;
}

void MultiThreader::TerminateThread(ThreadIdType ThreadID)
{
  if ( !m_SpawnedThreadActiveFlag[ThreadID] )
    {
    return;
    }

  m_SpawnedThreadActiveFlagLock[ThreadID]->Lock();
  m_SpawnedThreadActiveFlag[ThreadID] = 0;
  m_SpawnedThreadActiveFlagLock[ThreadID]->Unlock();

  // There is no multi threading, so there is only one thread.
  // This won't work - so give an error message.
  itkExceptionMacro(<< "Cannot terminate thread in single threaded environment!");

  m_SpawnedThreadActiveFlagLock[ThreadID] = 0;
  m_SpawnedThreadActiveFlagLock[ThreadID] = 0;
}

void
MultiThreader
::ThreadPoolWaitForSingleMethodThread(ThreadProcessIdType threadHandle)
{
  // No threading library specified.  Do nothing.  No joining or waiting
  // necessary.
}

ThreadProcessIdType
MultiThreader
::ThreadPoolDispatchSingleMethodThread(MultiThreader::ThreadInfoStruct *threadInfo)
{
  // No threading library specified.  Do nothing.  The computation
  // will be run by the main execution thread.
}
void
MultiThreader
::SpawnWaitForSingleMethodThread(ThreadProcessIdType threadHandle)
{
  // No threading library specified.  Do nothing.  No joining or waiting
  // necessary.
}

ThreadProcessIdType
MultiThreader
::SpawnDispatchSingleMethodThread(MultiThreader::ThreadInfoStruct *threadInfo)
{
  // No threading library specified.  Do nothing.  The computation
  // will be run by the main execution thread.
}
} // end namespace itk
