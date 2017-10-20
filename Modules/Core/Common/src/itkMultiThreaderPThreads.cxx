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
#include <unistd.h>

#ifdef __APPLE__
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

namespace itk
{
extern "C"
{
typedef void *( *c_void_cast )(void *);
}

void MultiThreader::MultipleMethodExecute()
{

  pthread_t process_id[ITK_MAX_THREADS];

  // obey the global maximum number of threads limit
  if( m_NumberOfThreads > m_GlobalMaximumNumberOfThreads )
    {
    m_NumberOfThreads = m_GlobalMaximumNumberOfThreads;
    }
  for( ThreadIdType thread_loop = 0; thread_loop < m_NumberOfThreads; ++thread_loop )
    {
    if( m_MultipleMethod[thread_loop] == (ThreadFunctionType)ITK_NULLPTR )
      {
      itkExceptionMacro(<< "No multiple method set for: " << thread_loop);
      return;
      }
    }

  // Using POSIX threads
  //
  // We want to use pthread_create to start m_NumberOfThreads - 1
  // additional
  // threads which will be used to call the NumberOfThreads-1 methods
  // defined in m_MultipleMethods[](). The parent thread
  // will call m_MultipleMethods[NumberOfThreads-1]().  When it is done,
  // it will wait for all the children to finish.
  //
  // First, start up the m_NumberOfThreads-1 processes.  Keep track
  // of their process ids for use later in the pthread_join call

  pthread_attr_t attr;

  pthread_attr_init(&attr);
#ifndef __CYGWIN__
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_PROCESS);
#endif
  for( ThreadIdType thread_loop = 1; thread_loop < m_NumberOfThreads; ++thread_loop )
    {
    m_ThreadInfoArray[thread_loop].UserData =
      m_MultipleData[thread_loop];
    m_ThreadInfoArray[thread_loop].NumberOfThreads = m_NumberOfThreads;
    int threadError = pthread_create( &( process_id[thread_loop] ),
                                      &attr, reinterpret_cast<c_void_cast>( m_MultipleMethod[thread_loop] ),
                                      ( (void *)( &m_ThreadInfoArray[thread_loop] ) ) );
    if( threadError != 0 )
      {
      itkExceptionMacro(<< "Unable to create a thread.  pthread_create() returned "
                        << threadError);
      }
    }

  // Now, the parent thread calls the last method itself
  m_ThreadInfoArray[0].UserData = m_MultipleData[0];
  m_ThreadInfoArray[0].NumberOfThreads = m_NumberOfThreads;
  ( m_MultipleMethod[0] )( (void *)( &m_ThreadInfoArray[0] ) );
  // The parent thread has finished its method - so now it
  // waits for each of the other processes to exit
  for( ThreadIdType thread_loop = 1; thread_loop < m_NumberOfThreads; ++thread_loop )
    {
    pthread_join(process_id[thread_loop], ITK_NULLPTR);
    }

}

ThreadIdType MultiThreader::SpawnThread(ThreadFunctionType f, void *UserData)
{
  ThreadIdType id = 0;

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

  pthread_attr_t attr;

  pthread_attr_init(&attr);
#ifndef __CYGWIN__
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_PROCESS);
#endif

  int threadError = pthread_create( &( m_SpawnedThreadProcessID[id] ),
                                    &attr, reinterpret_cast<c_void_cast>( f ),
                                    ( (void *)( &m_SpawnedThreadInfoArray[id] ) ) );
  if( threadError != 0 )
    {
    itkExceptionMacro(<< "Unable to create a thread.  pthread_create() returned "
                      << threadError);
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

  pthread_join(m_SpawnedThreadProcessID[ThreadID], ITK_NULLPTR);

  m_SpawnedThreadActiveFlagLock[ThreadID] = ITK_NULLPTR;
  m_SpawnedThreadActiveFlagLock[ThreadID] = ITK_NULLPTR;
}

void
MultiThreader
::SpawnWaitForSingleMethodThread(ThreadProcessIdType threadHandle)
{
  // Using POSIX threads
  if ( pthread_join(threadHandle, ITK_NULLPTR) )
    {
    itkExceptionMacro(<< "Unable to join thread.");
    }
}

ThreadProcessIdType
MultiThreader
::SpawnDispatchSingleMethodThread(MultiThreader::ThreadInfoStruct *threadInfo)
{
  // Using POSIX threads
  pthread_attr_t attr;
  pthread_t      threadHandle;

  pthread_attr_init(&attr);
#if !defined( __CYGWIN__ )
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);
#endif

  int threadError;
  threadError =
    pthread_create( &threadHandle, &attr, reinterpret_cast< c_void_cast >( this->SingleMethodProxy ),
                    reinterpret_cast< void * >( threadInfo ) );
  if ( threadError != 0 )
    {
    itkExceptionMacro(<< "Unable to create a thread.  pthread_create() returned "
                      << threadError);
    }
  return threadHandle;
}

} // end namespace itk
