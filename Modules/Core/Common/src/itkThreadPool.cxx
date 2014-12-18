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


#include "itkThreadPool.h"

#include <algorithm>

namespace itk
{
SimpleFastMutexLock ThreadPool::m_ThreadProcessIdentifiersVectorMutex;
SimpleFastMutexLock ThreadPool::m_ThreadPoolInstanceMutex;

ThreadPool::Pointer ThreadPool::m_ThreadPoolInstance;


ThreadPool::Pointer
ThreadPool
::New()
{
  return Self::GetInstance();
}


ThreadPool::Pointer
ThreadPool
::GetInstance()
{
  MutexLockHolder<SimpleFastMutexLock> mutexHolderAct(m_ThreadPoolInstanceMutex);
  if( m_ThreadPoolInstance.IsNull() )
    {
    // Try the factory first
    m_ThreadPoolInstance  = ObjectFactory< Self >::Create();
    // if the factory did not provide one, then create it here
    if ( m_ThreadPoolInstance.IsNull() )
      {
      m_ThreadPoolInstance = new ThreadPool();
      // Remove extra reference from construction.
      m_ThreadPoolInstance->UnRegister();
      }
    }
    return m_ThreadPoolInstance;
}

ThreadPool
::ThreadPool() :
  m_ScheduleForDestruction(false),
  m_ThreadCount(0),
  m_IdCounter(1),
  m_ExceptionOccurred(false)
{
}

void ThreadPool
::InitializeThreads(unsigned int maximumThreads)
{
  if( maximumThreads == 0 )
    {
    maximumThreads = 1;
    }
  for( unsigned int i = 0; i < maximumThreads; ++i )
    {
    AddThread();
    }
}

ThreadPool
::~ThreadPool()
{
  itkDebugMacro(<< std::endl << "Thread pool being destroyed" << std::endl);
}

/*
  Once wait thread is called we will look for the threadhandle's job id
  if it is JOB_THREADHANDLE_IS_FREE it means it is done -
  if it is +ve it means it is running

  if it is JOB_THREADHANDLE_IS_FREE / done set the jid to
  JOB_THREADHANDLE_IS_DONE which means that now assign work can
  assign further work to this thread
  */
bool
ThreadPool
::WaitForJobOnThreadHandle(ThreadProcessIdType threadHandle)
{
  try
    {
    itkDebugMacro(<< "Waiting for thread with threadHandle " << threadHandle);

    if(GetSemaphoreForThreadWait(threadHandle)->SemaphoreWait() != 0 )
      {
      itkExceptionMacro(<<"Error in semaphore wait");
      }

    MutexLockHolder<SimpleFastMutexLock> threadStructMutexHolder(m_ThreadProcessIdentifiersVectorMutex);
    ThreadProcessIdentifiersVecType::const_iterator tpIter = m_ThreadProcessIdentifiersVector.begin();
    ThreadProcessIdentifiersVecType::const_iterator end = m_ThreadProcessIdentifiersVector.end();
    for(;tpIter != end; ++tpIter)
      {
      if(CompareThreadHandles(tpIter->m_ThreadProcessHandle, threadHandle))
        {
        if(tpIter->m_ThreadNumericId == JOB_THREADHANDLE_IS_FREE)
          {
          itkDebugMacro(<< "Wait ended for thread with threadHandle " << threadHandle);
          break;
          }
        }
      }
    if(tpIter == end)
      {
      itkExceptionMacro(<< "can't find ThreadIdHandlePairing object for thread handle"
                        << threadHandle);
      }
    }
  catch( std::exception & itkDebugStatement( e ) )
    {
    m_ExceptionOccurred = true;
    itkDebugMacro(<< "Exception occured while waiting for thread with threadHandle : "
                  << threadHandle << std::endl << e.what());
    }
  return true;
}

void
ThreadPool
::RemoveActiveId(int id)
{
  try
    {
    MutexLockHolder<SimpleFastMutexLock> vMutex(m_ThreadProcessIdentifiersVectorMutex);
    // setting ThreadJobStruct
    for(ThreadProcessIdentifiersVecType::iterator it =
          m_ThreadProcessIdentifiersVector.begin(),
          end = m_ThreadProcessIdentifiersVector.end();
        it != end; ++it)
      {
      if( it->m_ThreadNumericId == id )
        {
        // set this to JOB_THREADHANDLE_IS_DONE so that now the thread
        // is in wait state
        it->m_ThreadNumericId = JOB_THREADHANDLE_IS_FREE;
        break;
        }
      }

    // Delete from worker queue
    ThreadJobContainerType::iterator newJob = this->m_WorkerQueue.find(id);
    if(newJob == this->m_WorkerQueue.end())
      {
      itkExceptionMacro(<< "Error occured, couldnt find id in WorkerQueue to mark executed. Id is : "
                        << id << std::endl );
      }
    this->m_WorkerQueue.erase(newJob);
    }
  catch( std::exception & e )
    {
    itkExceptionMacro(<< e.what() );
    }
}

ThreadPool::ThreadSemaphorePair*
ThreadPool
::GetSemaphore(ThreadSemHandlePairingQueueType &q, ThreadProcessIdType threadHandle)
{
  MutexLockHolder<SimpleFastMutexLock> threadStructMutexHolder(m_ThreadProcessIdentifiersVectorMutex);
  ThreadSemHandlePairingQueueType::iterator it = q.begin();
  ThreadSemHandlePairingQueueType::iterator end = q.end();
  for(; it != end; ++it)
    {
    if(CompareThreadHandles((*it)->m_ThreadProcessHandle, threadHandle))
      {
      break;
      }
    }
  if(it == end)
    {
    itkExceptionMacro(<< "Error occured finding semaphore for thread handle " << threadHandle);
    }
  return *it;

}

ThreadProcessIdType
ThreadPool::
GetThreadHandleForThreadId(ThreadIdType id)
{
  MutexLockHolder<SimpleFastMutexLock> threadStructMutexHolder(m_ThreadProcessIdentifiersVectorMutex);
  for (ThreadProcessIdentifiersVecType::iterator tpIt = m_ThreadProcessIdentifiersVector.begin(),
         end = m_ThreadProcessIdentifiersVector.end(); tpIt != end; ++tpIt)
    {
    if (tpIt->m_ThreadNumericId)
      {
      return tpIt->m_ThreadProcessHandle;
      }
    }
  itkExceptionMacro(<< "Error occured finding thread handle for thread id " << id);
}

ThreadPool::ThreadSemaphorePair *
ThreadPool
::GetSemaphoreForThreadWait(ThreadProcessIdType threadHandle)
{
  return this->GetSemaphore(this->m_ThreadSemHandlePairingForWaitQueue, threadHandle);
}

ThreadPool::ThreadSemaphorePair *
ThreadPool
::GetSemaphoreForThread(ThreadProcessIdType threadHandle)
{
  return this->GetSemaphore(m_ThreadSemHandlePairingQueue,threadHandle);
}

// TODO: Do we want to return a reference to the job in the work queue
//      This is returning a copy of the ThreadJob from the Queue rather
//      than a reference to the the job on the Queue.
const ThreadJob &
ThreadPool
::FetchWork(ThreadProcessIdType threadHandle)
{
  int  workId = JOB_THREADHANDLE_IS_DONE;

  ThreadJobContainerType::iterator newJob;
  if(GetSemaphoreForThread(threadHandle)->SemaphoreWait() != 0 )
    {
    itkExceptionMacro(<<"Error in semaphore wait");
    }

  {
  MutexLockHolder<SimpleFastMutexLock> threadStructMutexHolder(m_ThreadProcessIdentifiersVectorMutex);

  ThreadProcessIdentifiersVecType::const_iterator tpIter = m_ThreadProcessIdentifiersVector.begin();
  ThreadProcessIdentifiersVecType::iterator end = m_ThreadProcessIdentifiersVector.end();
  for(; tpIter != end; ++tpIter)
    {
    if(CompareThreadHandles(tpIter->m_ThreadProcessHandle,threadHandle))
      {
      break;
      }
    }
  if(tpIter == end)
    {
    itkExceptionMacro(<< "Can't find thread with handle "
                      << threadHandle);
    }

  workId = tpIter->m_ThreadNumericId;
  newJob = this->m_WorkerQueue.find(workId);

  if(newJob != this->m_WorkerQueue.end())
    {
    newJob->second.m_Assigned = true;
    newJob->second.m_Id = workId;
    }
  } // mutex

  if(newJob == this->m_WorkerQueue.end())
    {
    itkExceptionMacro(<< "no job found to run " << std::endl );
    }
  return newJob->second;
}

ThreadPool::ThreadProcessIdentifiers *
ThreadPool
::FindThreadToRun()
{
  ThreadProcessIdentifiers *returnValue(ITK_NULLPTR);
  //
  // loop twice, if necesary.
  // 1st pass, find free thread. if none
  // found, add thread.
  // second pass if thread was added, find just-added thread.
  // if AddThread fails, end outer loop and throw exception.
  for(unsigned j = 0; j < 2 && returnValue == ITK_NULLPTR; ++j)
    {
    for(ThreadProcessIdentifiersVecType::iterator tpIter = m_ThreadProcessIdentifiersVector.begin(),
          end = m_ThreadProcessIdentifiersVector.end();
        tpIter != end; ++tpIter)
      {
      // only if it is JOB_THREADHANDLE_IS_FREE or just added thread you
      // assign it a job
      if( tpIter->m_ThreadNumericId <= JOB_THREADHANDLE_IS_FREE )
        {
        // assign the job id to the thread now
        returnValue = &(*tpIter);
        break;  // break because we dont want to keep assigning
        }
      }
    if(returnValue == ITK_NULLPTR)
      {
      this->AddThread();
      itkDebugMacro(<< "Added a new thread"  );
      }
    }
  if(returnValue == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Unable to find thread to run");
    }
  return returnValue;
}

ThreadProcessIdType
ThreadPool
::AssignWork(ThreadJob threadJob)
{
#if defined(__APPLE__)
  ThreadProcessIdType returnValue = ITK_NULLPTR; // TODO:  This is being returned
#elif defined(_WIN32) || defined(_WIN64)
  ThreadProcessIdType returnValue = NULL; // TODO:  This is being returned
#else
  ThreadProcessIdType returnValue = pthread_self();
#endif
  try
    {
      {
      MutexLockHolder<SimpleFastMutexLock> mutexHolderSync(m_ThreadProcessIdentifiersVectorMutex);
      ThreadProcessIdentifiers *threadToRun = this->FindThreadToRun();

      // adding to queue
      threadJob.m_Id = this->m_IdCounter++;
      ThreadJobContainerPairType toInsert(threadJob.m_Id,threadJob);
      this->m_WorkerQueue.insert(toInsert);

      // now put the job id in the ThreadJobStruct vector
      threadToRun->m_ThreadNumericId = threadJob.m_Id;
      returnValue = threadToRun->m_ThreadProcessHandle;
      }
    if(GetSemaphoreForThread(returnValue)->SemaphorePost() != 0 )
      {
      itkExceptionMacro(<<"Error in semaphore post");
      }
    }
  catch( std::exception& e )
    {
    itkDebugMacro(<< std::endl << "Failed to assign work. \n" << e.what() << std::endl );
    m_ExceptionOccurred = true;
    itkExceptionMacro(<< e.what() );
    }
  return returnValue;
}

}
