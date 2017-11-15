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
#include "itksys/SystemTools.hxx"

#include <algorithm>

namespace itk
{
SimpleFastMutexLock ThreadPool::m_Mutex;

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
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_Mutex);
  if( m_ThreadPoolInstance.IsNull() )
    {
    m_ThreadPoolInstance  = ObjectFactory< Self >::Create();
    if ( m_ThreadPoolInstance.IsNull() )
      {
      new ThreadPool(); //constructor sets m_ThreadPoolInstance
      }
    }
  return m_ThreadPoolInstance;
}

ThreadPool
::ThreadPool() :
  m_ExceptionOccurred(false)
{
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_Mutex);
  m_ThreadPoolInstance = this; //threads need this
  m_ThreadPoolInstance->UnRegister(); // Remove extra reference
  PlatformCreate(m_ThreadsSemaphore);
}

ThreadIdType
ThreadPool
::GetGlobalDefaultNumberOfThreads()
{
  ThreadIdType threadCount = 0;
  /* The ITK_NUMBER_OF_THREADS_ENV_LIST contains is an
   * environmental variable that holds a ':' separated
   * list of environmental variables that whould be
   * queried in order for setting the m_GlobalMaximumNumberOfThreads.
   *
   * This is intended to be a mechanism suitable to easy
   * runtime modification to ease using the proper number
   * of threads for load balancing batch processing
   * systems where the number of threads
   * authorized for use may be less than the number
   * of physical processors on the computer.
   *
   * This list contains the Sun|Oracle Grid Engine
   * environmental variable "NSLOTS" by default
   */
  std::vector<std::string> ITK_NUMBER_OF_THREADS_ENV_LIST;
  std::string       itkNumberOfThreadsEvnListString = "";
  if( itksys::SystemTools::GetEnv("ITK_NUMBER_OF_THREADS_ENV_LIST",
                                  itkNumberOfThreadsEvnListString) )
    {
    // NOTE: We always put "ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS" at the end
    // unconditionally.
    itkNumberOfThreadsEvnListString += ":ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS";
    }
  else
    {
    itkNumberOfThreadsEvnListString = "NSLOTS:ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS";
    }
    {
    std::stringstream numberOfThreadsEnvListStream(itkNumberOfThreadsEvnListString);
    std::string       item;
    while( std::getline(numberOfThreadsEnvListStream, item, ':') )
      {
      if( item.size() > 0 ) // Do not add empty items.
        {
        ITK_NUMBER_OF_THREADS_ENV_LIST.push_back(item);
        }
      }
    }
  // first, check for environment variable
  std::string itkGlobalDefaultNumberOfThreadsEnv = "0";
  for( std::vector<std::string>::const_iterator lit = ITK_NUMBER_OF_THREADS_ENV_LIST.begin();
       lit != ITK_NUMBER_OF_THREADS_ENV_LIST.end();
       ++lit )
    {
    if( itksys::SystemTools::GetEnv(lit->c_str(), itkGlobalDefaultNumberOfThreadsEnv) )
      {
      threadCount = static_cast<ThreadIdType>( atoi( itkGlobalDefaultNumberOfThreadsEnv.c_str() ) );
      }
    }

  // otherwise, set number of threads based on system information
  if( threadCount <= 0 )
    {
    threadCount = GetGlobalDefaultNumberOfThreadsByPlatform();
    }

  // limit the number of threads to m_GlobalMaximumNumberOfThreads
  threadCount  = std::min( threadCount, ThreadIdType(ITK_MAX_THREADS) );

  // verify that the default number of threads is larger than zero
  threadCount  = std::max( threadCount, NumericTraits<ThreadIdType>::OneValue() );

  return threadCount;
}

void
ThreadPool
::AddThreads(ThreadIdType count)
{
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_Mutex);
  m_Threads.reserve(m_Threads.size() + count);
  for( unsigned int i = 0; i < count; ++i )
    {
    AddThread();
    }
}

void
ThreadPool
::DeleteThreads()
{
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_Mutex);
  for (size_t i = 0; i < m_Threads.size(); i++)
    {
    if (!PlatformClose(m_Threads[i]))
      {
      m_ExceptionOccurred = true;
      }
    }
}

int
ThreadPool
::GetNumberOfCurrentlyIdleThreads() const
{
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_Mutex);
  if ( m_Threads.empty() ) //not yet initialized
    {
    const_cast<ThreadPool *>(this)->AddThreads(ThreadPool::GetGlobalDefaultNumberOfThreads());
    }
  return int(m_Threads.size()) - int(m_WorkQueue.size()); // lousy approximation
}

ITK_THREAD_RETURN_TYPE
noOperation(void *)
{
  return ITK_THREAD_RETURN_VALUE;
}

ThreadPool
::~ThreadPool()
{
  bool waitForThreads = true;
#if defined(WIN32) && defined(ITKCommon_EXPORTS)
  //this destructor is called during DllMain's DLL_PROCESS_DETACH.
  //Because ITKCommon-4.X.dll is usually being detached due to process termination,
  //lpvReserved is non-NULL meaning that "all threads in the process
  //except the current thread either have exited already or have been
  //explicitly terminated by a call to the ExitProcess function".
  if ( !m_Threads.empty() ) //thread pool was used
    {
    DWORD dwWaitResult = WaitForSingleObject(m_Threads[0], 1);
    if (dwWaitResult == WAIT_OBJECT_0) //thread has finished
      {
      waitForThreads = false;
      }
    }
#endif

  if (waitForThreads) //add dummy jobs for clean thread exit
    {
    std::vector<Semaphore> jobSem(m_Threads.size());
    for (ThreadIdType i = 0; i < m_Threads.size(); i++)
      {
      ThreadJob dummy;
      dummy.m_ThreadFunction = &noOperation;
      dummy.m_Semaphore = &jobSem[i];
      dummy.m_UserData = ITK_NULLPTR; //makes dummy jobs easier to spot while debugging
      AddWork(dummy);
      }

    for (ThreadIdType i = 0; i < m_Threads.size(); i++)
      {
      WaitForJob(jobSem[i]);
      }
    }

  DeleteThreads();
  PlatformDelete(m_ThreadsSemaphore);
}

void
ThreadPool
::WaitForJob(Semaphore& jobSemaphore)
{
  PlatformWait(jobSemaphore);
  PlatformDelete(jobSemaphore);
}

void
ThreadPool
::AddWork(const ThreadJob& threadJob)
{
  {
    MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_Mutex);
    if ( m_Threads.empty() ) //first job
      {
      AddThreads(ThreadPool::GetGlobalDefaultNumberOfThreads());
      }
    m_WorkQueue.push_back(threadJob);
  }

  PlatformCreate(*threadJob.m_Semaphore);
  PlatformSignal(m_ThreadsSemaphore);
}


ITK_THREAD_RETURN_TYPE
ThreadPool
::ThreadExecute(void *)
{
  //plain pointer does not increase reference count
  ThreadPool* threadPool = m_ThreadPoolInstance.GetPointer();
  try
    {
    while (true)
      {
      threadPool->PlatformWait(threadPool->m_ThreadsSemaphore);

      ThreadJob job;
      {
      MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_Mutex);
      if (threadPool->m_WorkQueue.empty()) //another thread stole work meant for me
        {
        itkGenericExceptionMacro(<< "Work queue is empty!");
        }
      job = threadPool->m_WorkQueue.front();
      threadPool->m_WorkQueue.pop_front();
      } //releases the lock

      if (job.m_ThreadFunction == &noOperation)
        {
        PlatformSignal(*job.m_Semaphore);
        break; //exit infinite while loop
        }
      job.m_ThreadFunction(job.m_UserData); //execute the job, lock has been released
      PlatformSignal(*job.m_Semaphore);
      }
    }
  catch (ExceptionObject& exc)
    {
    std::cerr << exc << std::endl;
    threadPool->m_ExceptionOccurred = true;
    throw;
    }
  return ITK_THREAD_RETURN_VALUE;
}

}
