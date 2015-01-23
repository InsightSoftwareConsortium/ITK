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

namespace itk
{

ThreadPool
::ThreadSemaphorePair
::ThreadSemaphorePair(const ThreadProcessIdType & tph) :
  m_ThreadProcessHandle(tph)
{

  m_Semaphore = CreateSemaphore(
    ITK_NULLPTR,     // default security attributes
    0,        // initial count
    1000, // maximum count
    ITK_NULLPTR);    // unnamed semaphore
  if (m_Semaphore == ITK_NULLPTR)
    {
    itkGenericExceptionMacro(<< "CreateSemaphore error" << GetLastError());
    }
}


int
ThreadPool
::ThreadSemaphorePair
::SemaphoreWait()
{
  DWORD dwWaitResult = WaitForSingleObject(m_Semaphore,       // handle to semaphore
                                           INFINITE);
  switch( dwWaitResult )
    {
    // The thread got ownership of the mutex
    case WAIT_OBJECT_0:
      return 0; //success
      break;
    case WAIT_ABANDONED:
      return -1;
    case WAIT_FAILED:
      return -1;
    default:
      return -1;
    }
}

int
ThreadPool
::ThreadSemaphorePair::SemaphorePost()
{
  if(!ReleaseSemaphore(
       m_Semaphore,   // handle to semaphore
       1,               // increase count by one
       ITK_NULLPTR))
    {
    return -1;
    }
  return 0;
}


void
ThreadPool
::AddThread()
{
  m_ThreadCount++;
  HANDLE newlyAddedThreadHandle;
  ThreadProcessIdentifiers::WinThreadIdType  dwThreadId;

  newlyAddedThreadHandle = CreateThread(
    ITK_NULLPTR,
    0,
    (LPTHREAD_START_ROUTINE) ThreadPool::ThreadExecute,     // thread function
    this,
    0,
    &dwThreadId);
  if( newlyAddedThreadHandle == ITK_NULLPTR )
    {
    itkDebugMacro(<< "ERROR; adding thread to thread pool");
    itkExceptionMacro(<< "Cannot create thread.");
    }
  else
    {

    m_ThreadHandles.insert(newlyAddedThreadHandle);
    m_ThreadProcessIdentifiersVector.push_back(ThreadProcessIdentifiers(JOB_THREADHANDLE_JUST_ADDED,
                                                                   newlyAddedThreadHandle,dwThreadId) );

    m_ThreadSemHandlePairingForWaitQueue.push_back(new ThreadSemaphorePair(newlyAddedThreadHandle));
    m_ThreadSemHandlePairingQueue.push_back(new ThreadSemaphorePair(newlyAddedThreadHandle));
    }

}

bool
ThreadPool
::CompareThreadHandles(ThreadProcessIdType t1, ThreadProcessIdType t2)
{
  if (t1 == t2) return true;
  return false;
}

void *
ThreadPool
::ThreadExecute(void *param)
{
  // get the parameters passed inf
  //ThreadPool *winThreadPool = (ThreadPool *)param;
  //ThreadPool::ThreadArgStruct *threadStruct = (ThreadPool::ThreadArgStruct *)param;
  ThreadPool *winThreadPool = (ThreadPool *)param;
  ThreadIdType myId;
  ThreadProcessIdentifiers::WinThreadIdType threadId = GetCurrentThreadId();
  ThreadProcessIdentifiersVecType::iterator tpIter =
    winThreadPool->m_ThreadProcessIdentifiersVector.begin(),
    end = winThreadPool->m_ThreadProcessIdentifiersVector.end();
  bool threadIdFound(false);
  for (; tpIter != end; ++tpIter)
    {
    if (tpIter->m_WinThreadId == threadId)
      {
      myId = tpIter->m_ThreadNumericId;
      threadIdFound = true;
      break;
      }
    }
  if(!threadIdFound)
    {
    std::cerr << "Can't find thread ID " << threadId << std::endl
              << "candidates are:" << std::endl;
    for(tpIter = winThreadPool->m_ThreadProcessIdentifiersVector.begin(); tpIter != end; ++tpIter)
      {
      std::cerr << tpIter->m_WinThreadId << std::endl;
      }
    std::cerr.flush();
    abort();
    }
  HANDLE handle = winThreadPool->GetThreadHandleForThreadId(myId);

  while( !winThreadPool->m_ScheduleForDestruction )
    {
    const ThreadJob &currentWinJob = winThreadPool->FetchWork( handle);
    if( currentWinJob.m_Id < 0 || currentWinJob.m_Assigned == false )
      {
      itkDebugStatement(std::cerr << "In thread pool thread : Empty job returned from FetchWork so ignoring and continuing .."
                        << std::endl);
      continue;
      }
    currentWinJob.m_ThreadFunction(currentWinJob.m_UserData);
    winThreadPool->RemoveActiveId(currentWinJob.m_Id);
    //signal that current job is done
    if(winThreadPool->GetSemaphoreForThreadWait(handle )->SemaphorePost() != 0)
      {
      itkGenericExceptionMacro(<<"******************************Error in semaphore post");
      }
    }
  return 0;
}

}
