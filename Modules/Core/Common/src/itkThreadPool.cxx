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


#include "itkThreadPool.h"
#include "itksys/SystemTools.hxx"
#include "itkThreadSupport.h"
#include "itkNumericTraits.h"
#include "itkMultiThreaderBase.h"
#include "itkSingleton.h"

#include <algorithm>
#include <atomic>
#include <cassert>
#include <mutex>


namespace itk
{

struct ThreadPoolGlobals
{
  ThreadPoolGlobals() = default;

  // To lock on the various internal variables.
  std::mutex m_Mutex;

  // To allow singleton creation of ThreadPool.
  std::once_flag m_ThreadPoolOnceFlag;

  // The singleton instance of ThreadPool.
  ThreadPool::Pointer m_ThreadPoolInstance;

#if defined(_WIN32) && defined(ITKCommon_EXPORTS)
  // ThreadPool's destructor is called during DllMain's DLL_PROCESS_DETACH.
  // Because ITKCommon-5.X.dll is usually being detached due to process termination,
  // lpvReserved is non-NULL meaning that "all threads in the process
  // except the current thread either have exited already or have been
  // explicitly terminated by a call to the ExitProcess function".
  // Therefore we must not wait for the condition_variable.
  std::atomic<bool> m_WaitForThreads{ false };
#else // In a static library, we have to wait.
  std::atomic<bool> m_WaitForThreads{ true };
#endif
};

itkGetGlobalSimpleMacro(ThreadPool, ThreadPoolGlobals, PimplGlobals);

ThreadPool::Pointer
ThreadPool::New()
{
  return Self::GetInstance();
}


ThreadPool::Pointer
ThreadPool::GetInstance()
{
  // This is called once, on-demand to ensure that m_PimplGlobals is
  // initialized.
  itkInitGlobalsMacro(PimplGlobals);

  // Create a singleton ThreadPool.
  std::call_once(m_PimplGlobals->m_ThreadPoolOnceFlag, []() {
    m_PimplGlobals->m_ThreadPoolInstance = ObjectFactory<Self>::Create();
    if (m_PimplGlobals->m_ThreadPoolInstance.IsNull())
    {
      new ThreadPool(); // constructor sets m_PimplGlobals->m_ThreadPoolInstance
    }
#if defined(ITK_USE_PTHREADS)
    pthread_atfork(ThreadPool::PrepareForFork, ThreadPool::ResumeFromFork, ThreadPool::ResumeFromFork);
#endif
  });

  return m_PimplGlobals->m_ThreadPoolInstance;
}

bool
ThreadPool::GetDoNotWaitForThreads()
{
  itkInitGlobalsMacro(PimplGlobals);
  return !m_PimplGlobals->m_WaitForThreads;
}

void
ThreadPool::SetDoNotWaitForThreads(bool doNotWaitForThreads)
{
  itkInitGlobalsMacro(PimplGlobals);
  m_PimplGlobals->m_WaitForThreads = !doNotWaitForThreads;
}

ThreadPool::ThreadPool()
{
  // m_PimplGlobals->m_Mutex not needed to be acquired here because construction only occurs via GetInstance which is
  // protected by call_once.

  m_PimplGlobals->m_ThreadPoolInstance = this;        // threads need this
  m_PimplGlobals->m_ThreadPoolInstance->UnRegister(); // Remove extra reference
  ThreadIdType threadCount = MultiThreaderBase::GetGlobalDefaultNumberOfThreads();
  m_Threads.reserve(threadCount);
  for (ThreadIdType i = 0; i < threadCount; ++i)
  {
    m_Threads.emplace_back(&ThreadPool::ThreadExecute);
  }
}

void
ThreadPool::AddThreads(ThreadIdType count)
{
  std::unique_lock<std::mutex> mutexHolder(m_PimplGlobals->m_Mutex);
  m_Threads.reserve(m_Threads.size() + count);
  for (ThreadIdType i = 0; i < count; ++i)
  {
    m_Threads.emplace_back(&ThreadPool::ThreadExecute);
  }
}

std::mutex &
ThreadPool::GetMutex() const
{
  return m_PimplGlobals->m_Mutex;
}

int
ThreadPool::GetNumberOfCurrentlyIdleThreads() const
{
  std::unique_lock<std::mutex> mutexHolder(m_PimplGlobals->m_Mutex);
  return static_cast<int>(m_Threads.size()) - static_cast<int>(m_WorkQueue.size()); // lousy approximation
}

void
ThreadPool::CleanUp()
{
  bool shouldNotify;
  {
    std::unique_lock<std::mutex> mutexHolder(m_PimplGlobals->m_Mutex);

    this->m_Stopping = true;

    shouldNotify = m_PimplGlobals->m_WaitForThreads && !m_Threads.empty();
  }

  if (shouldNotify)
  {
    m_Condition.notify_all();
  }

  // Even if the threads have already been terminated,
  // we should join() the std::thread variables.
  // Otherwise some sanity check in debug mode complains.
  for (auto & thread : m_Threads)
  {
    assert(thread.joinable());
    thread.join();
  }
}

void
ThreadPool::PrepareForFork()
{
  m_PimplGlobals->m_ThreadPoolInstance->CleanUp();
}

void
ThreadPool::ResumeFromFork()
{
  ThreadPool * instance = m_PimplGlobals->m_ThreadPoolInstance.GetPointer();
  ThreadIdType threadCount = instance->m_Threads.size();
  instance->m_Threads.clear();
  instance->m_Stopping = false;
  instance->AddThreads(threadCount);
}

void
ThreadPool::ThreadExecute()
{
  // plain pointer does not increase reference count
  ThreadPool * threadPool = m_PimplGlobals->m_ThreadPoolInstance.GetPointer();

  while (true)
  {
    std::function<void()> task;

    {
      std::unique_lock<std::mutex> mutexHolder(m_PimplGlobals->m_Mutex);
      threadPool->m_Condition.wait(mutexHolder,
                                   [threadPool] { return threadPool->m_Stopping || !threadPool->m_WorkQueue.empty(); });
      if (threadPool->m_Stopping && threadPool->m_WorkQueue.empty())
      {
        return;
      }
      task = std::move(threadPool->m_WorkQueue.front());
      threadPool->m_WorkQueue.pop_front();
    }

    task(); // execute the task
  }
}

ThreadPoolGlobals * ThreadPool::m_PimplGlobals;

} // namespace itk
