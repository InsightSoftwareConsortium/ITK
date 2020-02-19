/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkThreadSupport.h"
#include "itkNumericTraits.h"
#include "itkMultiThreaderBase.h"
#include "itkSingleton.h"

#include <algorithm>


namespace itk
{

struct ThreadPoolGlobals
{
  ThreadPoolGlobals() = default;
  // To lock on the internal variables.
  std::mutex          m_Mutex;
  ThreadPool::Pointer m_ThreadPoolInstance;
#if defined(_WIN32) && defined(ITKCommon_EXPORTS)
  // ThreadPool's destructor is called during DllMain's DLL_PROCESS_DETACH.
  // Because ITKCommon-5.X.dll is usually being detached due to process termination,
  // lpvReserved is non-NULL meaning that "all threads in the process
  // except the current thread either have exited already or have been
  // explicitly terminated by a call to the ExitProcess function".
  // Therefore we must not wait for the condition_variable.
  bool m_WaitForThreads = false;
#else // In a static library, we have to wait.
  bool m_WaitForThreads = true;
#endif
};

itkGetGlobalSimpleMacro(ThreadPool, ThreadPoolGlobals, PimplGlobals);

ThreadPool::Pointer
ThreadPool ::New()
{
  return Self::GetInstance();
}


ThreadPool::Pointer
ThreadPool ::GetInstance()
{
  // This is called once, on-demand to ensure that m_PimplGlobals is
  // initialized.
  itkInitGlobalsMacro(PimplGlobals);

  if (m_PimplGlobals->m_ThreadPoolInstance.IsNull())
  {
    std::unique_lock<std::mutex> mutexHolder(m_PimplGlobals->m_Mutex);
    // After we have the lock, double check the initialization
    // flag to ensure it hasn't been changed by another thread.
    if (m_PimplGlobals->m_ThreadPoolInstance.IsNull())
    {
      m_PimplGlobals->m_ThreadPoolInstance = ObjectFactory<Self>::Create();
      if (m_PimplGlobals->m_ThreadPoolInstance.IsNull())
      {
        new ThreadPool(); // constructor sets m_PimplGlobals->m_ThreadPoolInstance
      }
    }
  }
  return m_PimplGlobals->m_ThreadPoolInstance;
}

bool
ThreadPool ::GetDoNotWaitForThreads()
{
  itkInitGlobalsMacro(PimplGlobals);
  return !m_PimplGlobals->m_WaitForThreads;
}

void
ThreadPool ::SetDoNotWaitForThreads(bool doNotWaitForThreads)
{
  itkInitGlobalsMacro(PimplGlobals);
  m_PimplGlobals->m_WaitForThreads = !doNotWaitForThreads;
}

ThreadPool ::ThreadPool()
{
  m_PimplGlobals->m_ThreadPoolInstance = this;        // threads need this
  m_PimplGlobals->m_ThreadPoolInstance->UnRegister(); // Remove extra reference
  ThreadIdType threadCount = MultiThreaderBase::GetGlobalDefaultNumberOfThreads();
  m_Threads.reserve(threadCount);
  for (unsigned int i = 0; i < threadCount; ++i)
  {
    m_Threads.emplace_back(&ThreadPool::ThreadExecute);
  }
}

void
ThreadPool ::AddThreads(ThreadIdType count)
{
  std::unique_lock<std::mutex> mutexHolder(m_PimplGlobals->m_Mutex);
  m_Threads.reserve(m_Threads.size() + count);
  for (unsigned int i = 0; i < count; ++i)
  {
    m_Threads.emplace_back(&ThreadPool::ThreadExecute);
  }
}

std::mutex &
ThreadPool ::GetMutex()
{
  return m_PimplGlobals->m_Mutex;
}

int
ThreadPool ::GetNumberOfCurrentlyIdleThreads() const
{
  std::unique_lock<std::mutex> mutexHolder(m_PimplGlobals->m_Mutex);
  return int(m_Threads.size()) - int(m_WorkQueue.size()); // lousy approximation
}

ThreadPool ::~ThreadPool()
{
  {
    std::unique_lock<std::mutex> mutexHolder(m_PimplGlobals->m_Mutex);

    this->m_Stopping = true;
  }

  if (m_PimplGlobals->m_WaitForThreads && !m_Threads.empty())
  {
    m_Condition.notify_all();
  }

  // Even if the threads have already been terminated,
  // we should join() the std::thread variables.
  // Otherwise some sanity check in debug mode complains.
  for (auto & m_Thread : m_Threads)
  {
    m_Thread.join();
  }
}


void
ThreadPool ::ThreadExecute()
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
