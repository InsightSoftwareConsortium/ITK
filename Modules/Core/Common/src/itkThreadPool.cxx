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
namespace
{
// C++20 std::atomic_flag is guaranteed lock-free and, since C++20, has a non-modifying test().
// C++17 has no atomic_flag::test(), so std::atomic<bool> is used and asserted lock-free instead.
// The feature-test macro is used rather than __cplusplus because MSVC misreports the latter.
#if defined(__cpp_lib_atomic_flag_test) && (__cpp_lib_atomic_flag_test >= 201907L)
using SingletonGuard = std::atomic_flag;
inline bool
IsGuardSet(const SingletonGuard & guard, std::memory_order order)
{
  return guard.test(order);
}
inline void
SetGuard(SingletonGuard & guard)
{
  guard.test_and_set(std::memory_order_release);
}
#else
using SingletonGuard = std::atomic<bool>;
static_assert(SingletonGuard::is_always_lock_free, "The ThreadPool singleton guard must be lock-free.");
inline bool
IsGuardSet(const SingletonGuard & guard, std::memory_order order)
{
  return guard.load(order);
}
inline void
SetGuard(SingletonGuard & guard)
{
  guard.store(true, std::memory_order_release);
}
#endif
} // namespace

struct ThreadPoolGlobals
{
  ThreadPoolGlobals() = default;

  // To lock on the various internal variables.
  std::mutex m_Mutex;

  // Guards singleton creation. Lives here so every ITK library instance shares one guard.
  SingletonGuard m_IsSingletonCreated{};

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

  // Acquire pairs with the release in SetGuard, so seeing the guard set implies seeing the pool.
  if (!IsGuardSet(m_PimplGlobals->m_IsSingletonCreated, std::memory_order_acquire))
  {
    const std::lock_guard<std::mutex> lockGuard(m_PimplGlobals->m_Mutex);

    if (!IsGuardSet(m_PimplGlobals->m_IsSingletonCreated, std::memory_order_relaxed))
    {
      m_PimplGlobals->m_ThreadPoolInstance = ObjectFactory<Self>::Create();
      if (m_PimplGlobals->m_ThreadPoolInstance.IsNull())
      {
        new ThreadPool(); // constructor sets m_PimplGlobals->m_ThreadPoolInstance
      }
#if defined(ITK_USE_PTHREADS)
      pthread_atfork(ThreadPool::PrepareForFork, ThreadPool::ResumeFromFork, ThreadPool::ResumeFromFork);
#endif
      SetGuard(m_PimplGlobals->m_IsSingletonCreated);
    }
  }

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
  // m_PimplGlobals->m_Mutex is already held by GetInstance while this constructor runs.

  m_PimplGlobals->m_ThreadPoolInstance = this;        // threads need this
  m_PimplGlobals->m_ThreadPoolInstance->UnRegister(); // Remove extra reference
  const ThreadIdType threadCount = MultiThreaderBase::GetGlobalDefaultNumberOfThreads();
  m_Threads.reserve(threadCount);
  for (ThreadIdType i = 0; i < threadCount; ++i)
  {
    m_Threads.emplace_back(&ThreadPool::ThreadExecute);
  }
}

void
ThreadPool::AddThreads(ThreadIdType count)
{
  const std::lock_guard<std::mutex> lockGuard(m_PimplGlobals->m_Mutex);
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
  const std::lock_guard<std::mutex> lockGuard(m_PimplGlobals->m_Mutex);
  return static_cast<int>(m_Threads.size()) - static_cast<int>(m_WorkQueue.size()); // lousy approximation
}

void
ThreadPool::CleanUp()
{
  bool shouldNotify = false;
  {
    const std::lock_guard<std::mutex> lockGuard(m_PimplGlobals->m_Mutex);

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
  ThreadPool *       instance = m_PimplGlobals->m_ThreadPoolInstance.GetPointer();
  const ThreadIdType threadCount = instance->m_Threads.size();
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
