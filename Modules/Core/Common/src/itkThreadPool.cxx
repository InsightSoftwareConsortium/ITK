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
#include "itkThreadSupport.h"
#include "itkNumericTraits.h"

#include <algorithm>

namespace itk
{
  struct ThreadPoolGlobals
  {
    ThreadPoolGlobals():m_DoNotWaitForThreads(false){};
    // To lock on the internal variables.
    std::mutex          m_Mutex;
    ThreadPool::Pointer m_ThreadPoolInstance;
    bool                m_DoNotWaitForThreads;
  };
}//end of itk namespace


namespace
{
/** \brief A function which does nothing
 *
 * This function is to be used to mark parameters as unused to suppress
 * compiler warning. It can be used when the parameter needs to be named
 * (i.e. itkNotUsed cannot be used) but is not always used. It ensures
 * that the parameter is not optimized out.
 */
template <typename T>
void Unused( const T &) {};

// This ensures that m_ThreadPoolGlobals is has been initialized once
// the library has been loaded. In some cases, this call will perform the
// initialization. In other cases, static initializers like the IO factory
// initialization code will have done the initialization.
static ::itk::ThreadPoolGlobals *
    initializedThreadPoolGlobals =
    ::itk::ThreadPool::GetThreadPoolGlobals();

/** \class ThreadPoolGlobalsInitializer
 *
 * \brief Initialize a ThreadPoolGlobals and delete it on program
 * completion.
 * */
class ThreadPoolGlobalsInitializer
{
public:
  using Self = ThreadPoolGlobalsInitializer;

  ThreadPoolGlobalsInitializer() = default;

  /** Delete the thread pool globals if it was created. */
  ~ThreadPoolGlobalsInitializer()
    {
    delete m_ThreadPoolGlobals;
    m_ThreadPoolGlobals = nullptr;
    }

  /** Create the ThreadPoolGlobals if needed and return it. */
  static ::itk::ThreadPoolGlobals * GetThreadPoolGlobals()
    {
    if( !m_ThreadPoolGlobals )
      {
      m_ThreadPoolGlobals = new ::itk::ThreadPoolGlobals;

      // To avoid being optimized out. The compiler does not like this
      // statement at a higher scope.
      Unused(initializedThreadPoolGlobals);
      }
    return m_ThreadPoolGlobals;
    }

private:
  static ::itk::ThreadPoolGlobals *
      m_ThreadPoolGlobals;
};

// Takes care of cleaning up the ThreadPoolGlobals
static ThreadPoolGlobalsInitializer ThreadPoolGlobalsInstance;
// Initialized by the compiler to zero
::itk::ThreadPoolGlobals *
    ThreadPoolGlobalsInitializer::m_ThreadPoolGlobals;
}// end of anonymous namespace

namespace itk
{

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
  std::unique_lock<std::mutex> mutexHolder( m_ThreadPoolGlobals->m_Mutex );

  // This is called once, on-demand to ensure that m_ThreadPoolGlobals is
  // initialized.
  static ThreadPoolGlobals * threadPoolGlobals = GetThreadPoolGlobals();
  Unused(threadPoolGlobals);

  if( m_ThreadPoolGlobals->m_ThreadPoolInstance.IsNull() )
    {
    m_ThreadPoolGlobals->m_ThreadPoolInstance  = ObjectFactory< Self >::Create();
    if ( m_ThreadPoolGlobals->m_ThreadPoolInstance.IsNull() )
      {
      new ThreadPool(); //constructor sets m_ThreadPoolGlobals->m_ThreadPoolInstance
      }
    }
  return m_ThreadPoolGlobals->m_ThreadPoolInstance;
}

ThreadPoolGlobals *
ThreadPool
::GetThreadPoolGlobals()
{
  if( m_ThreadPoolGlobals == nullptr )
    {
    m_ThreadPoolGlobals = ThreadPoolGlobalsInitializer::GetThreadPoolGlobals();
    }
  return m_ThreadPoolGlobals;
}

void
ThreadPool
::SetThreadPoolGlobals(::itk::ThreadPoolGlobals * globals)
{
  m_ThreadPoolGlobals = globals;
}

bool
ThreadPool
::GetDoNotWaitForThreads()
{
  static ThreadPoolGlobals * threadPoolGlobals = GetThreadPoolGlobals();
  Unused(threadPoolGlobals);
  return m_ThreadPoolGlobals->m_DoNotWaitForThreads;
}

void
ThreadPool
::SetDoNotWaitForThreads(bool doNotWaitForThreads)
{
  static ThreadPoolGlobals * threadPoolGlobals = GetThreadPoolGlobals();
  Unused(threadPoolGlobals);
  m_ThreadPoolGlobals->m_DoNotWaitForThreads = doNotWaitForThreads;
}

ThreadPool
::ThreadPool()
  : m_Stopping( false )
{
  m_ThreadPoolGlobals->m_ThreadPoolInstance = this; //threads need this
  m_ThreadPoolGlobals->m_ThreadPoolInstance->UnRegister(); // Remove extra reference
  ThreadIdType threadCount = ThreadPool::GetGlobalDefaultNumberOfThreads();
  m_Threads.reserve( threadCount );
  for ( unsigned int i = 0; i < threadCount; ++i )
    {
      m_Threads.emplace_back( &ThreadPool::ThreadExecute );
    }
}

#if !defined( ITK_LEGACY_REMOVE )
ThreadIdType
ThreadPool
::GetGlobalDefaultNumberOfThreadsByPlatform()
{
  return std::thread::hardware_concurrency();
}
#endif

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
      threadCount = static_cast<ThreadIdType>( std::stoi( itkGlobalDefaultNumberOfThreadsEnv.c_str() ) );
      }
    }

  // otherwise, set number of threads based on system information
  if( threadCount <= 0 )
    {
    threadCount = std::thread::hardware_concurrency();
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
  std::unique_lock<std::mutex> mutexHolder( m_ThreadPoolGlobals->m_Mutex );
  m_Threads.reserve( m_Threads.size() + count );
  for( unsigned int i = 0; i < count; ++i )
    {
    m_Threads.emplace_back( &ThreadPool::ThreadExecute );
    }
}

std::mutex&
ThreadPool
::GetMutex()
{
  return m_ThreadPoolGlobals->m_Mutex;
}

int
ThreadPool
::GetNumberOfCurrentlyIdleThreads() const
{
  std::unique_lock<std::mutex> mutexHolder( m_ThreadPoolGlobals->m_Mutex );
  return int(m_Threads.size()) - int(m_WorkQueue.size()); // lousy approximation
}

ThreadPool
::~ThreadPool()
{
  bool waitForThreads = m_Threads.size() > 0;
#if defined(_WIN32) && defined(ITKCommon_EXPORTS)
  //This destructor is called during DllMain's DLL_PROCESS_DETACH.
  //Because ITKCommon-4.X.dll is usually being detached due to process termination,
  //lpvReserved is non-NULL meaning that "all threads in the process
  //except the current thread either have exited already or have been
  //explicitly terminated by a call to the ExitProcess function".
  waitForThreads = false;
#else
  if(m_ThreadPoolGlobals->m_DoNotWaitForThreads)
    {
    waitForThreads = false;
    }
#endif

  {
    std::unique_lock< std::mutex > mutexHolder( m_ThreadPoolGlobals->m_Mutex );
    this->m_Stopping = true;
  }

  if (waitForThreads)
    {
    m_Condition.notify_all();
    }

  // Even if the threads have already been terminated,
  // we should join() the std::thread variables.
  // Otherwise some sanity check in debug mode makes a problem.
  for (ThreadIdType i = 0; i < m_Threads.size(); i++)
    {
    m_Threads[i].join();
    }
}


void
ThreadPool
::ThreadExecute()
{
  //plain pointer does not increase reference count
  ThreadPool* threadPool = m_ThreadPoolGlobals->m_ThreadPoolInstance.GetPointer();

  while ( true )
    {
      std::function< void() > task;

      {
        std::unique_lock<std::mutex> mutexHolder( m_ThreadPoolGlobals->m_Mutex );
        threadPool->m_Condition.wait( mutexHolder,
          [threadPool]
          {
            return threadPool->m_Stopping || !threadPool->m_WorkQueue.empty();
          }
        );
        if ( threadPool->m_Stopping && threadPool->m_WorkQueue.empty() )
          {
            return;
          }
        task = std::move( threadPool->m_WorkQueue.front() );
        threadPool->m_WorkQueue.pop_front();
      }

      task(); //execute the task
    }
}

ThreadPoolGlobals * ThreadPool::m_ThreadPoolGlobals;

}
