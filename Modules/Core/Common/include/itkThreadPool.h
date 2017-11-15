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
#ifndef itkThreadPool_h
#define itkThreadPool_h

#include "itkConfigure.h"
#include "itkIntTypes.h"

#include <map>
#include <set>
#include <deque>

#include "itkThreadJob.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkSimpleFastMutexLock.h"
#include "itkMutexLockHolder.h"

namespace itk
{

/**
 * \class ThreadPool
 * \brief Thread pool maintains a constant number of threads.
 *
 * Thread pool is called and initialized from within the MultiThreader.
 * Initially the thread pool is started with GlobalDefaultNumberOfThreads.
 * The ThreadJob class is used to submit jobs to the thread pool. The ThreadJob's
 * necessary members need to be set and then the ThreadJob can be passed to the
 * ThreadPool by calling its AddWork method.
 * One can then wait for the job by calling the WaitForJob method.
 *
 * When thread pool is in use by the MultiThreader, invoking SetNumberOfThreads
 * on MultiThreader will only increase the number of jobs submitted to the
 * ThreadPool, it will not increase the number of threads. This trick can be
 * used to increase the number of chunks, which can help load balancing in
 * case the algorithm takes more time for some parts of the image, and there
 * is relatively small overhead for chunking (splitting the image for processing).
 *
 * If more threads are required, e.g. in case when Barrier is used,
 * AddThreads method should be invoked.
 *
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ThreadPool : public Object
{
public:

  /** Standard class typedefs. */
  typedef ThreadPool               Self;
  typedef Object                   Superclass;
  typedef SmartPointer< Self >     Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  typedef ThreadJob::Semaphore Semaphore;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThreadPool, Object);

  /** Returns the global instance */
  static Pointer New();

  /** Returns the global singleton instance of the ThreadPool */
  static Pointer GetInstance();

  /** Add this job to the thread pool queue.
   * All data members of the ThreadJob must be filled.
   * The semaphore pointer must point to a valid semaphore structure.
   * AddWork will initialize that semaphore, and the invoker must pass it
   * to WaitForJob in order to wait for the job's completion.
   */
  void AddWork(const ThreadJob& job);

  /** Can call this method if we want to add extra threads to the pool. */
  void AddThreads(ThreadIdType count);

  /** The approximate number of idle threads. */
  int GetNumberOfCurrentlyIdleThreads() const;

  /** This method blocks until the given job has finished executing. */
  void WaitForJob(Semaphore& jobSemaphore);

  /** Platform specific number of threads */
  static ThreadIdType GetGlobalDefaultNumberOfThreadsByPlatform();

  /** Examines environment variables and falls back to hyper-threaded core count */
  static ThreadIdType GetGlobalDefaultNumberOfThreads();

protected:

  static void PlatformCreate(Semaphore &semaphore);
  static void PlatformWait(Semaphore &semaphore);
  static void PlatformSignal(Semaphore &semaphore);
  static void PlatformDelete(Semaphore &semaphore);
  static bool PlatformClose(ThreadProcessIdType &threadId); //returns success status

  /** Called to add a thread to the thread pool.
  This method add a thread to the thread pool and pushes the thread handle
  into the m_Threads vector.
   */
  void AddThread();

  /** Platform-specific function to clean up all the threads. */
  void DeleteThreads();

  ThreadPool();
  virtual ~ThreadPool() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ThreadPool);

  /** Set if exception occurs */
  bool m_ExceptionOccurred;

  /** This is a list of jobs(ThreadJob) submitted to the thread pool.
   * This is the only place where the jobs are submitted.
   * Filled by AddWork, emptied by ThreadExecute.
   */
  std::deque<ThreadJob> m_WorkQueue;

  /** When a thread is idle, it is waiting on m_ThreadsSemaphore.
  * AddWork signals this semaphore to resume a (random) thread.
  */
  Semaphore m_ThreadsSemaphore;

  /** Vector to hold all thread handles.
   * Thread handles are used to delete the threads.
   */
  std::vector<ThreadProcessIdType> m_Threads;

  /** To lock on the internal variables */
  static SimpleFastMutexLock m_Mutex;

  static Pointer m_ThreadPoolInstance;

  /** The continuously running thread function */
  static ITK_THREAD_RETURN_TYPE ThreadExecute(void *param);
};

}
#endif
