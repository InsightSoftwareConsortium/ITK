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
#ifndef itkMultiThreader_h
#define itkMultiThreader_h

#include "itkMultiThreaderBase.h"
#include "itkMutexLock.h"
#include "itkThreadSupport.h"
#include "itkIntTypes.h"

#include "itkThreadPool.h"

namespace itk
{
/** \class MultiThreader
 * \brief A class for performing multithreaded execution
 *
 * Multithreader is a class that provides support for multithreaded
 * execution using Windows or POSIX threads.
 * This class can be used to execute a single
 * method on multiple threads, or to specify a method per thread.
 *
 * \ingroup OSSystemObjects
 *
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT MultiThreader : public MultiThreaderBase
{
public:
  /** Standard class type aliases. */
  using Self = MultiThreader;
  using Superclass = MultiThreaderBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiThreader, MultiThreaderBase);


  /** Execute the SingleMethod (as define by SetSingleMethod) using
   * m_NumberOfThreads threads. As a side effect the m_NumberOfThreads will be
   * checked against the current m_GlobalMaximumNumberOfThreads and clamped if
   * necessary. */
  void SingleMethodExecute() override;

  /** Execute the MultipleMethods (as define by calling SetMultipleMethod for
   * each of the required m_NumberOfThreads methods) using m_NumberOfThreads
   * threads. As a side effect the m_NumberOfThreads will be checked against the
   * current m_GlobalMaximumNumberOfThreads and clamped if necessary. */
  void MultipleMethodExecute() override;

  /** Set the SingleMethod to f() and the UserData field of the
   * ThreadInfoStruct that is passed to it will be data.
   * This method (and all the methods passed to SetMultipleMethod)
   * must be of type itkThreadFunctionType and must take a single argument of
   * type void *. */
  void SetSingleMethod(ThreadFunctionType, void *data) override;

  /** Set the MultipleMethod at the given index to f() and the UserData
   * field of the ThreadInfoStruct that is passed to it will be data. */
  void SetMultipleMethod(ThreadIdType index, ThreadFunctionType, void *data) override;

  /** Create a new thread for the given function. Return a thread id
     * which is a number between 0 and ITK_MAX_THREADS - 1. This
   * id should be used to kill the thread at a later time. */
  ThreadIdType SpawnThread(ThreadFunctionType, void *data) override;

  /** Terminate the thread that was created with a SpawnThreadExecute() */
  void TerminateThread(ThreadIdType thread_id) override;

  /** Set the ThreadPool used by this MultiThreader. If not set,
    * the default ThreadPool will be used. Currently ThreadPool
    * is only used in SingleMethodExecute. */
  itkSetObjectMacro(ThreadPool, ThreadPool);

  /** Get the ThreadPool used by this MultiThreader */
  itkGetModifiableObjectMacro(ThreadPool, ThreadPool);

  /** Set the flag to use a threadpool instead of spawning individual
    * threads
    */
  itkSetMacro(UseThreadPool,bool);
  /** Get the UseThreadPool flag*/
  itkGetMacro(UseThreadPool,bool);


  typedef ThreadPool::Semaphore JobSemaphoreType;

  /** This is the structure that is passed to the thread that is
   * created from the SingleMethodExecute, MultipleMethodExecute or
   * the SpawnThread method. It is passed in as a void *, and it is up
   * to the method to cast correctly and extract the information.  The
   * ThreadID is a number between 0 and NumberOfThreads-1 that
   * indicates the id of this thread. The NumberOfThreads is
   * this->NumberOfThreads for threads created from
   * SingleMethodExecute or MultipleMethodExecute, and it is 1 for
   * threads created from SpawnThread.  The UserData is the (void
   * *)arg passed into the SetSingleMethod, SetMultipleMethod, or
   * SpawnThread method. */
#ifdef ThreadInfoStruct
#undef ThreadInfoStruct
#endif
  struct ThreadInfoStruct
    {
    ThreadIdType ThreadID;
    ThreadIdType NumberOfThreads;
    int *ActiveFlag;
    MutexLock::Pointer ActiveFlagLock;
    void *UserData;
    ThreadFunctionType ThreadFunction;
    JobSemaphoreType Semaphore;
    enum { SUCCESS, ITK_EXCEPTION, ITK_PROCESS_ABORTED_EXCEPTION, STD_EXCEPTION, UNKNOWN } ThreadExitCode;
    };

protected:
  MultiThreader();
  ~MultiThreader() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiThreader);

  // choose whether to use Spawn or ThreadPool methods
  bool m_UseThreadPool;


  // Thread pool instance and factory
  ThreadPool::Pointer m_ThreadPool;

  /** An array of thread info containing a thread id
   *  (0, 1, 2, .. ITK_MAX_THREADS-1), the thread count, and a pointer
   *  to void so that user data can be passed to each thread. */
  ThreadInfoStruct m_ThreadInfoArray[ITK_MAX_THREADS];

  /** The methods to invoke. */
  ThreadFunctionType m_SingleMethod;
  ThreadFunctionType m_MultipleMethod[ITK_MAX_THREADS];

  /** Storage of MutexFunctions and ints used to control spawned
   *  threads and the spawned thread ids. */
  int                 m_SpawnedThreadActiveFlag[ITK_MAX_THREADS];
  MutexLock::Pointer  m_SpawnedThreadActiveFlagLock[ITK_MAX_THREADS];
  ThreadProcessIdType m_SpawnedThreadProcessID[ITK_MAX_THREADS];
  ThreadInfoStruct    m_SpawnedThreadInfoArray[ITK_MAX_THREADS];

  /** Internal storage of the data. */
  void *m_SingleData;
  void *m_MultipleData[ITK_MAX_THREADS];

  /** Static function used as a "proxy callback" by the MultiThreader.  The
   * threading library will call this routine for each thread, which
   * will delegate the control to the prescribed SingleMethod. This
   * routine acts as an intermediary between the MultiThreader and the
   * user supplied callback (SingleMethod) in order to catch any
   * exceptions thrown by the threads. */
  static ITK_THREAD_RETURN_TYPE SingleMethodProxy(void *arg);

  /** Assign work to a thread in the thread pool */
  void ThreadPoolDispatchSingleMethodThread(ThreadInfoStruct *);

  /** spawn a new thread for the SingleMethod */
  ThreadProcessIdType SpawnDispatchSingleMethodThread(ThreadInfoStruct *);
  /** wait for a thread in the threadpool to finish work */
  void SpawnWaitForSingleMethodThread(ThreadProcessIdType);

  /** Friends of Multithreader.
   * ProcessObject is a friend so that it can call PrintSelf() on its
   * Multithreader. */
  friend class ProcessObject;
};
}  // end namespace itk
#endif
