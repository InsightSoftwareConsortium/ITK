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

class ITKCommon_EXPORT MultiThreader : public Object
{
public:
  /** Standard class typedefs. */
  typedef MultiThreader            Self;
  typedef Object                   Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiThreader, Object);

  /** Get/Set the number of threads to create. It will be clamped to the range
   * [ 1, m_GlobalMaximumNumberOfThreads ], so the caller of this method should
   * check that the requested number of threads was accepted. */
  void SetNumberOfThreads(ThreadIdType numberOfThreads);

  itkGetConstMacro(NumberOfThreads, ThreadIdType);

  /** Set/Get the maximum number of threads to use when multithreading.  It
   * will be clamped to the range [ 1, ITK_MAX_THREADS ] because several arrays
   * are already statically allocated using the ITK_MAX_THREADS number.
   * Therefore the caller of this method should check that the requested number
   * of threads was accepted. */
  static void SetGlobalMaximumNumberOfThreads(ThreadIdType val);
  static ThreadIdType  GetGlobalMaximumNumberOfThreads();

  /** Set/Get whether to use the to use the thread pool
   * implementation or the spawing implementation of
   * starting threads.
   */
  static void SetGlobalDefaultUseThreadPool( const bool GlobalDefaultUseThreadPool );
  static bool GetGlobalDefaultUseThreadPool( );

  /** Set/Get the value which is used to initialize the NumberOfThreads in the
   * constructor.  It will be clamped to the range [1, m_GlobalMaximumNumberOfThreads ].
   * Therefore the caller of this method should check that the requested number
   * of threads was accepted. */
  static void SetGlobalDefaultNumberOfThreads(ThreadIdType val);

  static ThreadIdType  GetGlobalDefaultNumberOfThreads();

  /** Execute the SingleMethod (as define by SetSingleMethod) using
   * m_NumberOfThreads threads. As a side effect the m_NumberOfThreads will be
   * checked against the current m_GlobalMaximumNumberOfThreads and clamped if
   * necessary. */
  void SingleMethodExecute();

  /** Execute the MultipleMethods (as define by calling SetMultipleMethod for
   * each of the required m_NumberOfThreads methods) using m_NumberOfThreads
   * threads. As a side effect the m_NumberOfThreads will be checked against the
   * current m_GlobalMaximumNumberOfThreads and clamped if necessary. */
  void MultipleMethodExecute();

  /** Set the SingleMethod to f() and the UserData field of the
   * ThreadInfoStruct that is passed to it will be data.
   * This method (and all the methods passed to SetMultipleMethod)
   * must be of type itkThreadFunctionType and must take a single argument of
   * type void *. */
  void SetSingleMethod(ThreadFunctionType, void *data);

  /** Set the MultipleMethod at the given index to f() and the UserData
   * field of the ThreadInfoStruct that is passed to it will be data. */
  void SetMultipleMethod(ThreadIdType index, ThreadFunctionType, void *data);

  /** Create a new thread for the given function. Return a thread id
     * which is a number between 0 and ITK_MAX_THREADS - 1. This
   * id should be used to kill the thread at a later time. */
  ThreadIdType SpawnThread(ThreadFunctionType, void *data);

  /** Terminate the thread that was created with a SpawnThreadExecute() */
  void TerminateThread(ThreadIdType thread_id);

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

  // Thread pool instance and factory
  ThreadPool::Pointer m_ThreadPool;

  // choose whether to use Spawn or ThreadPool methods
  bool m_UseThreadPool;

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

  /** Global variable defining the maximum number of threads that can be used.
   *  The m_GlobalMaximumNumberOfThreads must always be less than or equal to
   *  ITK_MAX_THREADS and greater than zero. */
  static ThreadIdType m_GlobalMaximumNumberOfThreads;

  /** Global value to control weather the threadpool implementation should
   * be used.  This defaults to the environmental variable "ITK_USE_THREADPOOL"
   * if set, else it default to true.
   */
  static bool m_GlobalDefaultUseThreadPool;

  /*  Global variable defining the default number of threads to set at
   *  construction time of a MultiThreader instance.  The
   *  m_GlobalDefaultNumberOfThreads must always be less than or equal to the
   *  m_GlobalMaximumNumberOfThreads and larger or equal to 1 once it has been
   *  initialized in the constructor of the first MultiThreader instantiation.
   */
  static ThreadIdType m_GlobalDefaultNumberOfThreads;

  /** The number of threads to use.
   *  The m_NumberOfThreads must always be less than or equal to
   *  the m_GlobalMaximumNumberOfThreads before it is used during the execution
   *  of a threaded method. Its value is clamped in the SingleMethodExecute()
   *  and MultipleMethodExecute(). Its value is initialized to
   *  m_GlobalDefaultNumberOfThreads at construction time. Its value is clamped
   *  to the current m_GlobalMaximumNumberOfThreads in the
   *  SingleMethodExecute() and MultipleMethodExecute() methods.
   */
  ThreadIdType m_NumberOfThreads;

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
