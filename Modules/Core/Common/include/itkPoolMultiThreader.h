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
#ifndef itkPoolMultiThreader_h
#define itkPoolMultiThreader_h

#include "itkMultiThreaderBase.h"
#include "itkMutexLock.h"
#include "itkThreadSupport.h"
#include "itkIntTypes.h"

#include "itkThreadPool.h"

namespace itk
{
/** \class PoolMultiThreader
 * \brief A class for performing multithreaded execution with a thread
 * pool back end
 *
 * \ingroup OSSystemObjects
 *
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT PoolMultiThreader : public MultiThreaderBase
{
public:
  /** Standard class type aliases. */
  using Self = PoolMultiThreader;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PoolMultiThreader, Object);


  /** Execute the SingleMethod (as define by SetSingleMethod) using
   * m_NumberOfThreads threads. As a side effect the m_NumberOfThreads will be
   * checked against the current m_GlobalMaximumNumberOfThreads and clamped if
   * necessary. */
  void SingleMethodExecute() override;

  void MultipleMethodExecute() override {}
  /** Set the SingleMethod to f() and the UserData field of the
   * ThreadInfoStruct that is passed to it will be data.
   * This method (and all the methods passed to SetMultipleMethod)
   * must be of type itkThreadFunctionType and must take a single argument of
   * type void *. */
  void SetSingleMethod(ThreadFunctionType, void *data) override;

  /** Set the MultipleMethod at the given index to f() and the UserData
   * field of the ThreadInfoStruct that is passed to it will be data. */
  void SetMultipleMethod(ThreadIdType, ThreadFunctionType, void *) override {}

  ThreadIdType SpawnThread(ThreadFunctionType, void *) override {return 0;}
  void TerminateThread(ThreadIdType) override {return;}
  using JobSemaphoreType = ThreadPool::Semaphore;

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
  PoolMultiThreader();
  ~PoolMultiThreader() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PoolMultiThreader);

  // Thread pool instance and factory
  ThreadPool::Pointer m_ThreadPool;

  /** An array of thread info containing a thread id
   *  (0, 1, 2, .. ITK_MAX_THREADS-1), the thread count, and a pointer
   *  to void so that user data can be passed to each thread. */
  ThreadInfoStruct m_ThreadInfoArray[ITK_MAX_THREADS];

  /** The methods to invoke. */
  ThreadFunctionType m_SingleMethod;

  /** Internal storage of the data. */
  void *m_SingleData;

  /** Static function used as a "proxy callback" by the MultiThreader.  The
   * threading library will call this routine for each thread, which
   * will delegate the control to the prescribed SingleMethod. This
   * routine acts as an intermediary between the MultiThreader and the
   * user supplied callback (SingleMethod) in order to catch any
   * exceptions thrown by the threads. */
  static ITK_THREAD_RETURN_TYPE SingleMethodProxy(void *arg);

  /** Friends of Multithreader.
   * ProcessObject is a friend so that it can call PrintSelf() on its
   * Multithreader. */
  friend class ProcessObject;
};
}  // end namespace itk
#endif
