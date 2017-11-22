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
#ifndef itkMultiThreaderBase_h
#define itkMultiThreaderBase_h

#include "itkObject.h"
#include "itkThreadSupport.h"
#include "itkIntTypes.h"


namespace itk
{
/** \class MultiThreaderBase
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

class ITKCommon_EXPORT MultiThreaderBase : public Object
{
public:
  /** Standard class type aliases. */
  using Self = MultiThreaderBase;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  static Pointer New(void);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiThreaderBase, Object);

  /** Get/Set the number of threads to create. It will be clamped to the range
   * [ 1, m_GlobalMaximumNumberOfThreads ], so the caller of this method should
   * check that the requested number of threads was accepted. */
  virtual void SetNumberOfThreads(ThreadIdType numberOfThreads);

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
  virtual void SingleMethodExecute() = 0;

  /** Execute the MultipleMethods (as define by calling SetMultipleMethod for
   * each of the required m_NumberOfThreads methods) using m_NumberOfThreads
   * threads. As a side effect the m_NumberOfThreads will be checked against the
   * current m_GlobalMaximumNumberOfThreads and clamped if necessary. */
  virtual void MultipleMethodExecute() = 0;

  /** Set the SingleMethod to f() and the UserData field of the
   * ThreadInfoStruct that is passed to it will be data.
   * This method (and all the methods passed to SetMultipleMethod)
   * must be of type itkThreadFunctionType and must take a single argument of
   * type void *. */
  virtual void SetSingleMethod(ThreadFunctionType, void *data) = 0;

  /** Set the MultipleMethod at the given index to f() and the UserData
   * field of the ThreadInfoStruct that is passed to it will be data. */
  virtual void SetMultipleMethod(ThreadIdType index, ThreadFunctionType, void *data) = 0;

  /** Create a new thread for the given function. Return a thread id
     * which is a number between 0 and ITK_MAX_THREADS - 1. This
   * id should be used to kill the thread at a later time. */
  virtual ThreadIdType SpawnThread(ThreadFunctionType, void *data) = 0;

  /** Terminate the thread that was created with a SpawnThreadExecute() */
  virtual void TerminateThread(ThreadIdType thread_id) = 0;

protected:
  MultiThreaderBase();
  ~MultiThreaderBase() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;


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

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiThreaderBase);

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
   *  construction time of a MultiThreaderBase instance.  The
   *  m_GlobalDefaultNumberOfThreads must always be less than or equal to the
   *  m_GlobalMaximumNumberOfThreads and larger or equal to 1 once it has been
   *  initialized in the constructor of the first MultiThreaderBase instantiation.
   */
  static ThreadIdType m_GlobalDefaultNumberOfThreads;


  /** Friends of Multithreader.
   * ProcessObject is a friend so that it can call PrintSelf() on its
   * Multithreader. */
  friend class ProcessObject;
};
}  // end namespace itk
#endif
