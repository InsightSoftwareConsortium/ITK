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
  ITK_DISALLOW_COPY_AND_ASSIGN(PoolMultiThreader);

  /** Standard class type aliases. */
  using Self = PoolMultiThreader;
  using Superclass = MultiThreaderBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PoolMultiThreader, MultiThreaderBase);


  /** Execute the SingleMethod (as define by SetSingleMethod) using
   * m_NumberOfWorkUnits work units. As a side effect the m_NumberOfWorkUnits will be
   * checked against the current m_GlobalMaximumNumberOfThreads and clamped if
   * necessary. */
  void SingleMethodExecute() override;

  /** Set the SingleMethod to f() and the UserData field of the
   * WorkUnitInfo that is passed to it will be data.
   * This method must be of type itkThreadFunctionType and
   * must take a single argument of type void. */
  void SetSingleMethod(ThreadFunctionType, void *data) override;

  /** Set the number of threads to use. PoolMultiThreader
   * can only INCREASE its number of threads. */
  virtual void SetMaximumNumberOfThreads( ThreadIdType numberOfThreads ) override;

  using JobSemaphoreType = ThreadPool::Semaphore;

  struct ThreadPoolInfoStruct :WorkUnitInfo
    {
    JobSemaphoreType Semaphore;
    };

protected:
  PoolMultiThreader();
  ~PoolMultiThreader() override;
  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  // Thread pool instance and factory
  ThreadPool::Pointer m_ThreadPool;

  /** An array of thread info containing a thread id
   *  (0, 1, 2, .. ITK_MAX_THREADS-1), the thread count, and a pointer
   *  to void so that user data can be passed to each thread. */
  ThreadPoolInfoStruct m_ThreadInfoArray[ITK_MAX_THREADS];

  /** Friends of Multithreader.
   * ProcessObject is a friend so that it can call PrintSelf() on its
   * Multithreader. */
  friend class ProcessObject;
};

}  // end namespace itk
#endif
