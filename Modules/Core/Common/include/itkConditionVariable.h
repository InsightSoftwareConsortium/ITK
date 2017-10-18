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
#ifndef  itkConditionVariable_h
#define  itkConditionVariable_h

#include "itkConfigure.h"
#include "itkMutexLock.h"
#include "itkLightObject.h"

namespace itk
{
/** \class ConditionVariable
 * \brief A thread synchronization object used to suspend execution until some
 * condition on shared data is met.
 *
 * A thread calls Wait() to suspend its execution until the condition is
 * met. Each call to Signal() from an executing thread will then cause a single
 * waiting thread to be released.  A call to Signal() means, "signal
 * that the condition is true."  Broadcast() releases all threads waiting on
 * the condition variable.
 *
 * The ITK ConditionVariable implementation is consistent with the standard
 * definition and use of condition variables in pthreads and other common
 * thread libraries.
 *
 * IMPORTANT: A condition variable always requires an associated SimpleMutexLock
 * object.  The mutex object is used to avoid a dangerous race condition when
 * Wait() and Signal() are called simultaneously from two different
 * threads.
 *
 * On systems using pthreads, this implementation abstract the
 * standard calls to the pthread condition variable.  On Win32
 * systems, there is no system provided condition variable.  This
 * class implements a condition variable using a critical section, a
 * semphore, an event and a number of counters.  The implementation is
 * almost an extract translation of the implementation presented by
 * Douglas C Schmidt and Irfan Pyarali in "Strategies for Implementing
 * POSIX Condition Variables on Win32". This article can be found at
 * http://www.cs.wustl.edu/~schmidt/win32-cv-1.html
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ConditionVariable:public LightObject
{
public:
  /** Standard class typedefs. */
  typedef ConditionVariable          Self;
  typedef LightObject                Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConditionVariable, LightObject);

  /** Suspend execution of this thread until the condition is signaled. The
   *  argument is a SimpleMutex object that must be locked prior to calling
   *  this method.  */
  void Wait(SimpleMutexLock *mutex);

  /** Signal that the condition is true and release one waiting thread */
  void Signal();

  /** Signal that the condition is true and release all waiting threads */
  void Broadcast();

protected:
  ConditionVariable();
  ~ConditionVariable() ITK_OVERRIDE;

private:
  ConditionVariable(const Self & other);
  const Self & operator=(const Self &);

  ConditionVariableType m_ConditionVariable;

};
} // end namespace itk

#endif
