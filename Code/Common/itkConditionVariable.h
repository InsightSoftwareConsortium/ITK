/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConditionVariable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef  _itkConditionVariable_h_
#define  _itkConditionVariable_h_

#include "itkMutexLock.h"
#include "itkSemaphore.h"
#include "itkLightObject.h"

namespace itk {

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
 * Wait() and Signal() are called simultaneously from two different threads.
 */
class ITK_EXPORT ConditionVariable : public LightObject
{
public:
  /** Standard class typedefs. */
  typedef ConditionVariable Self;
  typedef LightObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ConditionVariable, Object);

  /** Suspend execution of this thread until the condition is signaled. The
   *  argument is a SimpleMutex object that must be locked prior to calling
   *  this method.  */
  void Wait(SimpleMutexLock * mutex);

  /** Signal that the condition is true and release one waiting thread */
  void Signal();

  /** Signal that the condition is true and release all waiting threads */
  void Broadcast();

private:
#ifdef ITK_USE_PTHREADS
  pthread_cond_t m_ConditionVariable;
  MutexType m_Mutex;
#else
  int m_NumberOfWaiters;
  SimpleMutexLock m_Lock;
  Semaphore::Pointer m_Semaphore;
#endif
  ConditionVariable();
  ~ConditionVariable();
};

}//end of itk namespace

#endif
