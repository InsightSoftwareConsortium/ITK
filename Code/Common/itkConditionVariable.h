/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConditionVariable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef  __itkConditionVariable_h
#define  __itkConditionVariable_h

#include "itkConfigure.h"

// This implementation uses a routine called SignalObjectAndWait()
// which is only defined on WinNT 4.0 or greater systems.  We need to
// define this symbol in order to get the prototype for the
// routine. This needs to be done before we load any system headers.
#ifdef ITK_USE_WIN32_THREADS
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0400
#endif

#include "itkMutexLock.h"
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
 */
class ITKCommon_EXPORT ConditionVariable : public LightObject
{
public:
  /** Standard class typedefs. */
  typedef ConditionVariable        Self;
  typedef LightObject              Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ConditionVariable, LightObject);

  /** Suspend execution of this thread until the condition is signaled. The
   *  argument is a SimpleMutex object that must be locked prior to calling
   *  this method.  */
  void Wait(SimpleMutexLock * mutex);

  /** Signal that the condition is true and release one waiting thread */
  void Signal();

  /** Signal that the condition is true and release all waiting threads */
  void Broadcast();

protected:
  ConditionVariable();
  ~ConditionVariable();

private:
  ConditionVariable(const Self & other);
  const Self & operator=( const Self & );
#ifdef ITK_USE_PTHREADS
  pthread_cond_t m_ConditionVariable;
  MutexType m_Mutex;
#else
  int m_NumberOfWaiters;                   // number of waiting threads
#ifdef WIN32
  CRITICAL_SECTION m_NumberOfWaitersLock;  // Serialize access to 
                                           // m_NumberOfWaiters

  HANDLE m_Semaphore;                      // Semaphore to queue threads 
  HANDLE m_WaitersAreDone;                 // Auto-reset event used by the
                                           // broadcast/signal thread to
                                           // wait for all the waiting
                                           // threads to wake up and
                                           // release the semaphore

  size_t m_WasBroadcast;                   // Keeps track of whether we
                                           // were broadcasting or signaling
#endif
#endif
};

} // end namespace itk

#endif
