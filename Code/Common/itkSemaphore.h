/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSemaphore.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSemaphore_h_
#define __itkSemaphore_h_

#include "itkObjectFactory.h"
#include "itkLightObject.h"
#include "itkConfigure.h"

#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
#include "itkMutexLock.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>
#endif

#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifdef ITK_USE_SPROC
#include "itkMultiThreader.h"
#include <ulocks.h>
#endif
#ifdef ITK_USE_PTHREADS
#include <semaphore.h>
#endif
#endif

#ifdef ITK_USE_WIN32_THREADS
#include "itkWindows.h"
#endif

namespace itk {
  
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  typedef int SemaphoreType;
#endif
  
#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifdef ITK_USE_SPROC
typedef usema_t * SemaphoreType;
#endif
#ifdef ITK_USE_PTHREADS
typedef sem_t  SemaphoreType;
#endif
#endif

#ifdef ITK_USE_WIN32_THREADS
typedef HANDLE SemaphoreType;
#endif

#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifndef ITK_USE_SPROC
#ifndef ITK_USE_PTHREADS
#ifndef ITK_USE_WIN32_THREADS
typedef int SemaphoreType;
#endif
#endif
#endif
#endif

/** \class Semaphore
 * \brief The semaphore class is used to synchronize execution between threads.
 *
 * Semaphore objects maintain a counter value that can be incremented and
 * decremented by atomic calls to Up() and Down(). Semaphores are commonly used
 * to manage the use of a limited pool of system resources among several
 * threads. If Down() is called when the value of the semaphore is zero, the
 * calling thread will block until the semaphore value goes above zero again.
 * When a blocked thread is released, it decrements the Semaphore counter
 * before continuing to execute.
 *
 * The Up() and Down() operations are standard as defined by E.W. Dijkstra.
 * The Initialize( num ) operation creates the system semaphore object with an
 * initial counter value of num.  Initialize must be called before the
 * Semaphore can be used.  The Remove() method destroys the system semaphore
 * object.  It is not necessary to call Remove() unless you want to
 * re-Initialize() the object.
 *  
 * This class supports 3 types of semaphores on Unix systems, POSIX semaphores,
 * IPC semaphores, and IRIX semaphores from the SGI Sproc library.  On Windows
 * systems, POSIX semaphores and WIN32 thread library semaphores are supported.
 *
*/
class ITKCommon_EXPORT Semaphore : public LightObject
{
public:
  /** Standard class typedefs. */
  typedef Semaphore Self;
  typedef LightObject Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(Semaphore, LightObject);

  /** Initialize the semaphore with a count of value. */
  void Initialize(unsigned int value);
  
  /** Increment the semaphore count, unblocking up to one thread that may be
      blocked in the down() method.  */
  void Up();
  
  /** Decrement the semaphore count.  If the count is zero, this thread will be
      blocked until another thread calls the up() method.  The order in which
      threads will be unblocked is not defined, but implementors should give
      preference to those threads that have waited the longest.
  */
  void Down();
  
  /** Remove the semaphore from the system. */
  void Remove ();

protected:
  Semaphore ();
  ~Semaphore();

private:   
  
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  /** Every IPC semaphore must be created with a unique key. This variable
   * increments with each new ITK Semaphore created to give a unique key. */
  static int m_IPCSemaphoreKey;
  /** Mutex to lock increment operation on m_IPCSemaphoreKey */
  static SimpleMutexLock m_Mutex;
  
  int  UnixIpcSemaphoreCreate (int unix_semaphore_key);
  void UnixIpcSemaphoreRemove (int sid);
  void UnixIpcSemaphoreCall   (int sid, int op);
  void UnixIpcSemaphoreDown   (int sid);
  void UnixIpcSemaphoreUp     (int sid);
#endif
  
  char Pad1[128]; // to avoid false sharing in case of shared memory multiprocessor systems
  SemaphoreType m_Sema;
  char Pad2[128]; // to avoid false sharing in case of shared memory multiprocessor systems
};
 
}//itk

#endif
