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

#include "itkLightObject.h"
#include "itkConfigure.h"

#define ITK_USE_UNIX_IPC_SEMAPHORES

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
typedef unsigned SemaphoreType;
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
 * \brief
 *
 * On unix systems 3 kinds of semaphores are supported in this class They are,
 * namely, 1. Unix IPC Semaphores (specified by the compile-time variable
 * ITK_USE_UNIX_IPC_SEMAPHORES) 2. POSIX semaphores (used with the pthread
 * library) (specified by the compile-time variable ITK_USE_PTHREADS) 3. IRIX
 * semaphores (used on the SGI) (specified by the compile-time variable
 * ITK_USE_SPROC) Also semaphores on WIN32 systems are supported.  To use the
 * semaphores the Initialize() method must be called which creates a semaphore
 * in the system and initializes it's value. The operations Up() and Down() are
 * the standard operations as defined by E. W. Dijkstra. A call must be made to
 * Remove() before the semaphore object is deleted. The Remove() method frees
 * the system semaphore variable.
 *
*/
class ITK_EXPORT Semaphore : public LightObject
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
  itkTypeMacro(Semaphore, Object);

  /** Initialize the semaphore. */
  void Initialize(int value);
  
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
private:   
  Semaphore ();
  ~Semaphore();
  
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  static int unix_semaphore_key;  // every semaphore must be created with a unique key
  static SimpleMutexLock m_Mutex; // a lock over the 'unix_semaphore_key'
  
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
