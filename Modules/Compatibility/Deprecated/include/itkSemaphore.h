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
#ifndef itkSemaphore_h
#define itkSemaphore_h
#if !defined( ITK_LEGACY_REMOVE )

# include "itkObjectFactory.h"
#include <string>

#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
#include "itkMutexLock.h"
#include <cstdio>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <cerrno>
#endif

#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifdef ITK_USE_PTHREADS
#ifdef sun
#include <synch.h>
#else
#include <semaphore.h>
#endif
#endif
#endif

#ifdef ITK_USE_WIN32_THREADS
# include "itkWindows.h"
#endif

namespace itk
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
typedef int SemaphoreType;
#endif

#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifdef ITK_USE_PTHREADS
#ifdef sun
typedef sema_t SemaphoreType;
#else
#ifdef __APPLE__
typedef sem_t *SemaphoreType;
#else
typedef sem_t SemaphoreType;
#endif
#endif
#endif
#endif

#ifdef ITK_USE_WIN32_THREADS
typedef HANDLE SemaphoreType;
#endif

#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifndef ITK_USE_PTHREADS
#ifndef ITK_USE_WIN32_THREADS
typedef int SemaphoreType;
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
 * and IPC semaphores.  On Windows systems, POSIX semaphores and WIN32 thread
 * library semaphores are supported.
 * \deprecated
 * \ingroup ITKDeprecated
 */
class Semaphore:public LightObject
{
public:
  /** Standard class typedefs. */
  typedef Semaphore                  Self;
  typedef LightObject                Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

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
  void Remove();

protected:
  Semaphore ();
  ~Semaphore();
  // Newer compilers do agreessive dead code checking.
  // function is only to avoid the detection that
  // warning: private field 'm_Pad1' is not used
  // warning: private field 'm_Pad2' is not used
  std::string NeverUseThisCompilerWarningAvoidanceFunctionForPad1Pad2(void) const
    {
    return std::string(this->m_Pad1)+"-"+std::string(this->m_Pad2);
    }

private:

#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  /** Every IPC semaphore must be created with a unique key. This variable
   * increments with each new ITK Semaphore created to give a unique key. */
  static int m_IPCSemaphoreKey;
  /** Mutex to lock increment operation on m_IPCSemaphoreKey */
  static SimpleMutexLock m_Mutex;

  int  UnixIpcSemaphoreCreate(int unix_semaphore_key);

  void UnixIpcSemaphoreRemove(int sid);

  void UnixIpcSemaphoreCall(int sid, int op);

  void UnixIpcSemaphoreDown(int sid);

  void UnixIpcSemaphoreUp(int sid);

#endif

  char m_Pad1[128]; // to avoid false sharing in case of shared memory
                    // multiprocessor systems

  SemaphoreType m_Sema;

  char m_Pad2[128]; // to avoid false sharing in case of shared memory
                    // multiprocessor systems

#ifdef __APPLE__
  std::string GetUniqueName();

  static int  m_SemaphoreCount;
  std::string m_SemaphoreName;
#endif

/** When using pthread the semaphore cannot be removed twice so we use a flag */
#ifdef ITK_USE_PTHREADS
  bool m_PThreadsSemaphoreRemoved;
#endif
};
} //itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
