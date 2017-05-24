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
#include "itkThreadPool.h"
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "itksys/SystemInformation.hxx"

namespace itk
{
ThreadIdType
ThreadPool
::GetGlobalDefaultNumberOfThreadsByPlatform()
{
  ThreadIdType num;

  // Default the number of threads to be the number of available
  // processors if we are using pthreads()
#ifdef _SC_NPROCESSORS_ONLN
  num = static_cast<ThreadIdType>( sysconf(_SC_NPROCESSORS_ONLN) );
#elif defined( _SC_NPROC_ONLN )
  num = static_cast<ThreadIdType>( sysconf(_SC_NPROC_ONLN) );
#else
  num = 1;
#endif
#if defined( __SVR4 ) && defined( sun ) && defined( PTHREAD_MUTEX_NORMAL )
  pthread_setconcurrency(num);
#endif

  itksys::SystemInformation mySys;
  mySys.RunCPUCheck();
  mySys.RunOSCheck();
  mySys.RunMemoryCheck();
  int result = mySys.GetNumberOfPhysicalCPU(); // Avoid using hyperthreading cores.
  if( result == -1 )
    {
    num = 1;
    }
  return num;
}

void
ThreadPool
::PlatformCreate(Semaphore &semaphore)
{
  bool success = false;
#if defined(__APPLE__)
  success = semaphore_create(mach_task_self(), &semaphore, SYNC_POLICY_FIFO, 0) == KERN_SUCCESS;
#else
  success = sem_init(&semaphore, 0, 0) == 0;
#endif
  if (!success)
    {
    itkGenericExceptionMacro(<< std::endl << "CreateSemaphore error. " << strerror(errno));
    }
}

void
ThreadPool
::PlatformWait(Semaphore &semaphore)
{
  bool success = false;
#if defined(__APPLE__)
  success = semaphore_wait(semaphore) == KERN_SUCCESS;
#else
  success = sem_wait(&semaphore) == 0;
#endif
  if (!success)
    {
    itkGenericExceptionMacro(<< "SemaphoreWait error. " << strerror(errno));
    }
}

void
ThreadPool
::PlatformSignal(Semaphore &semaphore)
{
  bool success = false;
#if defined(__APPLE__)
  success = semaphore_signal(semaphore) == KERN_SUCCESS;
#else
  success = sem_post(&semaphore) == 0;
#endif
  if (!success)
    {
    //m_ExceptionOccurred = true;
    itkGenericExceptionMacro(<< "SignalSemaphore error. " << strerror(errno));
    }
}

void
ThreadPool
::PlatformDelete(Semaphore &semaphore)
{
  bool success = false;
#if defined(__APPLE__)
  success = semaphore_destroy(mach_task_self(), semaphore) == KERN_SUCCESS;
#else
  success = sem_destroy(&semaphore) == 0;
#endif
  if (!success)
    {
    //m_ExceptionOccurred = true;
    itkGenericExceptionMacro(<< "DeleteSemaphore error. " << strerror(errno));
    }
}

bool
ThreadPool
::PlatformClose(ThreadProcessIdType &threadId)
{
  return pthread_join(threadId, ITK_NULLPTR) == 0;
}

void
ThreadPool
::AddThread()
{
  m_Threads.resize(m_Threads.size() + 1);

  pthread_attr_t attr;
  pthread_attr_init(&attr);

#if !defined( __CYGWIN__ )
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);
#endif
  const int rc = pthread_create(&m_Threads.back(), &attr, &ThreadPool::ThreadExecute, ITK_NULLPTR);

  if (rc)
    {
    itkDebugStatement(std::cerr << "ERROR; return code from pthread_create() is " << rc << std::endl);
    itkExceptionMacro(<< "Cannot create thread. " << strerror(errno));
    }
}

}
