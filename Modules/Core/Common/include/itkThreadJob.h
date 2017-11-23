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

#ifndef itkThreadJob_h
#define itkThreadJob_h

#include "itkMacro.h"
#include "itkThreadSupport.h"

#if defined(ITK_USE_PTHREADS)
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h> // for sleep
#elif defined(ITK_USE_WIN32_THREADS)
#include <windows.h>
#endif

#if defined __APPLE__
#include <mach/mach_init.h>
#include <mach/mach_error.h>
#include <mach/semaphore.h>
#include <mach/task.h>
#include <mach/task_info.h>
#endif

namespace itk
{
/**
 * \class ThreadJob
 *
 * \brief This class is used to submit jobs to the thread pool.
 *
 * The thread job contains information of the submitted job:
 *   the function to be executed in parallel
 *   the function's argument
 *   a pointer to the semaphore to wait on for job completion.
 *
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */
struct ThreadJob
{
public:

#if defined(ITK_USE_PTHREADS) && defined(__APPLE__)
  typedef semaphore_t Semaphore;
#elif defined(ITK_USE_WIN32_THREADS)
  typedef HANDLE Semaphore;
#elif defined(ITK_USE_PTHREADS)
  typedef sem_t Semaphore;
#else
#error Unknown thread system!
#endif

  ThreadJob() :
    m_ThreadFunction(ITK_NULLPTR),
    m_Semaphore(ITK_NULLPTR),
    m_UserData(ITK_NULLPTR)
  {
  }

  ~ThreadJob()
  {
  }


/** Function that will be called. */
#if defined(_WIN32) || defined(_WIN64)
  ThreadFunctionType m_ThreadFunction;
#else
  void * (*m_ThreadFunction)(void *ptr);
#endif

  /** This is the Job's id. Used for waiting on this job's completion. */
  Semaphore * m_Semaphore;

 /** Stores the user's data that needs to be passed into the function. */
  void * m_UserData;
};

} // end namespace itk

#endif // itkThreadJob_h
