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
#include "itkMutexLock.h"

namespace itk
{
// New for the SimpleMutex
SimpleMutexLock * SimpleMutexLock::New()
{
  return new SimpleMutexLock;
}

// Construct a new MutexLock
SimpleMutexLock::SimpleMutexLock()
{
#ifdef ITK_USE_WIN32_THREADS
  m_MutexLock = CreateMutex(NULL, FALSE, NULL);
#endif

#ifdef ITK_USE_PTHREADS
#ifdef ITK_HP_PTHREADS
  pthread_mutex_init(&m_MutexLock, pthread_mutexattr_default);
#else
  pthread_mutex_init(&m_MutexLock, NULL);
#endif
#endif
}

// Destruct the MutexVariable
SimpleMutexLock::~SimpleMutexLock()
{
#ifdef ITK_USE_WIN32_THREADS
  CloseHandle(m_MutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_destroy(&m_MutexLock);
#endif
}

// Lock the MutexLock
void SimpleMutexLock::Lock()
{
#ifdef ITK_USE_WIN32_THREADS
  WaitForSingleObject(m_MutexLock, INFINITE);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_lock(&m_MutexLock);
#endif
}

// Unlock the MutexLock
void SimpleMutexLock::Unlock()
{
#ifdef ITK_USE_WIN32_THREADS
  ReleaseMutex(m_MutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_unlock(&m_MutexLock);
#endif
}

void MutexLock::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} //end namespace itk
