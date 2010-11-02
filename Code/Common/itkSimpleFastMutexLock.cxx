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
#include "itkSimpleFastMutexLock.h"

namespace itk
{
// Construct a new SimpleMutexLock
SimpleFastMutexLock::SimpleFastMutexLock()
{
#if defined( _WIN32 ) && !defined( ITK_USE_PTHREADS )
  //this->MutexLock = CreateMutex( NULL, FALSE, NULL );
  InitializeCriticalSection(&m_FastMutexLock);
#endif

#ifdef ITK_USE_PTHREADS
#ifdef ITK_HP_PTHREADS
  pthread_mutex_init(&( m_FastMutexLock ), pthread_mutexattr_default);
#else
  pthread_mutex_init(&( m_FastMutexLock ), NULL);
#endif
#endif
}

// Destruct the SimpleMutexVariable
SimpleFastMutexLock::~SimpleFastMutexLock()
{
#if defined( _WIN32 ) && !defined( ITK_USE_PTHREADS )
  //CloseHandle(this->MutexLock);
  DeleteCriticalSection(&m_FastMutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_destroy(&m_FastMutexLock);
#endif
}

// Lock the FastMutexLock
void SimpleFastMutexLock::Lock() const
{
#if defined( _WIN32 ) && !defined( ITK_USE_PTHREADS )
  //WaitForSingleObject( this->MutexLock, INFINITE );
  EnterCriticalSection(&m_FastMutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_lock(&m_FastMutexLock);
#endif
}

// Unlock the FastMutexLock
void SimpleFastMutexLock::Unlock() const
{
#if defined( _WIN32 ) && !defined( ITK_USE_PTHREADS )
  //ReleaseMutex( this->MutexLock );
  LeaveCriticalSection(&m_FastMutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_unlock(&m_FastMutexLock);
#endif
}
} //end namespace itk
