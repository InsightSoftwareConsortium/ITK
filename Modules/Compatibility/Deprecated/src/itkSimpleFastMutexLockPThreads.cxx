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
  pthread_mutexattr_t mta;
  pthread_mutexattr_init(&mta);
  pthread_mutexattr_settype(&mta, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&( m_FastMutexLock ), &mta);
}

// Destruct the SimpleMutexVariable
SimpleFastMutexLock::~SimpleFastMutexLock()
{
  pthread_mutex_destroy(&m_FastMutexLock);
}

// Lock the FastMutexLock
void SimpleFastMutexLock::Lock() const
{
  pthread_mutex_lock(&m_FastMutexLock);
}

// Non-blocking TryLock the FastMutexLock
bool SimpleFastMutexLock::TryLock() const
{
  const bool lockCaptured = ( pthread_mutex_trylock(&m_FastMutexLock) == 0 );
  /*
   * non-blocking lock of mutex
   * - if mutex is not already locked, you will obtain the lock & own the mutex, and return 0 immediately
   * - if mutex is already locked, pthread_mutex_trylock() will return immediately wth return value EBUSY
   */
  return lockCaptured;
}

// Unlock the FastMutexLock
void SimpleFastMutexLock::Unlock() const
{
  pthread_mutex_unlock(&m_FastMutexLock);
}
} //end namespace itk
