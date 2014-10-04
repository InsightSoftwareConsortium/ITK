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
// Construct a new MutexLock
SimpleMutexLock::SimpleMutexLock()
{
  pthread_mutex_init(&m_MutexLock, ITK_NULLPTR);
}

// Destruct the MutexVariable
SimpleMutexLock::~SimpleMutexLock()
{
  pthread_mutex_destroy(&m_MutexLock);
}

// Lock the MutexLock
void SimpleMutexLock::Lock()
{
  pthread_mutex_lock(&m_MutexLock);
}
//
// Lock the MutexLock
bool SimpleMutexLock::TryLock()
{
  const bool lockCaptured = ( pthread_mutex_trylock(&m_MutexLock) == 0 );
  /*
   * non-blocking lock of mutex
   * - if mutex is not already locked, you will obtain the lock & own the mutex, and return 0 immediately
   * - if mutex is already locked, pthread_mutex_trylock() will return immediately wth return value EBUSY
   */
  return lockCaptured;
}

// Unlock the MutexLock
void SimpleMutexLock::Unlock()
{
  pthread_mutex_unlock(&m_MutexLock);
}

} //end namespace itk
