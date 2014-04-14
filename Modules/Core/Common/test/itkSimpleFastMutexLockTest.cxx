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


#include "itkSimpleFastMutexLock.h"


int itkSimpleFastMutexLockTest( int , char * [] )
{
  itk::SimpleFastMutexLock lock;

  // Acquire the lock
  lock.Lock();

  // Acquire the lock again; as this call happens within the same thread,
  // it shouldn't block.
  // If the lock isn't recursive, it will block forever and the CTest
  // timeout will cause the test to fail.
  lock.Lock();

  return EXIT_SUCCESS;
}
