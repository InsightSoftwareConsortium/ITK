/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
#include <log4cxx/config.h>

#ifdef HAVE_PTHREAD
#include <semaphore.h>
#elif defined(HAVE_MS_THREAD)
#include <windows.h>
#include <limits.h>
#endif

#include <log4cxx/helpers/semaphore.h>

using namespace log4cxx::helpers;

Semaphore::Semaphore(int value)
{
#ifdef HAVE_PTHREAD
  if (::sem_init(&semaphore, 0, value) != 0)
  {
    throw SemaphoreException();
  }
#elif defined(HAVE_MS_THREAD)
  semaphore = ::CreateSemaphore(0, (long)value, LONG_MAX, 0);
  if (semaphore == 0)
  {
    throw SemaphoreException();
  }
#endif            
}

Semaphore::~Semaphore()
{
#ifdef HAVE_PTHREAD
  ::sem_destroy(&semaphore);
#elif defined(HAVE_MS_THREAD)
  ::CloseHandle(semaphore);
#endif
}

void Semaphore::wait()
{
#ifdef HAVE_PTHREAD
  if (::sem_wait(&semaphore) != 0)
  {
    throw SemaphoreException();
  }
#elif defined(HAVE_MS_THREAD)
  if (::WaitForSingleObject(semaphore, INFINITE) != WAIT_OBJECT_0)
  {
    throw SemaphoreException();
  }
#endif
}

bool Semaphore::tryWait()
{
#ifdef HAVE_PTHREAD
  return (::sem_trywait(&semaphore) == 0);
#elif defined(HAVE_MS_THREAD)
  bool bSuccess;
  switch(::WaitForSingleObject(semaphore, 0))
  {
  case WAIT_OBJECT_0:
    bSuccess = true;
    break;
  case WAIT_TIMEOUT:
    bSuccess = false;
    break;
  default:
    throw SemaphoreException();
    break;
  }
  return bSuccess;
#endif
}

void Semaphore::post()
{
#ifdef HAVE_PTHREAD
  if (::sem_post(&semaphore) != 0)
  {
    throw SemaphoreException();
  }
#elif defined(HAVE_MS_THREAD)
  long previousCount;
  if (!::ReleaseSemaphore(semaphore, 1, &previousCount))
  {
    throw SemaphoreException();
  }
#endif
}

