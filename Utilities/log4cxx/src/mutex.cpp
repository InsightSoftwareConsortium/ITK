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

#ifdef HAVE_MS_THREAD
#include <windows.h>
#endif

#include <log4cxx/helpers/mutex.h>

using namespace log4cxx::helpers;
using namespace log4cxx;

Mutex::Mutex()
{
#ifdef HAVE_PTHREAD
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&mutex, &attr);
  pthread_mutexattr_destroy(&attr);
#elif defined(HAVE_MS_THREAD)
  mutex = ::CreateMutex(0, 0, 0);
#endif
}

Mutex::~Mutex()
{
#ifdef HAVE_PTHREAD
  pthread_mutex_destroy(&mutex);
#elif defined(HAVE_MS_THREAD)
  ::CloseHandle(mutex);
#endif
}

void Mutex::lock()
{
#ifdef HAVE_PTHREAD
  if (pthread_mutex_lock(&mutex) != 0)
  {
    throw MutexException();
  }
#elif defined(HAVE_MS_THREAD)
  if (::WaitForSingleObject(mutex, INFINITE) == WAIT_ABANDONED)
  {
    throw MutexException();
  }
#endif
}

void Mutex::unlock()
{
#ifdef HAVE_PTHREAD
  if (pthread_mutex_unlock(&mutex) != 0)
  {
    throw MutexException();
  }
#elif defined(HAVE_MS_THREAD)
  if (!::ReleaseMutex(mutex))
  {
    throw MutexException();
  }
#endif
}
