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

#include <log4cxx/helpers/criticalsection.h>
#include <log4cxx/helpers/thread.h>

using namespace log4cxx::helpers;

CriticalSection::CriticalSection() : owningThread(0)
{
#ifdef HAVE_PTHREAD
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&mutex, &attr);
  pthread_mutexattr_destroy(&attr);
#elif defined(HAVE_MS_THREAD)
  InitializeCriticalSection(&mutex);
#endif            
}

CriticalSection::~CriticalSection()
{
#ifdef HAVE_PTHREAD
  pthread_mutex_destroy(&mutex);
#elif defined(HAVE_MS_THREAD)
  DeleteCriticalSection(&mutex);
#endif
}

void CriticalSection::lock()
{
#ifdef HAVE_PTHREAD
  pthread_mutex_lock(&mutex);
#elif defined(HAVE_MS_THREAD)
  EnterCriticalSection(&mutex);
#endif
  owningThread = Thread::getCurrentThreadId();
}

void CriticalSection::unlock()
{
  owningThread = 0;

#ifdef HAVE_PTHREAD
  pthread_mutex_unlock(&mutex);
#elif defined(HAVE_MS_THREAD)
  LeaveCriticalSection(&mutex);
#endif
}

unsigned long CriticalSection::getOwningThread()
{
  return owningThread;
}
