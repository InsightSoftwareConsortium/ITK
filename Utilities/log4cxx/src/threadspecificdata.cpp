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
#include <pthread.h>
#elif defined(HAVE_MS_THREAD)
#include <windows.h>
#endif

#include <log4cxx/helpers/threadspecificdata.h>

using namespace log4cxx::helpers;

ThreadSpecificData::ThreadSpecificData() : key(0)
{
#ifdef HAVE_PTHREAD
  pthread_key_create(&key, NULL);
#elif defined(HAVE_MS_THREAD)
  key = (void *)TlsAlloc();
#endif
}

ThreadSpecificData::~ThreadSpecificData()
{
#ifdef HAVE_PTHREAD
  pthread_key_delete(key);
#elif defined(HAVE_MS_THREAD)
  TlsFree((DWORD)key);
#endif
}

void * ThreadSpecificData::GetData() const
{
#ifdef HAVE_PTHREAD
  return pthread_getspecific((pthread_key_t)key);
#elif defined(HAVE_MS_THREAD)
  return TlsGetValue((DWORD)key);
#else
  return key;
#endif
}

void ThreadSpecificData::SetData(void * data)
{
#ifdef HAVE_PTHREAD
  pthread_setspecific((pthread_key_t)key, data);
#elif defined(HAVE_MS_THREAD)
  TlsSetValue((DWORD)key, data);
#else
  key = data;
#endif
}
