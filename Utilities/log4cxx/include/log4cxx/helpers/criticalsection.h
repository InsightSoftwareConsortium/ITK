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
 
#ifndef _LOG4CXX_HELPERS_CRITICAL_SECTION_H
#define _LOG4CXX_HELPERS_CRITICAL_SECTION_H

#include <log4cxx/config.h>

#ifdef HAVE_PTHREAD
#include <pthread.h>
#elif defined(HAVE_MS_THREAD)
#include <windows.h>
#endif

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT CriticalSection
    {
    public:
      CriticalSection();
      ~CriticalSection();
      void lock();
      void unlock();
      unsigned long getOwningThread();

#ifdef HAVE_PTHREAD
      pthread_mutex_t mutex;
#elif defined(HAVE_MS_THREAD)
      CRITICAL_SECTION mutex;
#endif            
      unsigned long owningThread;
    };

    /** CriticalSection helper class to be used on call stack
    */
    class WaitAccess
    {
    public:
      /// lock a critical section
      WaitAccess(CriticalSection& cs) : cs(cs)
      {
        cs.lock();
        locked = true;
      }

      /** automatically unlock the critical section
      if unlock has not be called.
      */
      ~WaitAccess()
      {
        if (locked)
        {
          unlock();
        }
      }

      /// unlock the critical section
      void unlock()
      {
        cs.unlock();
        locked = false;
      }

    private:
      /// the CriticalSection to be automatically unlocked
      CriticalSection& cs;
      /// verify the CriticalSection state
      bool locked;
    };
  }  // namespace helpers
}; // namespace log4cxx

#endif //_LOG4CXX_HELPERS_CRITICAL_SECTION_H
