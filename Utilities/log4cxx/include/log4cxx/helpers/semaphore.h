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
 
#ifndef _LOG4CXX_HELPERS_SEMAPHORE_H
#define _LOG4CXX_HELPERS_SEMAPHORE_H

#include <log4cxx/config.h>
#include <log4cxx/helpers/exception.h>

#ifdef HAVE_PTHREAD
#include <semaphore.h>
#endif

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT SemaphoreException : public Exception
    {
    };

#ifdef HAVE_MS_THREAD
    class Condition;
#endif

    class LOG4CXX_EXPORT Semaphore
    {
#ifdef HAVE_MS_THREAD
    friend class Condition;
#endif
    public:
      Semaphore(int value = 0);
      ~Semaphore();
      void wait();
      bool tryWait();
      void post();

    protected:
#ifdef HAVE_PTHREAD
      sem_t semaphore;
#elif defined (HAVE_MS_THREAD)
      void * semaphore;
#endif
    };
  }  // namespace helpers
}; // namespace log4cxx

#endif //_LOG4CXX_HELPERS_SEMAPHORE_H
