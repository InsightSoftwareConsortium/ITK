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
 
#ifndef _LOG4CXX_HELPERS_MUTEX_H
#define _LOG4CXX_HELPERS_MUTEX_H

#include <log4cxx/config.h>
#include <log4cxx/helpers/exception.h>

#ifdef HAVE_PTHREAD
#include <pthread.h>
#endif

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT MutexException : public Exception
    {
    };

    class Condition;

    class LOG4CXX_EXPORT Mutex
    {
    friend class Condition;
    public:
      Mutex();
      ~Mutex();
      void lock();
      void unlock();

    protected:
#ifdef HAVE_PTHREAD
      pthread_mutex_t mutex;
#elif defined(HAVE_MS_THREAD)
      void * mutex;
#endif
    };
  } // namespace helpers
};// namespace log4cxx

#endif //_LOG4CXX_HELPERS_MUTEX_H
