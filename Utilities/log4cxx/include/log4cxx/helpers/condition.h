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
 
#ifndef _LOG4CXX_HELPERS_CONDITION_H
#define _LOG4CXX_HELPERS_CONDITION_H

#include <log4cxx/config.h>
#include <log4cxx/helpers/exception.h>
#include <log4cxx/helpers/mutex.h>
#include <log4cxx/helpers/semaphore.h>

#ifdef HAVE_PTHREAD
#include <pthread.h>
#endif

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT ConditionException : public Exception
    {
    };

    class LOG4CXX_EXPORT Condition
    {
    public:
      Condition();
      ~Condition();
      void broadcast();
      void signal();
      void wait(Mutex& mutex);
      void wait(Mutex& mutex, long timeOut);

    protected:
#ifdef HAVE_PTHREAD
      pthread_cond_t condition;
#elif defined(HAVE_MS_THREAD)
      /// Number of waiting threads.
      long volatile waiters;

      /// Queue up threads waiting for the condition to become signaled.
      Semaphore sema;

      /**
      * An auto reset event used by the broadcast/signal thread to wait
      * for the waiting thread(s) to wake up and get a chance at the
      * semaphore.
      */
      void * waitersDone;

      /// Keeps track of whether we were broadcasting or just signaling.
      bool wasBroadCast;
#endif
    };
  } // namespace helpers
};// namespace log4cxx

#endif //_LOG4CXX_HELPERS_CONDITION_H
