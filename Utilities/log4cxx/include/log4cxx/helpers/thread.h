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
 
#ifndef _LOG4CXX_HELPERS_THREAD_H
#define _LOG4CXX_HELPERS_THREAD_H

#include <log4cxx/config.h>
#include <log4cxx/helpers/object.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/objectimpl.h>
#include <log4cxx/helpers/exception.h>
#include <log4cxx/mdc.h>

// Windows specific :
// winspool.h defines MIN_PRIORITY and MAX_PRIORITY
#ifdef MIN_PRIORITY
#define OLD_MIN_PRIORITY MIN_PRIORITY
#undef MIN_PRIORITY
#endif

#ifdef MAX_PRIORITY
#define OLD_MAX_PRIORITY MAX_PRIORITY
#undef MAX_PRIORITY
#endif

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT ThreadException : public Exception
    {
    };

    class LOG4CXX_EXPORT InterruptedException : public Exception
    {
    };
    
    /** The Runnable interface should be implemented by any class whose 
    instances are intended to be executed by a thread. 
    The class must define a method of no arguments called run.
    */
    class LOG4CXX_EXPORT Runnable : public virtual Object
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(Runnable)

      /** When an object implementing interface Runnable is used to
      create a thread, starting the thread causes the object's run 
      method to be called in that separately executing thread.
      */
      virtual void run() = 0;
    };

    typedef ObjectPtrT<Runnable> RunnablePtr;
    
    /** A thread is a thread of execution in a program.
    */
    class LOG4CXX_EXPORT Thread : public virtual ObjectImpl
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(Thread)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(Thread)
      END_LOG4CXX_CAST_MAP()

      /**  Allocates a new Thread object.*/
      Thread();
      
      /**  Allocates a new Thread object.*/
      Thread(RunnablePtr runnable);
      
      virtual ~Thread();

      /** Returns the current thread identifier
      */
      static unsigned long getCurrentThreadId();

      /** Causes the currently executing thread to sleep (temporarily
      cease execution) for the specified number of milliseconds.
      */
      static void sleep(long millis);

      /** Causes this thread to begin execution;
      calls the run method of this thread.
      */
      void start();

      /**  If this thread was constructed using a separate Runnable
      run object, then that Runnable object's run method is called;
      otherwise, this method does nothing and returns.
      */
      virtual void run();

      /** Waits for this thread to die.
      */
      void join();

      enum
      {
        MIN_PRIORITY = 1,
        NORM_PRIORITY = 2,
        MAX_PRIORITY = 3 
      };

      /** Changes the priority of this thread.
      */
      void setPriority(int newPriority);

      /**
      Atomic increment
      */
      static long InterlockedIncrement(volatile long * val);

      /**
      Atomic decrement
      */
      static long InterlockedDecrement(volatile long * val);
    
    protected:
      /** Thread descriptor */
#ifdef HAVE_PTHREAD
      pthread_t thread;
#elif defined(HAVE_MS_THREAD)
      void * thread;
#endif
      RunnablePtr runnable;
      MDC::Map parentMDCMap;
    };
    
    typedef ObjectPtrT<Thread> ThreadPtr;
  
  }  // namespace helpers
}; //namespace log4cxx

#endif // _LOG4CXX_HELPERS_THREAD_H
