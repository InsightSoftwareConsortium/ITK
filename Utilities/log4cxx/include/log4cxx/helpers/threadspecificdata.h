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
 
#ifndef _LOG4CXX_HELPERS_THREAD_SPECIFIC_DATA_H
#define _LOG4CXX_HELPERS_THREAD_SPECIFIC_DATA_H

#include <log4cxx/config.h>

#ifdef HAVE_PTHREAD
#include <pthread.h>
#endif

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT ThreadSpecificData
    {
    public:
      ThreadSpecificData();
      ~ThreadSpecificData();
      void * GetData() const;
      void SetData(void * data);

    protected:
#ifdef HAVE_PTHREAD
      pthread_key_t key;
#elif defined(HAVE_MS_THREAD)
      void * key;
#endif
    };
  }  // namespace helpers
}; // namespace log4cxx

#endif
