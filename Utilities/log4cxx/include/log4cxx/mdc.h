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
 
#ifndef _LOG4CXX_MDC_H
#define _LOG4CXX_MDC_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/threadspecificdata.h>
#include <map>

namespace log4cxx
{
  /**
  The MDC class is similar to the {@link NDC} class except that it is
  based on a map instead of a stack. It provides <em>mapped
  diagnostic contexts</em>. A <em>Mapped Diagnostic Context</em>, or
  MDC in short, is an instrument for distinguishing interleaved log
  output from different sources. Log output is typically interleaved
  when a server handles multiple clients near-simultaneously.

  <p><b><em>The MDC is managed on a per thread basis</em></b>. A
  child thread automatically inherits a <em>copy</em> of the mapped
  diagnostic context of its parent.

  <p>The MDC class requires JDK 1.2 or above. Under JDK 1.1 the MDC
  will always return empty values but otherwise will not affect or
  harm your application.
  */
  class LOG4CXX_EXPORT MDC
  {
  public:
    /** String to string stl mp
    */
    typedef std::map<String, String> Map;

  private:
    static Map * getCurrentThreadMap();
    static void setCurrentThreadMap(Map * map);

    static helpers::ThreadSpecificData threadSpecificData;
    const String& key;

  public:
    MDC(const String& key, const String& value);
    ~MDC();

    /**
    * Put a context value (the <code>o</code> parameter) as identified
    * with the <code>key</code> parameter into the current thread's
    * context map.
    *
    * <p>If the current thread does not have a context map it is
    * created as a side effect.
    * */
      static void put(const String& key, const String& value);

    /**
    * Get the context identified by the <code>key</code> parameter.
    *
    *  <p>This method has no side effects.
    * */
    static String get(const String& key);

    /**
    * Remove the the context identified by the <code>key</code>
    * parameter. */
    static String remove(const String& key);

    /**
    * Clear all entries in the MDC.
    */
    static void clear();

    /**
    * Get the current thread's MDC as a Map. This method is
    * intended to be used internally.
    * */
    static const Map getContext();
    
    static void setContext(Map& map);
  }; // class MDC;
}  // namespace log4cxx

#endif // _LOG4CXX_MDC_H
