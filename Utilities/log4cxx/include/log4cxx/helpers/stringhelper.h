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
 
#ifndef _LOG4CXX_HELPERS_STRING_HELPER_H
#define _LOG4CXX_HELPERS_STRING_HELPER_H
 
#include <log4cxx/config.h>
#include <log4cxx/helpers/tchar.h>
#include <stdarg.h>

namespace log4cxx
{
    namespace helpers
    {
    /** 
    String manipulation routines
    */
        class LOG4CXX_EXPORT StringHelper
        {
           public:
            static String toUpperCase(const String& s);
            static String toLowerCase(const String& s);
            static String trim(const String& s);
      static bool equalsIgnoreCase(const String& s1, const String& s2);
        static bool endsWith(const String& s, const String& suffix);
      /** 
      Creates a message with the given pattern and uses it to format the
      given arguments.
      
      This method provides a means to produce concatenated messages in
      language-neutral way.
      
      @param pattern the pattern for this message. The different arguments
      are represented in the pattern string by the symbols {0} to {9}
      
      @param argList a variable list of srrings to be formatted and
      substituted. The type of the strings must be (TCHAR *).
      */
      static String format(const String& pattern, va_list argList);
        };
    } 
} 

#endif //_LOG4CXX_HELPERS_STRING_HELPER_H
