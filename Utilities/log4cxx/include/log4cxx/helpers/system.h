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
 
 #ifndef _LOG4CXX_HELPERS_SYSTEM_H
 #define _LOG4CXX_HELPERS_SYSTEM_H

 #include <log4cxx/config.h>
 #include <log4cxx/helpers/tchar.h>
 #include <log4cxx/helpers/exception.h>

 namespace log4cxx
 {
   namespace helpers
  {
    class Properties;
    
    /** The System class contains several useful class fields and methods.
    It cannot be instantiated.
    */
    class LOG4CXX_EXPORT System
    {
    public:
    /** Returns the current time in milliseconds since midnight (0 hour),
    January 1, 1970.
    
    Returns the current time in milliseconds. Note that while the unit of
    time of the return value is a millisecond, the granularity of the value
    depends on the underlying operating system and may be larger. For
    example, many operating systems measure time in units of tens of
    milliseconds.
    
    @return the difference, measured in milliseconds, between the current
    time and midnight, January 1, 1970 UTC.
    */
    static int64_t currentTimeMillis();

    /** 
    Gets the system property indicated by the specified key.
    
    @param key the name of the system property.
    
    @return the string value of the system property, or the default value if
    there is no property with that key.
    
    @throws IllegalArgumentException if key is empty.
    */
    static String getProperty(const String& key);
    
    /**
    Sets the system property indicated by the specified key.

    @param key the name of the system property.
    @param value the value of the system property.

    @throws IllegalArgumentException if key is empty.
    */
    static void setProperty(const String& key, const String& value);
    
    static void setProperties(const Properties& props);
    };
  } // namespace helpers
 }; //  namespace log4cxx

 #endif //_LOG4CXX_HELPERS_SYSTEM_H
