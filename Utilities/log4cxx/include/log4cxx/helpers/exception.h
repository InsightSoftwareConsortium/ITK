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
 
#ifndef _LOG4CXX_HELPERS_EXCEPTION_H
#define _LOG4CXX_HELPERS_EXCEPTION_H

#include <log4cxx/helpers/tchar.h>

namespace log4cxx
{
  namespace helpers
  {
    /** The class Exception and its subclasses indicate conditions that a
    reasonable application might want to catch.
    */
    class LOG4CXX_EXPORT Exception
    {
    public:
      Exception() {}
      Exception(const String& message): message(message) {}
      inline const String& getMessage() { return message; }
      
    protected:
      String message;
      
  }; // class Exception

    /** RuntimeException is the parent class of those exceptions that can be
    thrown during the normal operation of the process.
    */
    class LOG4CXX_EXPORT RuntimeException : public Exception
    {
    public:
      RuntimeException() {}
      RuntimeException(const String& message)
       : Exception(message) {}
    }; // class RuntimeException

    /** Thrown when an application attempts to use null in a case where an
    object is required.
    */
    class LOG4CXX_EXPORT  NullPointerException : public RuntimeException
    {
    public:
      NullPointerException() {}
      NullPointerException(const String& message)
       : RuntimeException(message) {}
    }; // class NullPointerException

    /** Thrown to indicate that a method has been passed 
    an illegal or inappropriate argument.*/
    class LOG4CXX_EXPORT IllegalArgumentException : public RuntimeException
    {
    public:
      IllegalArgumentException(const String& message)
       : RuntimeException(message) {}
    }; // class IllegalArgumentException
    
    /** Signals that an I/O exception of some sort has occurred. This class
    is the general class of exceptions produced by failed or interrupted
    I/O operations.
    */
    class LOG4CXX_EXPORT IOException : public Exception
    {
    };
  }  // namespace helpers
}; // namespace log4cxx

#endif // _LOG4CXX_HELPERS_EXCEPTION_H
