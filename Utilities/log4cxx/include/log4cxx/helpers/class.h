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
 
#ifndef _LOG4CXX_HELPERS_CLASS_H
#define _LOG4CXX_HELPERS_CLASS_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/objectptr.h>

namespace log4cxx
{
  namespace helpers
  {
    class Object;
    typedef ObjectPtrT<Object> ObjectPtr;

    /**
    Thrown when an application tries to create an instance of a class using
    the newInstance method in class Class, but the specified class object
    cannot be instantiated because it is an interface or is an abstract class.
    */
    class LOG4CXX_EXPORT InstantiationException : public Exception
    {
    public:
      InstantiationException() : Exception(_T("Abstract class")) {}
    };

    /**
    Thrown when an application tries to load in a class through its
    string name but no definition for the class with the specified name
    could be found.
    */
    class LOG4CXX_EXPORT ClassNotFoundException : public Exception
    {
    public:
      ClassNotFoundException(const String& className);
    };

    class LOG4CXX_EXPORT Class
    {
    public:
      Class(const String& name);
      virtual ObjectPtr newInstance() const;
      const String& toString() const;
      const String& getName() const;
      static const Class& forName(const String& className);

    protected:
      static void registerClass(const Class * newClass);
      String name;
    };
  }  // namespace log4cxx
}; // namespace helper

#endif //_LOG4CXX_HELPERS_CLASS_H
