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
 
#ifndef _LOG4CXX_HELPERS_OBJECT_H
#define _LOG4CXX_HELPERS_OBJECT_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/class.h>
#include <log4cxx/helpers/objectptr.h>

#define DECLARE_ABSTRACT_LOG4CXX_OBJECT(object)\
public:\
class Class##object : public helpers::Class\
{\
public:\
  Class##object() : helpers::Class(_T(#object)) {}\
};\
virtual const helpers::Class& getClass() const;\
static const helpers::Class& getStaticClass();\
static Class##object theClass##object;

#define DECLARE_LOG4CXX_OBJECT(object)\
public:\
class Class##object : public helpers::Class\
{\
public:\
  Class##object() : helpers::Class(_T(#object)) {}\
  virtual helpers::ObjectPtr newInstance() const\
  {\
    return new object();\
  }\
};\
virtual const helpers::Class& getClass() const;\
static const helpers::Class& getStaticClass();\
static Class##object theClass##object;

#define DECLARE_LOG4CXX_OBJECT_WITH_CUSTOM_CLASS(object, class)\
public:\
virtual const helpers::Class& getClass() const;\
static const helpers::Class& getStaticClass();\
static class theClass##object;

#define IMPLEMENT_LOG4CXX_OBJECT(object)\
object::Class##object object::theClass##object;\
const log4cxx::helpers::Class& object::getClass() const { return theClass##object; }\
const log4cxx::helpers::Class& object::getStaticClass() { return theClass##object; }

#define IMPLEMENT_LOG4CXX_OBJECT_WITH_CUSTOM_CLASS(object, class)\
object::class object::theClass##object;\
const log4cxx::helpers::Class& object::getClass() const { return theClass##object; }\
const log4cxx::helpers::Class& object::getStaticClass() { return theClass##object; }

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT IllegalMonitorStateException : public Exception
    {
    public:
      IllegalMonitorStateException(const String& message) : Exception(message)
      {
      }
    };
    
    class Object;
    typedef ObjectPtrT<Object> ObjectPtr;

    /** base class for java-like objects.*/
    class LOG4CXX_EXPORT Object
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(Object)
      virtual ~Object() {}
      virtual void addRef() const = 0;
      virtual void releaseRef() const = 0;
      virtual void lock() const = 0;
      virtual void unlock() const = 0;
      virtual void wait() const = 0;
      virtual void notify() const = 0;
      virtual void notifyAll() const = 0;
      virtual bool instanceof(const Class& clazz) const = 0;
      virtual const void * cast(const Class& clazz) const = 0;
    };

    /** utility class for objects multi-thread synchronization.*/
    class synchronized
    {
    public:
      synchronized(const Object * object) : object(object)
        { object->lock(); }

      ~synchronized()
        { object->unlock(); }

    protected:
      const Object * object;
    };
  } 
} 

#define BEGIN_LOG4CXX_CAST_MAP()\
const void * cast(const helpers::Class& clazz) const\
{\
  const void * object = 0;\
  if (&clazz == &helpers::Object::getStaticClass()) return (helpers::Object *)this;

#define END_LOG4CXX_CAST_MAP()\
  return object;\
}\
bool instanceof(const helpers::Class& clazz) const\
{ return cast(clazz) != 0; }

#define LOG4CXX_CAST_ENTRY(Interface)\
if (&clazz == &Interface::getStaticClass()) return (Interface *)this;

#define LOG4CXX_CAST_ENTRY2(Interface, interface2)\
if (&clazz == &Interface::getStaticClass()) return (Interface *)(interface2 *)this;

#define LOG4CXX_CAST_ENTRY_CHAIN(Interface)\
object = Interface::cast(clazz);\
if (object != 0) return object;

#endif //_LOG4CXX_HELPERS_OBJECT_H
