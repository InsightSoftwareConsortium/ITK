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

#include <log4cxx/helpers/object.h>
#include <log4cxx/helpers/class.h>
#include <map>
#include <log4cxx/helpers/stringhelper.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

typedef std::map<String, const Class *> classMap;
classMap * registry = 0;

class RegistryDestructor
{
public:
  ~RegistryDestructor()
  {
    if (registry != 0)
    {
      delete registry;
    }
  }
} registryDestructor;

ClassNotFoundException::ClassNotFoundException(const String& className)
{
  message = _T("Class '") + className + _T("' not found");
}

Class::Class(const String& name) : name(name)
{
  registerClass(this);
}

const String& Class::toString() const
{
  return name;
}

const String& Class::getName() const
{
  return name;
}

ObjectPtr Class::newInstance() const
{
  throw InstantiationException();
  return 0;
}

const Class& Class::forName(const String& className)
{
  String strippedClassName;
  String::size_type pos = className.find_last_of(_T('.'));
  if (pos != String::npos)
  {
    strippedClassName = className.substr(pos + 1);
  }
  else
  {
    strippedClassName = className;
  }

  const Class * clazz = (*registry)[StringHelper::toLowerCase(strippedClassName)];

  if (clazz == 0)
  {
    throw ClassNotFoundException(className);
  }

  return *clazz;
}

void Class::registerClass(const Class * newClass)
{
  if (newClass == 0)
  {
    return;
  }

  if (registry == 0)
  {
    registry = new classMap();
  }

  (*registry)[StringHelper::toLowerCase(newClass->toString())] = newClass;
}
