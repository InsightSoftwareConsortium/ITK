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
#include <log4cxx/config/propertysetter.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/spi/optionhandler.h>
#include <log4cxx/helpers/properties.h>
#include <log4cxx/appender.h>
#include <log4cxx/layout.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;
using namespace log4cxx::config;

PropertySetter::PropertySetter(helpers::ObjectPtr obj) : obj(obj)
{
}

void PropertySetter::setProperties(helpers::ObjectPtr obj, helpers::Properties& properties, const String& prefix)
{
  PropertySetter(obj).setProperties(properties, prefix);
}


void PropertySetter::setProperties(helpers::Properties& properties, const String& prefix)
{
  int len = prefix.length();

  std::vector<String> names = properties.propertyNames();
  std::vector<String>::iterator it;

  for (it = names.begin(); it != names.end(); it++)
  {
    String key = *it;
    
    // handle only properties that start with the desired frefix.
    if (key.find(prefix) == 0)
    {
      // ignore key if it contains dots after the prefix
      if (key.find(_T('.'), len + 1) != String::npos)
      {
        continue;
      }
      
      String value = OptionConverter::findAndSubst(key, properties);
      key = key.substr(len);
      if (key == _T("layout")
        && obj->instanceof(Appender::getStaticClass()))
      {
        continue;
      }        
      setProperty(key, value);
    }
  }
  activate();
}

void PropertySetter::setProperty(const String& name, const String& value)
{
  if (value.empty())
    return;
  
  if (obj->instanceof(OptionHandler::getStaticClass()))
  {
    LogLog::debug(_T("Setting option name=[") + 
      name + _T("], value=[") + value + _T("]"));
    OptionHandlerPtr(obj)->setOption(name, value);
  }
}

void PropertySetter::activate()
{
  if (obj->instanceof(OptionHandler::getStaticClass()))
  {
    OptionHandlerPtr(obj)->activateOptions();
  }
}
