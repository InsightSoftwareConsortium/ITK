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
 
#include <log4cxx/spi/loggerfactory.h>
#include <log4cxx/spi/loggerrepository.h>
#include <log4cxx/helpers/optionconverter.h>
#include <algorithm>
#include <ctype.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/helpers/exception.h>
#include <stdlib.h>
#include <log4cxx/helpers/properties.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/level.h>
#include <log4cxx/helpers/object.h>
#include <log4cxx/helpers/class.h>
#include <log4cxx/helpers/loader.h>
#include <log4cxx/helpers/system.h>
#include <log4cxx/propertyconfigurator.h>
#include <log4cxx/xml/domconfigurator.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;
#ifdef HAVE_XML
using namespace log4cxx::xml;
#endif

String OptionConverter::DELIM_START = _T("${");
TCHAR OptionConverter::DELIM_STOP  = _T('}');
int OptionConverter::DELIM_START_LEN = 2;
int OptionConverter::DELIM_STOP_LEN  = 1;

namespace {
    // Function object to turn a lower case character into an upper case one
    class ToUpper {
    public:
        void operator()(TCHAR& c){c = toupper(c);}
    };
}

String OptionConverter::convertSpecialChars(const String& s)
{
  TCHAR c;
    int len = s.length();
    StringBuffer sbuf;
  
  String::const_iterator i = s.begin();
    while(i != s.end())
  {
    c = *i++;
    if (c == _T('\\'))
    {
      c =  *i++;

      switch (c)
      {
      case _T('n'):
        c = _T('\n');
        break;

      case _T('r'):
        c = _T('\r');
        break;

      case _T('t'):
        c = _T('\t');
        break;

      case _T('f'):
        c = _T('\f');
        break;

      case _T('\b'):
        c = _T('\b');
        break;

      case _T('\"'):
        c = _T('\"');
        break;

      case _T('\''):
        c = _T('\'');
        break;

      case _T('\\'):
        c = _T('\\');
        break;
      }
    }
    sbuf.put(c);
    }
    return sbuf.str();
}


bool OptionConverter::toBoolean(const String& value, bool dEfault)
{
  if (value.empty())
  {
    return dEfault;
  }

  String trimmedVal = StringHelper::toLowerCase(StringHelper::trim(value));

  if (trimmedVal == _T("true"))
  {
    return true;
  }
  if (trimmedVal == _T("false"))
  {
    return false;
  }

  return dEfault;
}

int OptionConverter::toInt(const String& value, int dEfault)
{
  if (value.empty())
  {
    return dEfault;
  }

  return (int)ttol(StringHelper::trim(value).c_str());
}

long OptionConverter::toFileSize(const String& value, long dEfault)
{
  if(value.empty())
  {
    return dEfault;
  }

  String s = StringHelper::toLowerCase(StringHelper::trim(value));

  long multiplier = 1;
  int index;
  
  if((index = s.find(_T("kb"))) != -1)
  {
    multiplier = 1024;
    s = s.substr(0, index);
  }
  else if((index = s.find(_T("mb"))) != -1) 
  {
    multiplier = 1024*1024;
    s = s.substr(0, index);
  }
  else if((index = s.find(_T("gb"))) != -1)
  {
    multiplier = 1024*1024*1024;
    s = s.substr(0, index);
  }
  if(!s.empty())
  {
    return ttol(s.c_str()) * multiplier;
  }

  return dEfault;
}

String OptionConverter::findAndSubst(const String& key, Properties& props)
{
  String value = props.getProperty(key);

  if(value.empty())
    return value;

  try
  {
    return substVars(value, props);
  }
  catch(IllegalArgumentException& e)
  {
    LogLog::error(_T("Bad option value [")+value+_T("]."), e);
    return value;
  }
}

String OptionConverter::substVars(const String& val, Properties& props)
{
  StringBuffer sbuf;

  int i = 0;
  int j, k;

  while(true)
  {
    j = val.find(DELIM_START, i);
    if(j == -1)
    {
      // no more variables
      if(i==0)
      { // this is a simple string
        return val;
      }
      else
      { // add the tail string which contails no variables and return the result.
        sbuf << val.substr(i, val.length() - i);
        return sbuf.str();
      }
    }
    else
    {
      sbuf << val.substr(i, j - i);
      k = val.find(DELIM_STOP, j);
      if(k == -1)
      {
        StringBuffer oss;
        oss << _T("\"") << val
          << _T("\" has no closing brace. Opening brace at position ")
          << j << _T(".");
        throw IllegalArgumentException(oss.str());
      }
      else
      {
        j += DELIM_START_LEN;
        String key = val.substr(j, k - j);
        // first try in System properties
        String replacement = getSystemProperty(key, _T(""));
        // then try props parameter
        if(replacement.empty())
        {
          replacement = props.getProperty(key);
        }

        if(!replacement.empty())
        {
          // Do variable substitution on the replacement string
          // such that we can solve "Hello ${x2}" as "Hello p1"
          // the where the properties are
          // x1=p1
          // x2=${x1}
          String recursiveReplacement = substVars(replacement, props);
          sbuf << (recursiveReplacement);
        }
        i = k + DELIM_STOP_LEN;
      }
    }
  }
}

String OptionConverter::getSystemProperty(const String& key, const String& def)
{
  if (!key.empty())
  {
    String value = System::getProperty(key);

    if (!value.empty())
    {
      return value;
    }
    else
    {
      return def;
    }
  }
  else
  {
    return def;
  }
}

const LevelPtr& OptionConverter::toLevel(const String& value,
  const LevelPtr& defaultValue)
{
    int hashIndex = value.find(_T("#"));

  if (hashIndex == -1)
  {
    if (value.empty())
    {
      return defaultValue;
    }
    else
    {
      LogLog::debug(
      _T("OptionConverter::toLevel: no class name specified, level=[")
      +value+_T("]"));
      // no class name specified : use standard Level class
      return Level::toLevel(value, defaultValue);
    }
  }

  const LevelPtr& result = defaultValue;

  String clazz = value.substr(hashIndex + 1);
  String levelName = value.substr(0, hashIndex);
  LogLog::debug(_T("OptionConverter::toLevel: class=[") +clazz+_T("], level=[")+
    levelName+_T("]"));

  // This is degenerate case but you never know.
  if (levelName.empty())
  {
    return Level::toLevel(value, defaultValue);
  }

  try
  {
    Level::LevelClass& levelClass =
      (Level::LevelClass&)Loader::loadClass(clazz);
    return levelClass.toLevel(levelName);
  }
  catch (ClassNotFoundException&)
  {
    LogLog::warn(_T("custom level class [") + clazz + _T("] not found."));
  }
  catch(Exception& oops)
  {
    LogLog::warn(
      _T("class [") + clazz + _T("], level [") + levelName +
      _T("] conversion) failed."), oops);
  }
  catch(...)
  {
    LogLog::warn(
      _T("class [") + clazz + _T("], level [") + levelName +
      _T("] conversion) failed."));
  }

  return defaultValue;
}


ObjectPtr OptionConverter::instantiateByKey(Properties& props, const String& key,
  const Class& superClass, const ObjectPtr& defaultValue)
{
  // Get the value of the property in string form
  String className = findAndSubst(key, props);
  if(className.empty())
  {
    LogLog::error(_T("Could not find value for key ") + key);
    return defaultValue;
  }

  // Trim className to avoid trailing spaces that cause problems.
  return OptionConverter::instantiateByClassName(
    StringHelper::trim(className), superClass, defaultValue);
}

ObjectPtr OptionConverter::instantiateByClassName(const String& className,
  const Class& superClass, const ObjectPtr& defaultValue)
{
  if(!className.empty())
  {
    try
    {
      const Class& classObj = Loader::loadClass(className);
      ObjectPtr newObject =  classObj.newInstance();
      if (!newObject->instanceof(superClass))
      {
        return defaultValue;
      }

      return newObject;
    }
    catch (Exception& e)
    {
      LogLog::error(_T("Could not instantiate class [") + className
        + _T("]."), e);
    }
  }
  return defaultValue;
}

void OptionConverter::selectAndConfigure(const String& configFileName,
   const String& _clazz, spi::LoggerRepositoryPtr& hierarchy)
{
  ConfiguratorPtr configurator;
  String clazz = _clazz;
  
#ifdef HAVE_XML
  if(clazz.empty() && !configFileName.empty() 
    && StringHelper::endsWith(configFileName, _T(".xml")))
  {
    clazz = DOMConfigurator::getStaticClass().toString();
  }
#endif
  
  if(!clazz.empty())
  {
    LogLog::debug(_T("Preferred configurator class: ") + clazz);
    configurator = instantiateByClassName(clazz,
      Configurator::getStaticClass(),
      0);
    if(configurator == 0)
    {
      LogLog::error(_T("Could not instantiate configurator [")+
        clazz+_T("]."));
      return;
    }
  } 
  else
  {
    configurator = new PropertyConfigurator();
  }
  
  configurator->doConfigure(configFileName, hierarchy);
}
