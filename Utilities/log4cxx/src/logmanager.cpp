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
 
#include <log4cxx/logmanager.h>
#include <log4cxx/spi/defaultrepositoryselector.h>
#include <log4cxx/hierarchy.h>
#include <log4cxx/spi/rootcategory.h>
#include <log4cxx/spi/loggerfactory.h>
#include <stdexcept>
#include <log4cxx/level.h>
#include <log4cxx/spi/loggerrepository.h>
#include <log4cxx/helpers/exception.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/loglog.h>
#include <sys/stat.h>

#define DEFAULT_CONFIGURATION_FILE _T("log4j.properties")
#define DEFAULT_XML_CONFIGURATION_FILE _T("log4j.xml")
#define DEFAULT_CONFIGURATION_KEY _T("log4j.configuration")
#define CONFIGURATOR_CLASS_KEY _T("log4j.configuratorClass")

using namespace log4cxx;
using namespace log4cxx::spi;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(DefaultRepositorySelector)

void * LogManager::guard = 0;
RepositorySelectorPtr LogManager::repositorySelector;

void LogManager::setRepositorySelector(spi::RepositorySelectorPtr selector,
  void * guard)
{
  if((LogManager::guard != 0) && (LogManager::guard != guard))
  {
    throw IllegalArgumentException(
    _T("Attempted to reset the LoggerFactory without possessing the guard."));
  }

  if(selector == 0)
  {
    throw IllegalArgumentException(
    _T("RepositorySelector must be non-null."));
  }

  LogManager::guard = guard;
  LogManager::repositorySelector = selector;
}

LoggerRepositoryPtr& LogManager::getLoggerRepository()
{
  if (repositorySelector == 0)
  {
    repositorySelector =
      new DefaultRepositorySelector(
        new Hierarchy(
          new RootCategory(Level::DEBUG)));
    
    // Use automatic configration to configure the default hierarchy
    String configuratorClassName =
      OptionConverter::getSystemProperty(CONFIGURATOR_CLASS_KEY,_T(""));
    String configurationOptionStr =
      OptionConverter::getSystemProperty(DEFAULT_CONFIGURATION_KEY,_T(""));
    
    struct stat buff;
    USES_CONVERSION;
    
    if (configurationOptionStr.empty())
    {
      configurationOptionStr = DEFAULT_XML_CONFIGURATION_FILE;
      if (stat(T2A(configurationOptionStr.c_str()), &buff) == -1)
      {
        configurationOptionStr = DEFAULT_CONFIGURATION_FILE;
      }
    }
    
    if (stat(T2A(configurationOptionStr.c_str()), &buff) == 0)
    {
      LogLog::debug(
        _T("Using configuration file [") +configurationOptionStr 
        + _T("] for automatic log4cxx configuration"));
      
      OptionConverter::selectAndConfigure(
        configurationOptionStr, 
        configuratorClassName,
        repositorySelector->getLoggerRepository()); 
    }
    else
    {
      LogLog::debug(
        _T("Could not find configuration file: [")
        +configurationOptionStr+_T("]."));
    }
  }

  return repositorySelector->getLoggerRepository();
}

LoggerPtr LogManager::getRootLogger()
{
  // Delegate the actual manufacturing of the logger to the logger repository.
  return getLoggerRepository()->getRootLogger();
}

/**
Retrieve the appropriate Logger instance.
*/
LoggerPtr LogManager::getLogger(const String& name)
{
  // Delegate the actual manufacturing of the logger to the logger repository.
  return getLoggerRepository()->getLogger(name);
}

/**
Retrieve the appropriate Logger instance.
*/
LoggerPtr LogManager::getLogger(const String& name,
                spi::LoggerFactoryPtr factory)
{
  // Delegate the actual manufacturing of the logger to the logger repository.
  return getLoggerRepository()->getLogger(name, factory);
}

LoggerPtr LogManager::exists(const String& name)
{
  return getLoggerRepository()->exists(name);
}

LoggerList LogManager::getCurrentLoggers()
{
  return getLoggerRepository()->getCurrentLoggers();
}

void LogManager::shutdown()
{
  getLoggerRepository()->shutdown();
}

void LogManager::resetConfiguration()
{
  getLoggerRepository()->resetConfiguration();
}
