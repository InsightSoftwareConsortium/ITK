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
#include <log4cxx/hierarchy.h>
#include <log4cxx/defaultcategoryfactory.h>
#include <log4cxx/logger.h>
#include <log4cxx/spi/hierarchyeventlistener.h>
#include <log4cxx/level.h>
#include <algorithm>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/appender.h>

using namespace log4cxx;
using namespace log4cxx::spi;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(Hierarchy)

namespace {
    bool startsWith(const String& teststr, const String& substr)
  {
        bool val = false;
        if(teststr.length() > substr.length()) {
            val = teststr.substr(0, substr.length()) == substr;
        }

        return val;
    }
}

Hierarchy::Hierarchy(const LoggerPtr& root) : root(root),
emittedNoAppenderWarning(false), emittedNoResourceBundleWarning(false)
{
  // Enable all level levels by default.
  setThreshold(Level::ALL);
  this->root->setHierarchy(this);
  defaultFactory = new DefaultCategoryFactory();
}

Hierarchy::~Hierarchy()
{
}

void Hierarchy::addHierarchyEventListener(const spi::HierarchyEventListenerPtr& listener)
{
  if (std::find(listeners.begin(), listeners.end(), listener) != listeners.end())
  {
    LogLog::warn(_T("Ignoring attempt to add an existent listener."));
  } 
  else
  {
    listeners.push_back(listener);
  }
}

void Hierarchy::clear()
{
  mapCs.lock();

  loggers.clear();
  
  mapCs.unlock();
}

void Hierarchy::emitNoAppenderWarning(const LoggerPtr& logger)
{
  // No appenders in hierarchy, warn user only once.
  if(!this->emittedNoAppenderWarning)
  {
    LogLog::warn(_T("No appenders could be found for logger (") +
      logger->getName() + _T(")."));
    LogLog::warn(_T("Please initialize the log4cxx system properly."));
    this->emittedNoAppenderWarning = true;
  }
}


LoggerPtr Hierarchy::exists(const String& name)
{
  mapCs.lock();
  
  LoggerPtr logger;
  LoggerMap::iterator it = loggers.find(name);
  if (it != loggers.end())
  {
    logger = it->second;
  }

  mapCs.unlock();
  
  return logger;
}
  
void Hierarchy::setThreshold(const LevelPtr& l)
{
  if (l != 0)
  {
    thresholdInt = l->level;
    threshold = l;
  }
}

void Hierarchy::setThreshold(const String& levelStr)

{
  const LevelPtr& l = Level::toLevel(levelStr, 0);

  if(l != 0)
  {
    setThreshold(l);
  } 
  else
  {
    LogLog::warn(_T("Could not convert [")+levelStr+_T("] to Level."));
  }
}

void Hierarchy::fireAddAppenderEvent(const LoggerPtr& logger, const AppenderPtr& appender)
{
    HierarchyEventListenerList::iterator it, itEnd = listeners.end();
    HierarchyEventListenerPtr listener;

    for(it = listeners.begin(); it != itEnd; it++)
  {
    listener = *it;
    listener->addAppenderEvent(logger, appender);
  }
}

void Hierarchy::fireRemoveAppenderEvent(const LoggerPtr& logger, const AppenderPtr& appender)

{
    HierarchyEventListenerList::iterator it, itEnd = listeners.end();
    HierarchyEventListenerPtr listener;

    for(it = listeners.begin(); it != itEnd; it++)
  {
    listener = *it;
    listener->removeAppenderEvent(logger, appender);
  }
}

const LevelPtr& Hierarchy::getThreshold() const
{
  return threshold;
}

LoggerPtr Hierarchy::getLogger(const String& name)
{
  return getLogger(name, defaultFactory);
}

LoggerPtr Hierarchy::getLogger(const String& name, spi::LoggerFactoryPtr factory)
{
  // Synchronize to prevent write conflicts. Read conflicts (in
  // getEffectiveLevel method) are possible only if variable
  // assignments are non-atomic.
  LoggerPtr logger;

  mapCs.lock();

  LoggerMap::iterator it = loggers.find(name);
  
  if (it != loggers.end())
  {
    logger = it->second;
  }
  else
  {
    logger = factory->makeNewLoggerInstance(name);

    logger->setHierarchy(this);
    loggers.insert(LoggerMap::value_type(name, logger));

    ProvisionNodeMap::iterator it2 = provisionNodes.find(name);
    if (it2 != provisionNodes.end())
    {
      updateChildren(it2->second, logger);
      provisionNodes.erase(it2);
    }

    updateParents(logger);
  }

  mapCs.unlock();

  return logger;
}

LoggerList Hierarchy::getCurrentLoggers() const
{
  mapCs.lock();

  LoggerList v;
  LoggerMap::const_iterator it, itEnd = loggers.end();

  for (it = loggers.begin(); it != itEnd; it++)
  {
    v.push_back(it->second);
  }

  mapCs.unlock();

  return v;
}

LoggerPtr Hierarchy::getRootLogger() const
{
  return root;
}

bool Hierarchy::isDisabled(int level) const
{
  return thresholdInt > level;
}


void Hierarchy::resetConfiguration()
{
  mapCs.lock();
  
  getRootLogger()->setLevel(Level::DEBUG);
  root->setResourceBundle(0);
  setThreshold(Level::ALL);
  
  shutdown(); // nested locks are OK
  
  LoggerList loggers = getCurrentLoggers();
  LoggerList::iterator it, itEnd = loggers.end();

  for (it = loggers.begin(); it != itEnd; it++)
  {
    LoggerPtr& logger = *it;
    logger->setLevel(0);
    logger->setAdditivity(true);
    logger->setResourceBundle(0);
  }

  //rendererMap.clear();

  mapCs.unlock();
}

void Hierarchy::shutdown()
{
  LoggerPtr root = getRootLogger();
  
  // begin by closing nested appenders
  root->closeNestedAppenders();
  
  LoggerList loggers = getCurrentLoggers();
  LoggerList::iterator it, itEnd = loggers.end();

  for (it = loggers.begin(); it != itEnd; it++)
  {
    LoggerPtr& logger = *it;
    logger->closeNestedAppenders();
  }

  // then, remove all appenders
  root->removeAllAppenders();
  for (it = loggers.begin(); it != itEnd; it++)
  {
    LoggerPtr& logger = *it;
    logger->removeAllAppenders();
  }
}


void Hierarchy::updateParents(LoggerPtr& logger)
{
  const String& name = logger->name;
  int length = name.size();
  bool parentFound = false;

  //tcout << _T("UpdateParents called for ") << name << std::endl;

  // if name = "w.x.y.z", loop thourgh "w.x.y", "w.x" and "w", but not "w.x.y.z"
  for(int i = name.find_last_of(_T('.'), length-1); i != String::npos;
  i = name.find_last_of(_T('.'), i-1))
  {
    String substr = name.substr(0, i);
    //tcout << _T("UpdateParents processing ") << substr << std::endl;

        LoggerMap::iterator it = loggers.find(substr);
    if(it != loggers.end())
    {
      parentFound = true;
      logger->parent = it->second;
      break; // no need to update the ancestors of the closest ancestor
    }
    else
    {
      ProvisionNodeMap::iterator it2 = provisionNodes.find(name);
      if (it2 != provisionNodes.end())
      {
        it2->second.push_back(logger);
      }
      else
      {
        //tcout << _T("Inserting ProvisionNode for ") << substr << std::endl;
        ProvisionNode node(logger);
        provisionNodes.insert(
          ProvisionNodeMap::value_type(substr, node));
      }
    }
  }

  // If we could not find any existing parents, then link with root.
  if(!parentFound)
  {
    logger->parent = root;
  }
}

void Hierarchy::updateChildren(ProvisionNode& pn, LoggerPtr& logger)
{
  //tcout << _T("updateChildren called for ") << logger->name << std::endl;

  ProvisionNode::iterator it, itEnd = pn.end();
  
  for(it = pn.begin(); it != itEnd; it++)
  {
    LoggerPtr& l = *it;
    //tcout << _T("Updating child ") << l->name << std::endl;
    
    // Unless this child already points to a correct (lower) parent,
    // make cat.parent point to l.parent and l.parent to cat.
    if(!startsWith(l->parent->name, logger->name))
    {
      logger->parent = l->parent;
      l->parent = logger;
    }
  }
}
