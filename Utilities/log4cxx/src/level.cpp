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
 
#include <log4cxx/level.h>
#include <log4cxx/helpers/stringhelper.h>
 
using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT_WITH_CUSTOM_CLASS(Level, LevelClass)

const LevelPtr Level::OFF(new Level(Level::OFF_INT, _T("OFF"), 0));
const LevelPtr Level::FATAL(new Level(Level::FATAL_INT, _T("FATAL"), 0));
const LevelPtr Level::ERROR(new Level(Level::ERROR_INT, _T("ERROR"), 3));
const LevelPtr Level::WARN(new Level(Level::WARN_INT, _T("WARN"),  4));
const LevelPtr Level::INFO(new Level(Level::INFO_INT, _T("INFO"),  6));
const LevelPtr Level::DEBUG(new Level(Level::DEBUG_INT, _T("DEBUG"), 7));
const LevelPtr Level::ALL(new Level(Level::ALL_INT, _T("ALL"), 7));

Level::Level(int level, const String& levelStr, int syslogEquivalent)
: level(level), levelStr(levelStr), syslogEquivalent(syslogEquivalent)
{
}


const LevelPtr& Level::toLevel(const String& sArg)
{
    return toLevel(sArg, Level::DEBUG);
}

const LevelPtr& Level::toLevel(int val)
{
    return toLevel(val, Level::DEBUG);
}

const LevelPtr& Level::toLevel(int val, const LevelPtr& defaultLevel)
{
    switch(val)
    {
    case ALL_INT: return ALL;
    case DEBUG_INT: return DEBUG;
    case INFO_INT: return INFO;
    case WARN_INT: return WARN;
    case ERROR_INT: return ERROR;
    case FATAL_INT: return FATAL;
    case OFF_INT: return OFF;
    default: return defaultLevel;
    }
}

const LevelPtr& Level::toLevel(const String& sArg, const LevelPtr& defaultLevel)
{
    if (sArg.empty())
    {
       return defaultLevel;
    }

    String s = StringHelper::toUpperCase(sArg);

    if(s == (_T("ALL"))) return ALL;
    if(s == (_T("DEBUG"))) return DEBUG;
    if(s == (_T("INFO"))) return INFO;
    if(s == (_T("WARN")))  return WARN;
    if(s == (_T("ERROR"))) return ERROR;
    if(s == (_T("FATAL"))) return FATAL;
    if(s == (_T("OFF"))) return OFF;
    
    return defaultLevel;
}

bool Level::equals(const LevelPtr& level) const
{
  return (this->level == level->level);
}

int Level::getSyslogEquivalent() const
{
  return syslogEquivalent;
}

bool Level::isGreaterOrEqual(const LevelPtr& level) const
{
    return this->level >= level->level;
}

const String& Level::toString() const
{
  return levelStr;
}

int Level::toInt() const
{
  return level;
}



