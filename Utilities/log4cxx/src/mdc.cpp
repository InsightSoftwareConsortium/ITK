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
 
#include <log4cxx/mdc.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

helpers::ThreadSpecificData MDC::threadSpecificData;

MDC::MDC(const String& key, const String& value) : key(key)
{
  put(key, value);
}

MDC::~MDC()
{
  remove(key);
}

MDC::Map * MDC::getCurrentThreadMap()
{
  return (MDC::Map *)threadSpecificData.GetData();
}

void MDC::setCurrentThreadMap(MDC::Map * map)
{
  threadSpecificData.SetData((void *)map);
}

void MDC::put(const String& key, const String& value)
{
  Map * map = getCurrentThreadMap();

  if (map == 0)
  {
    map = new Map;
    setCurrentThreadMap(map);
  }

  (*map)[key] = value;
}

String MDC::get(const String& key)
{
  Map::iterator it;
  Map * map = getCurrentThreadMap();

  if (map != 0)
  {
    Map::iterator it = map->find(key);
    if (it == map->end())
    {
      return String();
    }
    else
    {
      return it->second;
    }
  }
  else
  {
    return String();
  }

}

String MDC::remove(const String& key)
{
  String value;
  Map::iterator it;
  Map * map = getCurrentThreadMap();
  if (map != 0 && (it = map->find(key)) != map->end())
  {
    value = it->second;
    map->erase(it);
  }

  return value;
}

void MDC::clear()
{
  Map * map = getCurrentThreadMap();
  if(map != 0)
  {
    delete map;
    setCurrentThreadMap(0);
  }
}

const MDC::Map MDC::getContext()
{
  Map * map = getCurrentThreadMap();
  if(map != 0)
  {
    return *map;
  }
  else
  {
    return Map();
  }
}

void MDC::setContext(Map& map)
{
  Map * currentMap = getCurrentThreadMap();

  if (currentMap == 0)
  {
    currentMap = new Map;
    setCurrentThreadMap(currentMap);
  }
  
  *currentMap = map;
}
