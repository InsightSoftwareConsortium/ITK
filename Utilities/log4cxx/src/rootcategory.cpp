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
 
#include <log4cxx/spi/rootcategory.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/level.h>
#include <log4cxx/appender.h>

using namespace log4cxx;
using namespace log4cxx::spi;
using namespace log4cxx::helpers;

RootCategory::RootCategory(const LevelPtr& level) : Logger(_T("root"))
{
  setLevel(level);
}

const LevelPtr& RootCategory::getEffectiveLevel()
{
  return level;
}

void RootCategory::setLevel(const LevelPtr& level)
{
  if(level == 0)
  {
    LogLog::error(_T("You have tried to set a null level to root."));
  }
  else
  {

    this->level = level;
  }
}



