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
 
#include <log4cxx/helpers/loglog.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

bool LogLog::debugEnabled = false;  
bool LogLog::quietMode = false;
String LogLog::DEBUG_KEY = _T("log4j.debug");

void LogLog::setInternalDebugging(bool debugEnabled)
{
  LogLog::debugEnabled = debugEnabled;
}

void LogLog::debug(const String& msg)
{
  if(debugEnabled && !quietMode)
  {
    tcout << msg << std::endl;
  }
}

void LogLog::debug(const String& msg, Exception& e)
{
  debug(msg);
  tcerr << e.getMessage() << std::endl;
}


void LogLog::error(const String& msg)
{
  if(quietMode)
    return;

  tcerr << msg << std::endl;
}  

void LogLog::error(const String& msg, Exception& e)
{
  error(msg);
  tcerr << e.getMessage() << std::endl;
}

void LogLog::setQuietMode(bool quietMode) 
{
  LogLog::quietMode = quietMode;
}

void LogLog::warn(const String& msg) 
{
  if(quietMode)
    return;
  
  tcerr << msg << std::endl;
}

void LogLog::warn(const String& msg, Exception& e)
{
  warn(msg);
  tcerr << e.getMessage() << std::endl;
}
 
