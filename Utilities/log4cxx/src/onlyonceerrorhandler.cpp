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
 
#include <log4cxx/appender.h>
#include <log4cxx/logger.h>
#include <log4cxx/helpers/onlyonceerrorhandler.h>
#include <log4cxx/helpers/loglog.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(OnlyOnceErrorHandler)

OnlyOnceErrorHandler::OnlyOnceErrorHandler() :
 WARN_PREFIX(_T("log4cxx warning: ")),
ERROR_PREFIX(_T("log4cxx error: ")), firstTime(true)
{
}


void OnlyOnceErrorHandler::setLogger(const LoggerPtr& logger)
{
}

void OnlyOnceErrorHandler::activateOptions()
{
}

void OnlyOnceErrorHandler::setOption(const String& name, const String& value)
{
}

void OnlyOnceErrorHandler::error(const String& message, log4cxx::helpers::Exception& e,
  int errorCode) const
{
  if(firstTime)
  {
    LogLog::error(message, e);
    firstTime = false;
  }
}

void OnlyOnceErrorHandler::error(const String& message, log4cxx::helpers::Exception& e,
  int errorCode, const log4cxx::spi::LoggingEventPtr& event) const
{
  error(message, e, errorCode);
}


void OnlyOnceErrorHandler::error(const String& message) const
{
  if(firstTime)
  {
    LogLog::error(message);
    firstTime = false;
  }
}


void OnlyOnceErrorHandler::setAppender(const AppenderPtr& appender)
{
}


void OnlyOnceErrorHandler::setBackupAppender(const AppenderPtr& appender)
{
}
