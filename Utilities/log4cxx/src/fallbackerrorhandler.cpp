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
#include <log4cxx/varia/fallbackerrorhandler.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/spi/loggingevent.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;
using namespace log4cxx::varia;

IMPLEMENT_LOG4CXX_OBJECT(FallbackErrorHandler)

FallbackErrorHandler::FallbackErrorHandler()
{
}

void FallbackErrorHandler::setLogger(const LoggerPtr& logger)
{
  LogLog::debug(_T("FB: Adding logger [") + logger->getName() + _T("]."));
  loggers.push_back(logger);
}

void FallbackErrorHandler::error(const String& message, helpers::Exception& e,
  int errorCode) const
{ 
  error(message, e, errorCode, 0);
}

void FallbackErrorHandler::error(const String& message, helpers::Exception& e,
  int errorCode, const spi::LoggingEventPtr& event) const
{
  LogLog::debug(_T("FB: The following error reported: ") + message, e);
  LogLog::debug(_T("FB: INITIATING FALLBACK PROCEDURE."));
  for(size_t i = 0; i < loggers.size(); i++)
  {
    LoggerPtr& l = (LoggerPtr&)loggers.at(i);
    LogLog::debug(_T("FB: Searching for [")+primary->getName()+_T("] in logger [")
      +l->getName() + _T("]."));
    LogLog::debug(_T("FB: Replacing [")+primary->getName()+_T("] by [")
      + backup->getName() + _T("] in logger [")+ l->getName() +_T("]."));
    l->removeAppender(primary);
    LogLog::debug(_T("FB: Adding appender [")+backup->getName()+_T("] to logger ")
      +  l->getName());
    l->addAppender(backup);
  }    
}

void FallbackErrorHandler::setAppender(const AppenderPtr& primary)
{
  LogLog::debug(_T("FB: Setting primary appender to [") + 
    primary->getName() + _T("]."));
  this->primary = primary;
}

void FallbackErrorHandler::setBackupAppender(const AppenderPtr& backup)
{
  LogLog::debug(_T("FB: Setting backup appender to [") +
    backup->getName() + _T("]."));
  this->backup = backup;
}

void FallbackErrorHandler::activateOptions()
{
}

void FallbackErrorHandler::setOption(const String& name, const String& value)
{
}

